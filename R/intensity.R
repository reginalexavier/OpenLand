#' @include intensityClass.R
NULL

#' intensityanalysis
#' It's a function that perform the intensity test analysis from a table contingency
#'
#' @param dataset The list of object from \code{contingencyTable}
#' @param class_n A natural class of land use
#' @param class_m A antropic class of land use
#' @return Intensity objects including the levels tables and strationarity test tables
#' @export
#'
#' @examples

intensityanalysis <- function(dataset, class_n, class_m) {
  # seting the data
  AE <- dataset[[3]][[1, 2]] #study area

  allinterval <-
    dataset[[length(dataset)]] #whole interval in year

  lulc <-
    dplyr::left_join(dataset[[1]], dataset[[2]][, c(1, 2)], by = c("From" = "classValue")) %>%
    dplyr::left_join(dataset[[2]][, c(1, 2)], by = c("To" = "classValue")) %>% dplyr::select(-c(From, To)) %>%
    dplyr::rename("From" = "className.x", "To" = "className.y") %>%
    dplyr::select(Period, From, To, km2, interval, QtPixel)


  lulc$Period <-
    factor(as.factor(lulc$Period), levels = rev(levels(as.factor(lulc$Period)))) #turning Period a factor

  class_fillColor <- dataset[[2]][c(2, 3)]

  lookupcolor <- class_fillColor$color

  names(lookupcolor) <- class_fillColor$className

  #____________Interval-------
  #EQ1 - St ----

  eq1 <- lulc %>% dplyr::filter(From != To) %>%
    dplyr::group_by(Period, interval) %>% dplyr::summarise(intch_km2 = sum(km2)) %>% #interval change:intch_km2
    dplyr::mutate(STt = (intch_km2 / (interval * AE)) * 100) %>%
    dplyr::select(1, 3, 4)

  #EQ2 - U ----
  eq2 <- lulc %>% dplyr::filter(From != To) %>%
    dplyr::summarise(num02 = sum(km2)) %>% #area mudada em todos os Periodos somados
    dplyr::mutate(U = (num02 / (allinterval * AE)) * 100)

  level01 <-
    eq1 %>% dplyr::mutate(U = eq2[[2]], tipo = if_else(STt > U, "Faster", "Slow"))

  #____________Categorical ----
  #EQ3 - Gtj ----
  num03 <- lulc %>% dplyr::filter(From != To) %>%
    dplyr::group_by(Period, To, interval) %>% dplyr::summarise(num03 = sum(km2)) #ganho bruto da classe no tempo Yt+1

  denom03 <-
    lulc %>% dplyr::group_by(Period, To) %>% dplyr::summarise(denom03 = sum(km2)) #area total da classe no tempo Yt+1

  eq3 <- num03 %>% dplyr::left_join(denom03, by = c("Period", "To")) %>%
    dplyr::mutate(Gtj = (num03 / (denom03 * interval)) * 100) %>% dplyr::left_join(eq1, by = "Period")

  #EQ4 -   Lti ---------
  num04 <- lulc %>% dplyr::filter(From != To) %>%
    dplyr::group_by(Period, From, interval) %>% dplyr::summarise(num04 = sum(km2)) #perda bruto da classe i no tempo Yt

  denom04 <-
    lulc %>% dplyr::group_by(Period, From) %>% dplyr::summarise(denom04 = sum(km2)) #area total da classe no tempo Yt

  eq4 <-
    num04 %>% dplyr::left_join(denom04, by = c("Period", "From")) %>%
    dplyr::mutate(Lti = (num04 / (denom04 * interval)) * 100) %>% dplyr::left_join(eq1, by = "Period")

  #____________Transition ----
  #meu n é agropecuario como a classe que ganhou mais durante esse perriodo
  #Witch transitions are particularly intensive in a given time interval?
  #EQ5 - Rtin----
  num05 <-
    lulc %>% dplyr::filter(From != To, To == class_n) %>% #no caso tudo que vai para a pastagem
    dplyr::group_by(Period, From, interval) %>% dplyr::summarise(num05 = sum(km2)) #area de transição de cada i para n

  denom05 <-
    lulc %>% dplyr::filter(From != class_n) %>%  #filtro de todas as outras classes que nao sao da classe n
    dplyr::group_by(Period, From) %>% dplyr::summarise(denom05 = sum(km2)) #area total de cada classe i no tempo Yt

  eq5 <-
    num05 %>% dplyr::left_join(denom05, by = c("Period", "From")) %>%
    dplyr::mutate(Rtin = (num05 / (interval * denom05)) * 100)

  #EQ6 - Wtn ----
  num06 <- lulc %>% dplyr::filter(From != To, To == class_n) %>%
    dplyr::group_by(Period, To, interval) %>% dplyr::summarise(num06 = sum(km2)) #ganho bruto da classe n durante a transição

  denom06 <- lulc  %>% dplyr::filter(From != class_n) %>%
    dplyr::group_by(Period) %>% dplyr::summarise(denom06 = sum(km2)) #Area que não é da classe n no tempo Yt ????

  eq6 <- num06 %>% dplyr::left_join(denom06, by = "Period") %>%
    dplyr::mutate(Wtn = (num06 / (interval * denom06)) * 100)

  plot03ganho_n <-
    eq5 %>% dplyr::left_join(eq6, by = c("Period", "interval"))

  #EQ7 - Qtmj----
  num07 <- lulc %>% dplyr::filter(From != To, From == class_m) %>%
    dplyr::group_by(Period, To, interval) %>% dplyr::summarise(num07 = sum(km2)) #area de transição de m para cada j

  denom07 <- lulc %>% dplyr::filter(To != class_m) %>%
    dplyr::group_by(Period, To) %>% dplyr::summarise(denom07 = sum(km2)) #area total de cada classe j no tempo (Yt+1)

  eq7 <- num07 %>% dplyr::left_join(denom07, by = c("Period", "To")) %>%
    dplyr::mutate(Qtmj = (num07 / (interval * denom07)) * 100)

  #EQ8 - Vtm----
  num08 <- lulc %>% dplyr::filter(From != To, From == class_m) %>%
    dplyr::group_by(Period, From, interval) %>% dplyr::summarise(num08 = sum(km2)) #perda bruta da classe m durante a transição

  denom08 <- lulc %>% dplyr::filter(To != class_m) %>%
    dplyr::group_by(Period) %>% dplyr::summarise(denom08 = sum(km2)) #Area que não é da classe m no tempo Y(t+1)


  eq8 <- num08 %>% dplyr::left_join(denom08, by = "Period") %>%
    dplyr::mutate(Vtm = (num08 / (interval * denom08)) * 100)

  plot03perda_m <-
    eq7 %>% dplyr::left_join(eq8, by = c("Period", "interval"))

  #___Stationarity test------

  #Level02 ----
  #gain
  st_lv2_gain <- eq3 %>% dplyr::filter(Gtj > STt) %>% dplyr::group_by(To) %>%
    dplyr::summarise(
      gain = dplyr::n(),
      N = length(unique(eq3$Period)),
      Stationarity = "Active Gain",
      Test = ifelse(gain == N, "Y", "N")
    ) %>% rbind(
      eq3 %>% dplyr::filter(Gtj < STt) %>% dplyr::group_by(To) %>%
        dplyr::summarise(
          gain = dplyr::n(),
          N = length(unique(eq3$Period)),
          Stationarity = "Dormant Gain",
          Test = ifelse(gain == N, "Y", "N")
        )
    )

  #loss
  st_lv2_loss <- eq4 %>% dplyr::filter(Lti > STt) %>% dplyr::group_by(From) %>%
    dplyr::summarise(
      loss = dplyr::n(),
      N = length(unique(eq4$Period)),
      Stationarity = "Active Loss",
      Test = ifelse(loss == N, "Y", "N")
    ) %>% rbind(
      eq4 %>% dplyr::filter(Lti < STt) %>% dplyr::group_by(From) %>%
        dplyr::summarise(
          loss = dplyr::n(),
          N = length(unique(eq4$Period)),
          Stationarity = "Dormant Loss",
          Test = ifelse(loss == N, "Y", "N")
        )
    )

  #Level03 gain_n ----
  st_gain_n <-
    plot03ganho_n %>% dplyr::filter(Rtin > Wtn) %>% dplyr::group_by(From) %>%
    dplyr::summarise(
      loss = dplyr::n(),
      N = length(unique(plot03ganho_n$Period)),
      Stationarity = paste("targeted by", class_n),
      Test = ifelse(loss == N, "Y", "N")
    ) %>% rbind(
      plot03ganho_n %>% dplyr::filter(Rtin < Wtn) %>% dplyr::group_by(From) %>%
        dplyr::summarise(
          loss = dplyr::n(),
          N = length(unique(plot03ganho_n$Period)),
          Stationarity = paste("avoided by", class_n),
          Test = ifelse(loss == N, "Y", "N")
        )
    )

  #Level03b loss_m----
  st_loss_m <-
    plot03perda_m %>% dplyr::filter(Qtmj > Vtm) %>% dplyr::group_by(To) %>%
    dplyr::summarise(
      gain = dplyr::n(),
      N = length(unique(plot03perda_m$Period)),
      Stationarity = paste("targeted", class_m),
      Test = ifelse(gain == N, "Y", "N")
    ) %>% rbind(
      plot03perda_m %>% dplyr::filter(Qtmj < Vtm) %>% dplyr::group_by(To) %>%
        dplyr::summarise(
          gain = dplyr::n(),
          N = length(unique(plot03perda_m$Period)),
          Stationarity = paste("avoided", class_m),
          Test = ifelse(gain == N, "Y", "N")
        )
    )

  intensity_tables <-
    list(
      lulc_table = lulc,
      lv1_tbl = intensity(level01),
      lv2_gain = intensity(eq3),
      lv2_loss = intensity(eq4),
      gain_n = intensity(plot03ganho_n),
      loss_m = intensity(plot03perda_m),
      st_lv2_gain = st_lv2_gain,
      st_lv2_loss = st_lv2_loss,
      st_gain_n = st_gain_n,
      st_loss_m = st_loss_m
    )

}
