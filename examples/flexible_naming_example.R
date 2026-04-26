# Exemplo de Uso - Nomenclatura Flexível

## Este arquivo demonstra como usar as novas funcionalidades de nomenclatura flexível

library(OpenLand)

# Função auxiliar para criar rasters de demonstração com nomes personalizados
create_demo_with_names <- function(years, name_format = "default") {
  rasters <- .demo_landscape(year = years, res = 1)
  
  # Aplicar diferentes formatos de nome
  if (name_format == "dash") {
    names(rasters) <- paste0("landscape-", years)
  } else if (name_format == "year_first") {
    names(rasters) <- paste0(years, "_landscape_data")
  } else if (name_format == "complex") {
    names(rasters) <- paste0("study_year", years, "_final")
  } else {
    # default: manter nomes originais no formato landscape_YEAR
    names(rasters) <- paste0("landscape_", years)
  }
  
  return(rasters)
}

# Exemplo 1: Formato padrão (funciona como antes)
cat("\n=== Exemplo 1: Formato Padrão ===\n")
cat("Nomes dos rasters: landscape_2020, landscape_2021, landscape_2022\n")

rasters_default <- create_demo_with_names(2020:2022, "default")
cat("Nomes reais:", names(rasters_default), "\n")

# resultado_default <- contingencyTable(rasters_default, pixelresolution = 1)

# Exemplo 2: Usando hífen como separador  
cat("\n=== Exemplo 2: Hífen como Separador ===\n")
cat("Nomes dos rasters: landscape-2020, landscape-2021, landscape-2022\n")

rasters_dash <- create_demo_with_names(2020:2022, "dash")
cat("Nomes reais:", names(rasters_dash), "\n")

# resultado_dash <- contingencyTable(rasters_dash, 
#                                   pixelresolution = 1,
#                                   name_separator = "-")

# Exemplo 3: Ano no início
cat("\n=== Exemplo 3: Ano no Início ===\n")
cat("Nomes dos rasters: 2020_landscape_data, 2021_landscape_data, 2022_landscape_data\n")

rasters_year_first <- create_demo_with_names(2020:2022, "year_first")
cat("Nomes reais:", names(rasters_year_first), "\n")

# resultado_year_first <- contingencyTable(rasters_year_first,
#                                         pixelresolution = 1,
#                                         year_position = "first")

# Exemplo 4: Padrão personalizado
cat("\n=== Exemplo 4: Padrão Personalizado ===\n")
cat("Nomes dos rasters: study_year2020_final, study_year2021_final, study_year2022_final\n")

rasters_complex <- create_demo_with_names(2020:2022, "complex")
cat("Nomes reais:", names(rasters_complex), "\n")

# resultado_complex <- contingencyTable(rasters_complex,
#                                      pixelresolution = 1,
#                                      name_pattern = "\\d{4}")

# Teste da função de extração (se disponível)
if (exists("extract_year_from_name")) {
  cat("\n=== Testes da Função de Extração ===\n")
  
  # Teste separador padrão
  cat("Teste 1 - landscape_2020:", extract_year_from_name("landscape_2020"), "\n")
  
  # Teste separador hífen
  cat("Teste 2 - landscape-2020:", extract_year_from_name("landscape-2020", separator = "-"), "\n")
  
  # Teste ano no início
  cat("Teste 3 - 2020_landscape:", extract_year_from_name("2020_landscape", position = "first"), "\n")
  
  # Teste padrão personalizado
  cat("Teste 4 - study_year2020_final:", extract_year_from_name("study_year2020_final", pattern = "\\d{4}"), "\n")
}

cat("\n=== Resumo ===\n")
cat("As novas funcionalidades permitem:\n")
cat("1. Diferentes separadores (_, -, ., etc.)\n")
cat("2. Ano em diferentes posições (início, fim, meio)\n")
cat("3. Padrões personalizados com regex\n")
cat("4. Compatibilidade total com código existente\n")
