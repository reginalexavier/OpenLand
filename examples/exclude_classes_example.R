# Exemplo prático: Usando exclude_classes no contingencyTable
# 
# Este exemplo demonstra como usar o novo parâmetro exclude_classes 
# para remover classes indesejadas da análise de mudança de uso da terra.

library(OpenLand)
library(raster)

# Carregar dados de exemplo (se disponível)
# url <- "https://zenodo.org/record/3685230/files/SaoLourencoBasin.rda?download=1"
# temp <- tempfile()
# download.file(url, temp, mode = "wb")
# load(temp)

# Exemplo 1: Excluindo classe 0 (fundo/sem dados)
# ================================================

# Usar dados de exemplo ou seus próprios rasters
# result_with_zero <- contingencyTable(SaoLourencoBasin, pixelresolution = 30)
# print("Classes presentes COM a classe 0:")
# print(sort(unique(result_with_zero$tb_legend$categoryValue)))

# Agora excluindo a classe 0
# result_no_zero <- contingencyTable(SaoLourencoBasin, 
#                                   pixelresolution = 30, 
#                                   exclude_classes = 0)
# print("Classes presentes SEM a classe 0:")
# print(sort(unique(result_no_zero$tb_legend$categoryValue)))

# Verificar quais classes foram excluídas
# excluded <- attr(result_no_zero$tb_legend, "excluded_classes")
# print(paste("Classes excluídas:", paste(excluded, collapse = ", ")))

# Exemplo 2: Excluindo múltiplas classes
# ======================================

# Excluir classes 0 (fundo) e 255 (sem dados)
# result_multiple_exclusions <- contingencyTable(SaoLourencoBasin,
#                                               pixelresolution = 30,
#                                               exclude_classes = c(0, 255))

# Exemplo 3: Comparando resultados
# ================================

# compare_results <- function(original, filtered) {
#   cat("Análise original:\n")
#   cat("- Número de transições:", nrow(original$lulc_Multistep), "\n")
#   cat("- Número de classes:", nrow(original$tb_legend), "\n")
#   cat("- Área total (km²):", round(original$totalArea$area_km2, 2), "\n\n")
#   
#   cat("Análise filtrada:\n")
#   cat("- Número de transições:", nrow(filtered$lulc_Multistep), "\n")
#   cat("- Número de classes:", nrow(filtered$tb_legend), "\n")
#   cat("- Área total (km²):", round(filtered$totalArea$area_km2, 2), "\n")
#   cat("- Classes excluídas:", paste(attr(filtered$tb_legend, "excluded_classes"), collapse = ", "), "\n")
# }

# compare_results(result_with_zero, result_no_zero)

# Exemplo 4: Uso prático com workflow completo
# ============================================

# practical_workflow <- function(input_raster, pixel_res = 30, exclude_vals = 0) {
#   
#   # Análise com exclusões
#   result <- contingencyTable(input_raster, 
#                             pixelresolution = pixel_res,
#                             exclude_classes = exclude_vals)
#   
#   # Personalizar nomes das classes (exemplo)
#   if (nrow(result$tb_legend) >= 3) {
#     result$tb_legend$categoryName <- factor(
#       c("Floresta", "Agricultura", "Urbano")[1:nrow(result$tb_legend)],
#       levels = c("Floresta", "Agricultura", "Urbano")[1:nrow(result$tb_legend)]
#     )
#     
#     result$tb_legend$color <- c("#228B22", "#FFD700", "#DC143C")[1:nrow(result$tb_legend)]
#   }
#   
#   return(result)
# }

# Executar workflow
# final_result <- practical_workflow(SaoLourencoBasin, exclude_vals = 0)

# Validação: Verificar se a exclusão funcionou
# =============================================

validate_exclusions <- function(result, excluded_classes) {
  # Verificar se as classes excluídas não aparecem nos resultados
  from_classes <- unique(result$lulc_Multistep$From)
  to_classes <- unique(result$lulc_Multistep$To)
  legend_classes <- result$tb_legend$categoryValue
  
  excluded_found_from <- any(excluded_classes %in% from_classes)
  excluded_found_to <- any(excluded_classes %in% to_classes)
  excluded_found_legend <- any(excluded_classes %in% legend_classes)
  
  if (excluded_found_from || excluded_found_to || excluded_found_legend) {
    warning("Classes excluídas ainda aparecem nos resultados!")
    return(FALSE)
  } else {
    message("✓ Exclusão de classes funcionou corretamente")
    return(TRUE)
  }
}

# Exemplo de validação
# validate_exclusions(result_no_zero, 0)

cat("Exemplo de uso do parâmetro exclude_classes:\n")
cat("contingencyTable(input_raster, exclude_classes = 0)        # Excluir classe 0\n")
cat("contingencyTable(input_raster, exclude_classes = c(0,255)) # Excluir múltiplas classes\n")
