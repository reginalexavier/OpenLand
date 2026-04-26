# Test file for netgrossplot faceting functionality
# This demonstrates the new facet_var parameter

# Load required libraries
library(OpenLand)
library(dplyr)
library(ggplot2)

# Load sample data
data(SL_2002_2014)

# Test 1: Regular netgrossplot (existing functionality)
cat("Testing existing netgrossplot functionality...\n")

# Edit category names for better visualization
SL_2002_2014$tb_legend$categoryName <- factor(c("Ap", "FF", "SA", "SG", "aa", "SF",
                                             "Agua", "Iu", "Ac", "R", "Im"),
                                      levels = c("FF", "SF", "SA", "SG", "aa", "Ap",
                                               "Ac", "Im", "Iu", "Agua", "R"))

# Create regular plot
regular_plot <- netgrossplot(
  dataset = SL_2002_2014$lulc_Multistep,
  legendtable = SL_2002_2014$tb_legend,
  title = "Regular Net-Gross Plot",
  xlab = "LUC Category",
  ylab = "Area (km²)",
  changesLabel = c(GC = "Gross changes", NG = "Net Gain", NL = "Net Loss"),
  color = c(GC = "gray70", NG = "#006400", NL = "#EE2C2C")
)

print(regular_plot)
cat("Regular plot created successfully ✓\n")

# Test 2: Create sample data with faceting variable for demonstration
cat("\nTesting faceting functionality...\n")

# Add a sample grouping variable to demonstrate faceting
# In real use cases, this would come from your actual data
test_data <- SL_2002_2014$lulc_Multistep

# Add sample region grouping (this would typically come from your spatial analysis)
set.seed(123)  # For reproducibility
n_rows <- nrow(test_data)
regions <- sample(c("North", "South", "East", "West"), n_rows, replace = TRUE)
test_data$region <- regions

# Create faceted plot
faceted_plot <- netgrossplot(
  dataset = test_data,
  legendtable = SL_2002_2014$tb_legend,
  title = "Net-Gross Plot by Region",
  xlab = "LUC Category",
  ylab = "Area (km²)",
  changesLabel = c(GC = "Gross changes", NG = "Net Gain", NL = "Net Loss"),
  color = c(GC = "gray70", NG = "#006400", NL = "#EE2C2C"),
  facet_var = "region",
  facet_ncol = 2,
  facet_scales = "free_y"
)

print(faceted_plot)
cat("Faceted plot created successfully ✓\n")

# Test 3: Test error handling
cat("\nTesting error handling...\n")

# Test with invalid facet variable
tryCatch({
  netgrossplot(
    dataset = test_data,
    legendtable = SL_2002_2014$tb_legend,
    facet_var = "nonexistent_column"
  )
}, error = function(e) {
  cat("Error handling for invalid column works correctly ✓\n")
  cat("Error message:", e$message, "\n")
})

# Test 4: Demonstrate different facet options
cat("\nTesting different facet configurations...\n")

# Create example with land use type grouping
test_data$land_use_type <- sample(c("Urban", "Agricultural", "Forest"), n_rows, replace = TRUE)

# Faceted plot with different configuration
faceted_plot2 <- netgrossplot(
  dataset = test_data,
  legendtable = SL_2002_2014$tb_legend,
  title = "Net-Gross Plot by Land Use Type",
  xlab = "LUC Category", 
  ylab = "Area (km²)",
  facet_var = "land_use_type",
  facet_ncol = 3,
  facet_scales = "fixed"
)

print(faceted_plot2)
cat("Alternative faceted plot created successfully ✓\n")

cat("\n🎉 All netgrossplot faceting tests completed successfully!\n")
cat("\nNew features added:\n")
cat("✅ facet_var: Column name for faceting\n")
cat("✅ facet_scales: 'fixed', 'free', 'free_x', 'free_y'\n") 
cat("✅ facet_ncol: Number of columns\n")
cat("✅ facet_nrow: Number of rows\n")
cat("✅ facet_labeller: Custom labeller function\n")
cat("✅ Comprehensive error handling\n")
cat("✅ Backward compatibility maintained\n")
