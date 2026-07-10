# Quick introduction to the OpenLand package

------------------------------------------------------------------------

**This is a Vignette on how to use the OpenLand package for exploratory
analysis of Land Use and Cover (LUC) time series.**

## Description of the tool

OpenLand is an open-source R package for the analysis of land use and
cover (LUC) time series. It includes support for consistency check and
loading spatiotemporal raster data and synthesized spatial plotting.
Several LUC change (LUCC) metrics in regular or irregular time intervals
can be extracted and visualized through one- and multistep sankey and
chord diagrams. A complete intensity analysis according to (Aldwaik and
Pontius 2012) is implemented, including tools for the generation of
standardized multilevel output graphics.

## São Lourenço river Basin example dataset

The OpenLand functionality is illustrated for a LUC dataset of São
Lourenço river basin, a major Pantanal wetland contribution area as
provided by the 4^(th) edition of the Monitoring of Changes in Land
cover and Land Use in the Upper Paraguay River Basin - Brazilian
portion - Review Period: 2012 to 2014 (Embrapa Pantanal et al. 2015).
The time series is composed by five LUC maps (2002, 2008, 2010, 2012 and
2014). The study area of approximately 22,400 km² is located in the
Cerrado Savannah biom in the southeast of the Brazilian state of Mato
Grosso, which has experienced a LUCC of about 12% of its extension
during the 12-years period, including deforestation and intensification
of existing agricultural uses. For processing in the OpenLand package,
the original multi-year shape file was transformed into rasters and then
saved as a 5-layer `RasterStack` (`SaoLourencoBasin`), available from a
public repository
[(10.5281/zenodo.3685229)](https://doi.org/10.5281/zenodo.3685230) as an
`.RDA` file which can be loaded into `R`.

## The complete rendering of this document depends on the availability of the dataset in zenodo. Please try at another time.

Aldwaik, Safaa Zakaria, and Robert Gilmore Pontius. 2012. “Intensity
analysis to unify measurements of size and stationarity of land changes
by interval, category, and transition.” *Landsc. Urban Plan.* 106 (1):
103–14. <https://doi.org/10.1016/j.landurbplan.2012.02.010>.

Embrapa Pantanal, Instituto SOS Pantanal, and WWF-Brasil. 2015.
*Mapeamento da Bacia do Alto Paraguai*.
