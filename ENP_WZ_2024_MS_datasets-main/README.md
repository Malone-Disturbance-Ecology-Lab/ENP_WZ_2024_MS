# ENP_WZ_2024_MS_datasets
This repository includes the data management scripts that created the data used in ENP_WZ_2024_MS repository.

The following scripts produce the following datasets for use in subsequent analyses:
 1. longterm_dataframe.R:
     1. Produces AR_long_term_monthly.csv, which includes monthly mean water level and salinity starting in 1993 for four sites in the southeast Everglades: NPEV8, EVER (two monitoring sites managed by ENP and maintained in DBHYDRO), TS/Ph1, and TS/Ph7 (two monitoring sites associated with the Florida Coastal Everglades LTER (FCE-LTER).
     2. This dataset is used in flow.trend in ENP_WZ_2024_MS.

 2. water_salinity_dataframe.R:
     1. Produces AR_wl_sal_2021_weekly.csv, which includes weekly mean water level and salinity in 2021 for three sites in the southeast Everglades: TS/Ph1, a freshwater marl prairie site; SE1, an ecotone site; and TS/Ph7, a scrub mangrove forest site.
     2. This dataset is used in flow.gam in ENP_WZ_2024_MS.
 
 3. flux_wl_dataframe.R 
     1. Produces AR_flux_sites_2021.csv, which includes half-hourly flux (NEE CO2) and meterological (PAR and air Temperature) data and water level indicator data for three sites in the southeast Everglades: TS/Ph1, a freshwater marl prairie site; SE1, an ecotone site; and TS/Ph7, a scrub mangrove forest site.
     2. This dataset is used in flow.NLM in ENP_WZ_2024_MS.
