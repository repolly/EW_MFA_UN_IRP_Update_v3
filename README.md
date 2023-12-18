# ew-mfa-update

Purpose of repository: Update of material flow accounts for Domestic Extraction of all abiotic materials on a economy-wide (i.e. national) level. These include the aggregated material groups of fossil fuels, metal ores, and non-metallic minerals.

In addition, intermediary files on the production of all kinds of abiotic materials (i.e. including processed materials) are used for comparison against mining production accounts in/from the FINEPRINT project.

The compilation makes use of various international datasets, which report data on various "states/formats" of materials, e.g. the mined amounts (i.e. ores and other minerals), the derived concentrates, or the derived processed materials/contents.

The final data output is provided in the highest detail feasible for a harmonized dataset, as well as in the so called CCC detail used for integration in the UNEP IRP Global Material Flow Database.

The compilation is conducted in accordance with standards on Material Flow Accounting (MFA) published by Eurostat, the OECD, and UNEP over the last two decades.

<br>




## Important to consider

Updates for the latest years are supposed to be conducted via this repository.

This repository is builds upon former data from repository "ew_mfa_update_v2", which compiled the whole time range. 

<br>




## General overview

The repository roughly does the following:

- Retrieval of raw data
- Formatting and cleaning
- Harmonization (i.e. materials, countries, units)
  - Including geographical adjustments of specific countries in some cases
- Integration
- Conversion (of units and materials)
- Estimation (of ores and of construction materials)
- Final cleaning and final formatting
  - Including geographical adjustments of specific countries in some cases
- Intermediary and final data checks
- Other (e.g. compilation of data for comparison with mining data, compilation of tailings accounts)

<br>




## Information on primary data sources

### Main data sources

There are six main data sources:

- WMD / BGS / USGS (for metal ores and non-metallic minerals)
- UNSD / IEA / EIA (for fossil fuels)

The above order also reflects the priority given to each data source regarding data integration. Meaning that data points for production of the same material in a given country in a given year by source 1 are given priority over the same data points by source 2, and the same for source 2 versus source 3. Exceptions exist (please see explanations on integration below).

The input data from the above primary sources is comprised in the following ways:

- WMD
  - Data retrieved online via script
    - Download of Excel file
    - For latest 5 years
  - Additional inofficial info provided by WMD team regarding quality of data points (i.e. whether reported/estimated)
    - Excel file
    - For 2015-2017
- BGS
  - Data retrieved online via script
    - Web-scraping HTML table from webpage, saved as rds-file
    - For latest ... years
  
- USGS
  - Data retrieved online via script
    - Download of individual Excel files of commodity-specific "Mineral Yearbooks"
    - For latest years not compiled in former database
  - Additional time series collected manually from country-specific “Minerals Yearbooks”
    - Own Excel file
    - For years ...

UNSD
  - Data file directly received from UNSD
    - Web-scraping did not seem feasible + No API download available
    - Request via e-mail necessary -> Leonardo Souza (energy_stat@un.org)
    - Data comes via a bulk file for all years (from 1950 onwards)
    - UNSD Energy Statistics updates are usually finished around the end of a calendar year and published at the beginning of a calendar year
    
- IEA
  - Data retrieved online, manually, from OECD iLibrary
    - Download of csv file
    - For ... years
- EIA
  - Data retrieved online via script
    - Usage of API (v1), saved as rds-file (Please note: API v2 available since 2022)
    - For ... years

<br>




### Data sources for ore estimation factors

Data sources used for the compilation of ore estimation factors (files in folder 01_input/03_factors):

- estimation_factors_snl.csv: metal-to-ore ratios compiled in repository https://github.com/fineprint-global/compilation_mining_data
- estimation_factors_new.csv: metal-to-ore ratios compiled in repository https://github.com/fineprint-global/compilation_mining_data
- estimation_factors_old.csv: Concentrations (mostly for metals) from former GRU SQL database -> allocated to current material_ids
- estimation_factors_old_2.csv: Specific concentrations applied during processing via Excel files during former compilations 
- estimation_factors_add.csv: Average factor for phosphates, derived from WMD versus BGS production data (create_additional_est_fac.R)
- estimation_factors_add_2.csv: Factors for lithium oxide, derived from WMD versus BGS production data (create_additional_est_fac.R)
- estimation_factors_add_3.csv: Factors for aluminium, derived from WMD & BGS production data (create_additional_est_fac.R)
  - Factors not applied, because aluminium production data removed earlier in order to avoid false estimates (only bauxite production is relevant)

<br>




### Data sources for estimations on construction minerals

- For limestone, Sand, gravel, clay, gypsum
  - UNSD: Production of cement
    - Manual download
  - USGS: Production of cement
    - Included in download of other USGS data
  - UN Comtrade: Imports/exports of cement + sand + gravel
    - Manual download

- For sand and gravel
  - IEA: Production/imports/exports of bitumen
    - Included in download of other IEA data

- For clay
  - UNSD: Production of bricks + Production of clay tiles
   - Manual download

<br>




## Repository structure

### 01_input

- 01_concordance_tables
  - General ID tables (e.g. for material, country, unit) -> from GRU W-drive "Datenbank\Listen"
  - Concordance tables (e.g. for harmonization of source namings/IDs to above IDs)
  - Other lists (e.g. years in which countries existed)
  - usgs_retrieval
    - lists needed for USGS Excel file formatting

- 02_log_files
  - bgs/iea/wmd
    - Files for adjustments of special cases
  - usgs_retrieval
    - log file of USGS files downloaded during last update (in order to not download or process all files)

- 03_factors
  - conversion factors for units and elemental metals
  - estimation factors for ore values (with global and regional factors being created by ore estimation script)

04_former_data
  - former_irp_database
    - Includes output data from data compilation in 2017 -> used for comparison
  - wmd
    - 6.4. acc source.xlsx: Info regarding quality of data points in 2015-2017 (i.e. whether reported/estimated)

<br>



### 02_scripts

For explanations on exact process of scripts, see "Process" below

Simple overview:

- 00_functions
  - geographic_adjust.R
  - lastfile.R
  - archive

- 01_data_retrieval
  - retrieve_bgs.R
  - retrieve_eia.R
  - retrieve_usgs.R
  - retrieve_wmd.R
  - usgs_manual_extension.R

- 02_data_harmonization
  - clean_bgs.R
  - clean_eia.R
  - clean_harmonize_usgs_extensions.R
  - clean_usgs.R
  - clean_wmd.R
  - harmonize_bgs.R
  - harmonize_eia.R
  - harmonize_iea.R
  - harmonize_unsd.R
  - harmonize_usgs.R
  - harmonize_wmd.R

- 03_data_integration
  - integration_all.R
  - integration_fossil_fuels.R
  - integration_wmd_bgs_usgs.R

- 04a_data_conversion
  - conversion_elements.R
  - conversion_units.R

- 04b_data_estimation
  - estimation_construction_min.R
  - estimation_ores_smooth.R
  - archive

- 05_data_check
  - checks_rendering.R
  - check_large_changes.Rmd
  - ccc_comparison_now_former.Rmd
  - final_checks.R
  - comparison_CCC_2017vs2021.R
  - other
    - bgs_vs_estimates.Rmd
    - archive
  - archive

- 06_final_formatting
  - final_cleaning.R
  - final_formatting.R

- 07_other
  - 01_data_for_comparison
    - all_materials_v2.R
  - 02_data_requests
  - 03_other
    - create_additional_est_fac.R
    - create_approximate_tailings_accounts.R
    - prepare_old_factors.R

<br>



### 03_intermediate

- 01_data_retrieval
  - bgs
  - constr_min
  - eia
  - iea
  - unsd
  - usgs
  - wmd
- 02_data_harmonization
- 03_data_integration
- 04a_data_conversion
  - conversion_elements
  - conversion_units
- 4b_data_estimation
- 05_data_check
  - 01_intermediate_checks
  - 02_final_checks
  - 03_other
- 06_final_formatting

<br>



### 04_output

- 01_harmonization
  - 01_conc_nomatch
    - Contains all csv files for missing ID allocations during harmonization (i.e. material, country, unit IDs)

- 02_final
  - Contains the final data output comprising four different files: CCC aggregation (with and without names) + full data detail (with and without names)

- 03_comparison
  - Contains data for comparison with FINEPRINT mining accounts

- 04_requests
  - all_data
    - Contains two specific selections of intermediate data: (1) All data after integration and unit conversion, but before estimations, and (2) WMD+BGS+USGS after harmonization, but before integration (i.e. including double counting)
  - lithium

- 05_supplementary_info
  - File for all estimation factors which have been compiled/integrated
  - File for all estimation factors which have actually been applied to the data
  - Overview of data points adjusted, because they were identified as outliers

- 06_other
  - tailings
    - "Experimental" compilation/estimation of tailings

<br>




## Process

The whole process should be possible to conduct solely from script update.R.
This file sources all other necessary scripts for the complete data compilation. This includes all steps.

Apart from the main script, several output files are created by individual scripts in order to show specific outcomes of the respective processing step (e.g. harmonization or check/comparison of data). These output files have to be opened/checked individually.

Furthermore, certain input data has to be provided manually and is therefore not retrieved or adjusted by any script (see more below).

The following details of the process are not supposed to describe every single step, but rather to provide a rough overview.


### Data retrieval

#### WMD

- retrieve_wmd.R
- Simple download of Excel file from WMD webpage


#### BGS

- retrieve_bgs.R
- Scraping data from BGS webpage
  - Steps:
    - Extract BGS commodity numbers from HTML code
    - Extract available years
      - Specify groups of years (max. download: 11 years per time)
    - Download of all data, in two nested loops
      - By each year group and by each commodity
        - Get commodity name
        - Get unit
        - Combine data in tibble at each end of inner loop
        - Combine data in list at each end of outer loop

- Sulphur data
  - Is not available online in BGS data tool, can only be retrieved manually from Pdf file at https://www2.bgs.ac.uk/mineralsuk/statistics/worldArchive.html
  - However, only pyrites and sulphur ore are relevant
    - Amount to only a few data points, totalling only a few millions tons globally (which are mainly estimations)
    - Therefore has not been updated for recent years in this compilation (but has been included for earlier years through data from former database)


#### USGS

- retrieve_usgs.R
- Downloading commodity Excel files from USGS website
  - Steps:
    - Get all URLs for each USGS commodity page
      - Create a list of names based on that
    - Two nested loops for downloading files
      - Outer loop to get list of latest files for each commodity
        - Filter for only the files not downloaded yet (see log file USGS_file_list)
      - Inner loop for downloading only missing files for selected commodity
    - Save updated log file USGS_file_list and save list of new files

- usgs_manual_extension.R
- Create table for manual USGS extension
  - To collected data from country-specific mineral yearbooks
  - Has to be collected manually from Pdf/Excel files
  - Only relevant for materials where data cannot be provided by file download (in particular all kinds of stones)


#### UNSD

- Data received from UNSD as zip-file
- Data saved in ./03_intermediate/01_data_retrieval/unsd
  - UN DB Codes
  - Data as txt-file


#### IEA

- Subscription to OECD iLibrary required (via WU, i.e. only possible from WU network).
  - Unless other access to IEA data is available
- Steps for download
  - Go to [respective iLibrary page of IEA World Energy Statistics](https://www.oecd-ilibrary.org/energy/data/iea-world-energy-statistics-and-balances_enestats-data-en).
  - Access dataset "World energy statistics"
  - Open "Customize" > "Selection" >  
    - "Country": Select all
    - "Product": Select all
    - "Flow": Select Production/Imports/Exports.
    - "Time": Select last 10 years
  - Open "Customize" > "Table options" (if necessary)
  - Open "Export" > "Text file (CSV)"  
    - Select "default format" and download
- Data saved in ./03_intermediate/01_data_retrieval/iea
  - Keeping default file name (i.e. format "WBES_DDMMYYYYhhmm.....")


#### EIA

- retrieve_eia.R
- Retrieval of EIA data
  - Via API of EIA (please note: key needed -> see .Renviron file)
  - API documentation: https://www.eia.gov/opendata/commands.php
  - PLEASE NOTE: EIA has released a new API version in 2022
  - Steps:
    - Make sure EIA commodity/category IDs are still valid
    - Two nested loops for download
      - Outer loop to retrieve series identifiers for category/material and filter for required series_ids
        - Remove series_ids which cause double counting
      - Inner loop to retrieve data for each series_id
        - Combine data at end of loop


#### Input for estimations on construction minerals

- UNSD data
  - http://data.un.org/
  - Download data for
    - "Building bricks, made of clay"
    - "Cement, except in the form of clinkers"
    - "Tiles, roofing, made of clay"
- UN Comtrade
  - https://comtrade.un.org/data/
  - Download data for
    - 2523 (Portland cement)
    - 2505 (Sands of all kinds)
      - Please note: 250590 might be more correct, because excluding silica (but not sure how correct Comtrade classification is)
    - 2517 (Pebbles, gravels, aggregates and macadam)


#### Other data

- ./01_input/04_former_data
  - All required files should be found
    - in sub-folders of "W:\WU\Projekte\GRU\04_Daten\Datenbank"
    - or at copy of current repo: "W:\WU\Projekte\GRU\04_Daten\Datenbank\Copy_Github_Repository"

<br>





### Data cleaning/formatting

#### WMD

- clean_wmd.R
- Format all datasets from WMD
  - Steps:
    - Format recently downloaded data (i.e. for five latest years)
      - Read all individual spreadsheets
      - Delete obsolete rows
      - Turn into long format
    - Format information on data quality (for years 2015-2017)
      - Rename columns
      - Remove NAs
      - Fill all rows with respective commodity names


#### BGS

- clean_bgs.R
- Format recently downloaded data from BGS
  - Steps:
    - Format recently downloaded data 
      - Define years for column names in wide format
      - Provide names to production columns
      - Provide names to footnote columns
      - Turn to long format
      - Format column types and NAs


#### USGS

- clean_usgs.R
  - Please note: This script is a particular case: It stems from former data compilations from many years ago. It was in the meantime adjusted by a former assistant and since then was never revised to work properly again.
  - Scripts seems to not return all necessary data.
  - Manual exceptions for nearly 30 files have been added in the script.
  - Issue it not trivial, but not much important data seems to be lost, as most of the materials are covered by WMD and BGS.
  - For further information see comments in the script.


#### USGS extension data

- clean_harmonize_usgs_extension.R
- See "Data harmonization" below


#### EIA

- clean_eia.R
- Cleaning/Formatting of data retrieved from EIA API
  - Steps:
    - Rename and format column types
    - Split commodity-country info
    - Delete obsolete columns

<br>





### Data harmonization

#### WMD

- harmonize_wmd.R
- Harmonize all datasets from WMD
  - Steps:
    - Harmonize recently downloaded data (i.e. for five latest years)
      - Add alphanumiso
        - Check for missing allocations
      - Add material_ids
        - Check for missing allocations
      - Add unit_ids
        - Check for missing allocations
      - Aggregate
    - Harmonize information on data quality (for years 2015-2017)
      - Add alphanumiso
        - Check for missing allocations
      - Add material_ids
        - Check for missing allocations


#### BGS

- harmonize_bgs.R
- Harmonize data from BGS
    - Steps:
      - Remove problematic mat_ids (because likely not representing primary extraction)
      - Add alphanumiso
        - Check for missing allocations
      - Adjust geographic inconsistencies (based on geo_exist.csv and formula geographic_adjust.R)
      - Add material_ids
        - Check for missing allocations
      - Add material_ids for BGS sub-commodity column
        - Check for missing allocations
      - Adjustment of special cases for material_ids (e.g. missing sub-commodity entries or difference in reported materials)
      - Add unit_ids
        - Check for missing allocations
      - Adjustment of special cases which are wrong in BGS data
      - Harmonize footnote column
      - Aggregate
      - Harmonize data on sulphur from former database
        - Combine with downloaded data 


#### USGS

- harmonize_usgs.R
- Harmonize downloaded and former data from USGS (not USGS manual extensions)
  - Steps:
    - Harmonize downloaded data
      - Add alphanumiso
        - Check for missing allocations
      - Add unit_ids
        - Check for missing allocations
      - Add material_ids
        - Check for missing allocations
    - Remove values which have been identified as wrong
    - Combine recent and historic data


#### USGS extensions

- clean_harmonize_usgs_extensions.R
- Clean and Harmonize USGS manual data extensions from 2017 and 2021
- And integrate with each other and with other usgs data
  - Steps:
    - Harmonize extension 2021
      - Format
      - Check for wrong or missing alphanumiso
      - Check for wrong or missing material_id
      - Check for wrong or missing unit_id
    - Get only the entries different to input data
    - Combine
    - Aggregate


#### UNSD

- harmonize_unsd.R
- Harmonize data from UNSD
  - Steps:
    - Add alphanumiso
      - Check for missing allocations
    - Add material_ids
      - Check for missing allocations
    - Add unit_ids
      - Check for missing allocations
    - Adjust "Other hydrocarbons"
    - Aggregate


#### IEA

- harmonize_iea.R
- Harmonize data from IEA
  - Steps:
    - Harmonize recent data: Last ten years
      - Add alphanumiso
        - Check for missing allocations
      - Add material_ids
        - Check for missing allocations
      - Add unit_ids
        - Check for missing allocations
      - Aggregate
    - Adjust "Other hydrocarbons"
    - Adjust special cases


#### EIA

- harmonize_eia.R
- Harmonize data from EIA
  - Steps:
    - Add alphanumiso
      - Check for missing allocations
    - Add material_ids
      - Check for missing allocations
    - Add unit_ids
      - Check for missing allocations
    - Aggregate

<br>





### Data conversion 1 (elemental metals)

- conversion_elements.R
- Conversion of metal compounds to elemental metals
  - Steps:
    - Remove lithium from conversion (being a very specific case)
    - Loop (for all relevant data sources)
      - Joining conversion factors
      - Converting values
      - Replacing material_ids

<br>





### Data integration

#### WMD / BGS / USGS

- integration_wmd_bgs_usgs.R
- Integration of all metals and non-metallic minerals (From WMD, BGS, USGS)
  - Steps:
    - Create pre-defined lists
      - ores/minerals
      - mat_ids and associated_mat_ids
      - associated_mat_ids of all mat_ids (not minerals)
    - Remove obsolete time series from all datasets (i.e. with only values = 0)
    - Save table with all data combined and aggregate everything for countries which merged across time
      - in order to do integration with aggregated table in order to avoid double counting and re-combine with disaggregated data at end
      - Aggregate all in loop based on cou_for_aft
    - WMD / BGS
      - Save all BGS data before 1984 (because that's when WMD data starts)
      - Select source based on certainty of value (i.e. footnote: reported or estimated)
      - Get intermediate selection from bgs where there are more reported values for a material per country than in wmd data
      - Filter for everything else from WMD
      - Filter for everything else from BGS
      - Filter out everything from BGS which would be associated with a material from WMD (not including minerals)
      - Combine intermediate tables + bgs_1983
      - Create list of all ores/minerals which would also be covered by associated materials in intermediate data
      - Select minerals (i.e. everything which already represents extraction and therefore has priority over values representing content)
      - Get associated mat_ids for minerals above (i.e. materials already covered by their respective minerals)
      - Extend associated mat_ids for elements of agg_mat_ids (i.e. materials in aggregated categories)
      - Extend associated mat_ids for anything related to associated IDs above and not being a mineral/ore (e.g. Nm.PO being related to Nm.P)
      - Filter out associated materials from intermediate data, because DE already covered by ores/minerals
      - Filter out agg_mat_ids for countries where material in agg_mat_id is already included (int_data_3)
    - USGS
      - Do the same filtering as above, but also combine with respective associated IDs from WMD-BGS
        - Select minerals (i.e. everything which already represents extraction and therefore has priority over values representing content)
        - Get associated mat_ids for minerals above (i.e. materials already covered by their respective minerals)
        - Extend associated mat_ids for elements of agg_mat_ids (i.e. materials in aggregated categories)
        - Extend associated mat_ids for anything related to associated IDs above and not being a mineral/ore (e.g. Nm.PO being related to Nm.P)
        - Extend associated IDs with those from WMD-BGS
        - Filter out associated materials from intermediate data, because DE already covered by ores/minerals
        - Filter out anything related to associated IDs already included in data (not being a mineral)
        - Extend associated IDs with those from WMD-BGS
        - Filter out agg_mat_ids for countries where material in agg_mat_id is already included (int_data_3)
          - Extend by those from WMD-BGS
        - Filter out everything from usgs which would be associated with a material from wmd_bgs (not including minerals)
    - Combine everything
    - Filter out stuff temporarily which is likely to cause double counting
    - Filter out "other products"
    - Add disaggregated data again
      - Remove resulting NAs from re-adding disaggregated data


#### UNSD / IEA / EIA

- integration_fossil_fuels.R
- Integration of all fossil fuels (UNSD, IEA, EIA)
  - Steps:
    - Save table with all data combined and aggregate everything for countries which merged across time
      - Aggregate all in loop based on cou_for_aft
    - Integration UNSD with IEA
      - Compare coverage of UNSD and IEA for each material per country and decide which source to take
        - Comparison coal
          - UNSD filtered for coal
          - UNSD coal -> number of years with value > 0 per country
          - IEA filtered for coal
          - IEA coal -> number of years with value > 0 per country
          - Make a decision (i.e. iea only if coverage at least 50% higher)
          - Filter UNSD data for respective entries
          - Filter IEA data for respective entries
        - IEA coal prior 1991
      - Comparison non-coal fossil fuels (incl. peat)
        - Similar to coal above
      - Integrate all parts
      - Remove obsolete time series from all datasets (i.e. only values = 0)
    - Integration unsd_iea with EIA
      - Similar to UNSD-IEA above
    - Add disaggregated data again


#### Integration of all data

- integration_all.R
- Integration of metals & non-metallic minerals (wmd, bgs, usgs) with fossil fuels (unsd, iea, eia)

<br>





### Conversion 2 (units)

- conversion_units.R
- Convert all units to metric tons
  - Steps:
    - Check missing conversion factors
      - in general (especially for mass units)
      - for material-specific cases (i.e. volume/energy)
    - Add factor and convert 
      - for mass
      - for volume/energy
      - for volume/energy in case that factor for material_id is missing (temporary, until entries are added)
    - Remove entries which are not in "t"
    - Aggregate


<br>





### Estimations

#### Ores

- estimation_ores_smooth.R
- Estimation of ores for relevant materials
  - Steps:
    - Remove numbers smaller than or equal to 1 from GRU and SNL factors
    - Combine snl factors with gru factors
    - Remove outliers
    - Fill values for all years (i.e. 1970-2020)
    - Smooth
    - Create global averages from mining factors and integrate
    - Add old factors
    - Create global averages from old factors and integrate
    - Add additional factors
    - Create global averages from additional factors and integrate
    - Filter out Me.Li2O from global averages
    - Adjust specific cases
    - Apply estimations
    - Aggregate
    - Save all applied factors as csv
    - Save all integrated factors as csv


#### Ores

- estimation_construction_min.R
- Estimation of construction minerals
  - Steps:
    - Load all necessary input data
    - Harmonize UNSD cement production
    - Harmonize USGS cement
    - Combine cement data
    - Cement trade harmonization
    - Sand trade harmonization
    - Gravel trade harmonization
    - Brick production harmonization
    - Clay tile production harmonization
    - Bitumen harmonization
    - Calculations
      - Cement estimations
        - Reduce to maximum year of production
      - Bricks estimations
      - Clay tiles estimations
      - Bitumen estimations
    - Integrate/combine data
      - Ensure same time coverage for data points which are combined (i.e. M.coc and M.sag)
      - M.coc (cement, bricks, tiles)
        - Extend M.coc time series for cement estimations
        - Extend M.coc time series for bricks estimations
        - Extend M.coc time series for tiles estimations
      - M.sag (cement, bitumen)
        - Extend M.sag time series for cement estimations
        - Extend M.sag time series for bitumen estimations
    - Correct estimations for sand and gravel with respective trade data
    - Integrate with reported data
      - Aggregate data for M.sag and M.crs
      - Integrate
      - Add detail for M.sag and M.crs again

<br>





### Final cleaning/formatting

#### Final cleaning

- final_cleaning.R
- Final cleaning
  - Especially geographically
    - i.e. countries which shouldn't have entries in a certain year
    - i.e. double counting between predecessor and sucessor countries
  - Steps:
    - Adjust based on years in which a country should exist / have existed
    - Adjust based on possible double entries between predecessor and successor states


#### Adjustment of outliers
- check_large_changes.Rmd
  - Steps:
    - create upper and lower range based on standard deviation
    - create a table with adjusted values
    - integrate and save adjusted data


#### Final cleaning

- final_formatting.R
- Final formatting
  - Steps:
    - Remove obsolete time series (i.e. only values = 0)
    - Check for missing mat_id-CCC allocations
    - Fill all years for all A.3 after 2011 until max year of each CCC for each country
    - Aggregate
    - Save CCC data as csv
    - Save CCC data with names as csv
    - Save disaggregated data as rds
    - Save disaggregated data as csv
    - Save disaggregated data with names as csv
    - Yearly sums (for checking)


<br>





### Final checks

- final_checks.R
- Final checks
  - Steps:
    - Check double counting
      - by material_category and associated_id
      - by list of former-after countries
      - for countries which split
    - Check highest numbers
    - Comparison to former version
      - share of number of data points changed per ccc
      - share of number of data points changed per year
      - share of number of data points changed per country
      - absolute change by year
      - largest positive changes per year and ccc
      - largest negative changes per year and ccc
      - number of data points changed (positive, negative, new/removed or changed materials)
    - Save comparison files








## Considerable changes compared to data compilation in ...

...





## Other information



**Adjustment of concordance tables for missing values**

Insert missing values in the respective concordance tables, i.e. source_material_ids.csv, etc.
These are shown during the update across the respective "nomatch" tables.  

Explanation: Due to possible changes in the namings of countries, materials or units by the primary sources, it can occur that certain errors occur during the harmonization (i.e. the matching to our respective codes). 
For a range values the matching is not supposed to happen (e.g. regions/continents; or materials not integrated from that source, like fossil fuels from WMD). This has to be checked on an individual basis. Automatic exclusion could be implemented, would however underlie the same problems in changes of namings and therefore does not promise to deliver a complete cleaning of irrelevant values.

<br>



**Classification detail**

Please note that concordance tables depicting primary-source-to-database allocations are mostly applicable one-way, but not two-way, i.e. for assigning an internal ID for each entry in the primary data, but not the other way round. The main reason is that in order to harmonize data to a common format, some primary entries are given the same internal ID twice. For example, in case of country IDs, both, the Dem. Rep. of Congo and Zaire are given the country's current ID, in fact being the same geographic entity over the whole time period.  
Regarding the (re-)unifications and the split of countries, in almost every single cases the highest detail and actual geographical existence is kept, e.g. West and East Germany, North and South Yemen, Ethiophia and Eritrea, Sudan and South Sudan. An exception here are North and South Vietnam, both being assigned to Vietnam for the respective years.

Please note that minor variations in the classification of countries as well as commodities may occur. Even though the rough classification of all data sources might appear the same or very similar, there are many differences if looking in detail. In terms of countries, small states might be reported individually or included within larger countries, for example Monaco in France, San Marino in Italy, Hong Kong and Macau in China, Liechtenstein in Switzerland, Zanzibar in Tanzania. The underlying data integration tries to always keep the highest detail, but in some cases countries are already aggregated and a disaggregated processing/integration not possible. In such cases, the combined value is associated with the larger country in which the other one was included.
For commodities, due to the highly varying nature of types of minerals and the need to report mostly commodity categories, the issue is somewhat different, i.e. it may occur in some cases that certain minerals are included within different categories. If possible to properly distinguish, the reported value will be associated with the proper commodities in the final data (e.g. oil production from tar sands in Canada converted and assigned to/as tar sand production if reported as "Other hydrocarbons"). However, in some cases such differentiation is not possible (e.g limestone reported together with other stones as "Stone" or "Gravel", etc.).
All the above can be seen in detail in the respective concordanc tables in 01_input/01_concordance_tables.

<br>


**Units and unit conversion table**

The harmonized unit codes/IDs are based on a simple and flexibly extendable system, applying metric units for mass and volume as well as standard units for energy.
In order to differentiate between multiples of all kinds of units, standard SI-prefixes are used for all kinds of units, independent of the units underlying system. This means it is applied in the same way for metric tons (e.g. t/Kt/Mt) as for barrels per day (e.g. bpd/Kbpd/Mbpd).

All data points are converted to metric tons. The conversion factors (multipliers) for units are not kept in the table unit_ids, in order to keep it flexible to extend on any level of detail. While keeping a clean structure, this allows for differentiating on a high level of detail per type of commodity as well as per country, and if necessary, even time. So far, such a differentiation is only made per types of commodity (e.g. m³ of stone, sand, wood, etc.).

<br>


