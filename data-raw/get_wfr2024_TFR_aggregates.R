################################################################################
###
### DATE CREATED: 2024-08-01
###
### AUTHOR: Mark Wheldon
###
### PROJECT: FPEMglobal & World Fertility Report 2024
###
### DESCRIPTION: Get country and area groups (aggregates) based on TFR
### used for World Fertility Report 2024.
###
### Country aggregates used in WFR 2024 are based on TFR. There are
### three groups, defined by when TFR went below 2.1: 3 groups:
### - before 1994
### - 1994 to 2054
### - after 2054
###
### The source file, 'Countries_TFRgroups.xlsx' was provided by
### Vladimira Kantorova.
###
###-----------------------------------------------------------------------------
###
### NOTES
###
### Country aggregation lists are saved as both .xlsx and .rda
### files. The .xlsx files are for user convenience. The .rda files
### are for potential use in the package (these are also convenient
### for users because they are under '/data'). There are no .csv files
### because this is how non-ASCII characters in country names turn
### into kryptonite and wreck everything.
###
################################################################################

###-----------------------------------------------------------------------------
### * Set Up

.packages = c("openxlsx", "readxl", "here")
sink_ <- lapply(.packages, function(z) stopifnot(require(package = z, character.only = TRUE)))

###-----------------------------------------------------------------------------
### * Read Aggregate Definitions

###-----------------------------------------------------------------------------
### ** File Locations

data_raw_dir <- here::here("data-raw")
extdata_dir <- here::here("inst", "extdata")
data_dir <- here::here("data")

if (!dir.exists(data_raw_dir)) stop("Cannot find '/data-raw' directory: '", toString(data_raw_dir), "' does not exist.")
if (!dir.exists(extdata_dir)) dir.create(extdata_dir, recursive = TRUE)
if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)

###-----------------------------------------------------------------------------
### ** Read File

agg_wfr2024_raw <-
    readxl::read_excel(path = file.path(data_raw_dir, "Countries_TFRgroups.xlsx"))

###-----------------------------------------------------------------------------
### * Export Aggregates

###-----------------------------------------------------------------------------
### ** rda File

aggregates_wfr2024_locations <-
    data.frame(LocID = agg_wfr2024_raw$LocID,
               LocTypeID = 4L,
               LocTypeName = "Country/Area",
               LocationID = agg_wfr2024_raw$LocID,
               ParentPrintName = c("Before 1994", "1994-2054", "After 2054")[as.numeric(agg_wfr2024_raw$TFR_group)])

save(aggregates_wfr2024_locations, file = file.path(data_dir, "aggregates_wfr2024_locations.rda"))

###-----------------------------------------------------------------------------
### ** csv File

aggregates_wfr2024_csv <-
    data.frame(iso.country = aggregates_wfr2024_locations$LocID,
               iso.group = "",
               groupname = aggregates_wfr2024_locations$ParentPrintName)

aggregates_wfr2024_csv <- aggregates_wfr2024_csv[order(aggregates_wfr2024_csv$iso.country), ]

write.csv(aggregates_wfr2024_csv,
          file = file.path(extdata_dir, "aggregates_special_wfr2024.csv"),
          row.names = FALSE)
