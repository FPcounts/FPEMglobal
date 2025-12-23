################################################################################
###
### DATE CREATED: 2025-02-18
###
### AUTHOR: Mark Wheldon
###
### PROJECT: FPEMglobal
###
### DESCRIPTION: Get regional commission aggregates, as updated 12 Dec 2024.
###
###-----------------------------------------------------------------------------
###
### NOTES
###
### These are saved in an .xlsx file.
###
################################################################################

###-----------------------------------------------------------------------------
### * Set Up

.packages = c("readxl")
.sink <-
    lapply(.packages,
           function(z) stopifnot(require(package = z, character.only = TRUE)))

###-----------------------------------------------------------------------------
### * Read Aggregate Definitions

###-----------------------------------------------------------------------------
### ** File Locations

data_raw_dir <- here::here("data-raw")
extdata_dir <- here::here("inst", "extdata")
data_dir <- here::here("data")

if (!dir.exists(data_raw_dir))
    stop("Cannot find '/data-raw' directory: '", toString(data_raw_dir), "' does not exist.")
if (!dir.exists(extdata_dir)) dir.create(extdata_dir, recursive = TRUE)
if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)

###-----------------------------------------------------------------------------
### ** Read File

aggregates_rc_20241219_raw <-
    readxl::read_xlsx(path = file.path(data_raw_dir,
                                       "./CompositionOfRegions_RCs_20241219.xlsx"),
                      sheet = "Ref_Area_Long")

###-----------------------------------------------------------------------------
### * Export Aggregates

###-----------------------------------------------------------------------------
### ** rda File

## This has been manually put in 'DemoData' format by mapping columns from the
## UNSD .xlsx file to fields in DemoData locations table.

aggregates_rc_20241219_locations <-
    data.frame(LocID = aggregates_rc_20241219_raw$M49,
               LocTypeID = 4L,
               LocTypeName = "Country/Area",
               LocationID = aggregates_rc_20241219_raw$M49,
               LocPrintName = aggregates_rc_20241219_raw$Name,
               ParentID = aggregates_rc_20241219_raw$RC_UNSDCode,
               ParentTypeID = 13,
               ParentTypeName = "Special other",
               ParentPrintName = aggregates_rc_20241219_raw$RC_RegionName)

comment(aggregates_rc_20241219_raw) <-
    c("Manually created from a spreadsheet of Regional Commission regions prepared by the UN Statistics Division, updated 2024-12-19. The spreadsheet columns were manually mapped to DemoData fields; these regions do not appear in DemoData as at 2024-12-19.")

save(aggregates_rc_20241219_locations,
     file = file.path(data_dir, "aggregates_rc_20241219_locations.rda"))

###-----------------------------------------------------------------------------
### ** csv Files

for (rc in c("eca", "ece", "eclac", "escap", "escwa")) {
    rc_agg <- subset(aggregates_rc_20241219_locations,
                     ParentTypeID == 13 &
                     grepl(paste0("^", toupper(rc), "[ :]+[A-Z]+"), ParentPrintName))
    rc_agg <- data.frame(iso.country = rc_agg$LocationID, groupname = rc_agg$ParentPrintName,
                      iso.group = rc_agg$ParentID)
    write.csv(rc_agg, file = file.path(extdata_dir, paste0("aggregates_special_", rc, "_regions_20241219.csv")),
              row.names = FALSE)
}
