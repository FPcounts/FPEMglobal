################################################################################
###
### DATE CREATED: 2024-07-16
###
### AUTHOR: Mark Wheldon
###
### PROJECT: FPEMglobal
###
### DESCRIPTION: Get country and area groups (aggregates) that were
### used in WPP 2024.
###
### Country aggregates used in WPP 2024 are different, in many cases,
### from the aggregates used previously. This script downloads them
### from DemoData for use in FPEMglobal.
###
### This script was adapted from ".../United Nations/DESA-POP -
### PEPS/WPP2024/API/DemoBaseOPS-API-json-2024.R", written by Patrick Gerland.
###
###-----------------------------------------------------------------------------
###
### NOTES
###
### Country aggregation lists are saved as both .xlsx and .rda
### files. The .xlsx files are for user convenience. The .rda files
### are for use in the package (these are also convenient for users
### because they are under '/data'). There are no .csv files because
### this is how non-ASCII characters in country names turn into
### kryptonite and wreck all and everything about the package's
### internal gummings.
###
################################################################################

###-----------------------------------------------------------------------------
### * Set Up

.packages = c("RODBC", "openxlsx", "readxl", "data.table",
              "lubridate", "RJSONIO", "dplyr", "httr", "jsonlite",
              "RCurl",
              "here", "keyring")
sink_ <- lapply(.packages, function(z) stopifnot(require(package = z, character.only = TRUE)))

###-----------------------------------------------------------------------------
### * Download Aggregate Definitions

###-----------------------------------------------------------------------------
### ** File Locations

extdata_dir <- here::here("inst", "extdata")
data_dir <- here::here("data")

if (!dir.exists(extdata_dir)) dir.create(extdata_dir, recursive = TRUE)
if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)

###-----------------------------------------------------------------------------
### ** Parameters

## query EAGLE list of locations
WPP_revision <- 2024
WPP_RevID <- 20

## myRevisionYear <- WPP_revision
myRevisionID <- WPP_RevID
myProjectionYear <- 2024
myVariant <- "Medium"

eagle_URL      <- "https://popdiv.dfs.un.org/peps/eagle/api/file/ProcessedListCompact/"
## eagle_locations <- data.table(fromJSON(paste0(eagle_URL, WPP_revision), flatten=TRUE))

## userid & password for Eagle/PEPxplorer API
if (!isTRUE(grepl("https://popdiv\\.dfs\\.un\\.org/peps/eagle/api", keyring::key_list()$service))) {
    stop("Key for service 'https://popdiv.dfs.un.org/peps/eagle/api' not found. Use 'key_set(service = \"https://popdiv.dfs.un.org/peps/eagle/api\", username = \"Patrick.Gerland\")' to set the password and re-run.")
}
user <- "Patrick.Gerland"
pw <- keyring::key_get(service = "https://popdiv.dfs.un.org/peps/eagle/api", username = "Patrick.Gerland")

options(scipen = 999)
options(timeout = max(1000, getOption("timeout")))

## List of standard and special aggregates for aggregations
List_Aggregates_Codes <-
    c(standard = 1002,
      geographical_groups = 5000,
      economic_trade_groups = 5001,
      political_groups = 5002,
      un_groups = 5003,
      population_peak_groups = 5006
      )

List_Aggregates <- setNames(nm = List_Aggregates_Codes)

###-----------------------------------------------------------------------------
### ** Query

wpp2024_list_aggregates <-
    lapply(List_Aggregates, function(myList) {
        obj_name <- paste0("wpp2024_list_aggregates_", myList)
        eagle_URL <-
            paste0("https://popdiv.dfs.un.org/peps/pepxplorer/api/location/locationlist/",
                   WPP_RevID, "/", myList)
        json_file <- httr::GET(eagle_URL, authenticate(user, pw, type="any"))
        ## transform json object back into text and flatten it
        return(jsonlite::fromJSON(content(json_file, 'text'),
                                  simplifyVector = TRUE, flatten = TRUE))
    })

save(wpp2024_list_aggregates, file = file.path(data_dir, "wpp2024_list_aggregates.rda"))

locations <- lapply(wpp2024_list_aggregates, function(z) data.table(z$Locations))
openxlsx::write.xlsx(locations, file = file.path(extdata_dir, "wpp2024_list_aggregates.xlsx"), asTable = TRUE)
