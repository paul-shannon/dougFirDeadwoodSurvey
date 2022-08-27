library(yaml)
library(RUnit)
library(readr)

options(digits=9)

if(interactive()){
    base.name <- "template-minimal.yaml"
  }else{
      args <- commandArgs(trailingOnly=TRUE)
      if(length(args) != 1)
         stop("usage Rscript readYaml.R df-xxx")
      base.name <- args[1]
      }

if(grepl(".yaml", base.name, fixed=TRUE))
    base.name <- sub(".yaml", "", base.name, fixed=TRUE)

f <- sprintf("%s.yaml", base.name)
stopifnot(file.exists(f))
x <- yaml.load(readLines(f, n=-1))
length(x)
head(names(x))
expected.fields <- c("description",
                     "notes",
                     "status",
                     "datedReferenceTree",
                     "date",
                     "lat",
                     "long",
                     "dbh",
                     "yearOfDeath",
                     "yearOfDeath.evidence",
                     "failureMode",
                     "failureHeight",
                     "directionOfFall",
                     "branches.small",
                     "branches.large",
                     "branches.huge",
                     "bark",
                     "collapse",
                     "epiphyte.overall")
#----------------------------------------------------------------------------------------------------
valid <- function(id, el)
{
   printf("------------ checking %s", id)

   missing.fields <- setdiff(expected.fields, names(el))
   if(length(missing.fields) > 0)
       stop(sprintf("missing fields for %s: %s", id, paste(missing.fields, collapse=", ")))

   extra.fields   <- setdiff(names(el), expected.fields)
   if(length(extra.fields) > 0)
       stop(sprintf("extra fields for %s: %s", id, paste(extra.fields, collapse=", ")))

   checkEquals(names(el), expected.fields)

   description <- el[[ "description"]]
   checkTrue(is.character(description))
   printf("--- description valid: %s", nchar(description) > 0)

   notes <- el[["notes"]]
   checkTrue(is.character(notes))

   status <- el[[ "status"]]
   checkTrue(is.character(status))
   checkTrue(status %in% c("NA", "placeholder", "active", "ready"))

   datedReferenceTree <- el[[ "datedReferenceTree"]]
   checkTrue(datedReferenceTree %in% c("TRUE", "FALSE"))
   date <- el[[ "date"]]

   date.parsable <- suppressWarnings(!is.na(parse_datetime(date, "%Y-%m-%d")))
   checkTrue(date.parsable)

   lat <- el[["lat"]]
   checkTrue(is.numeric(lat) & lat >= 47 & lat <= 48)

   long <- el[["long"]]
   checkTrue(is.numeric(lat) & long >= 122 & long <= 123)

   dbh <- el[["dbh"]]
   checkTrue(is.numeric(dbh) & (dbh == -1 | (dbh > 8 & dbh < 85)))

   yearOfDeath <- el[["yearOfDeath"]]
   checkTrue(is.numeric(yearOfDeath) & (yearOfDeath == -1 | (yearOfDeath > 1930 & yearOfDeath < 2022)))

   yearOfDeath.evidence <- el[["yearOfDeath.evidence"]]
   checkTrue(is.character(yearOfDeath.evidence))

   failureMode <- el[["failureMode"]]
   checkTrue(is.character(failureMode) & failureMode %in% c("NA", "uproot", "roots", "snag", "logging"))

   failureHeight <- el[["failureHeight"]]
   checkTrue(is.numeric(failureHeight) & failureHeight %in% -1:100)

   directionOfFall <- el[["directionOfFall"]]
   checkTrue(is.numeric(directionOfFall) & directionOfFall %in% -1:360)

   branches.small <- el[["branches.small"]]
   checkTrue(is.numeric(branches.small) & branches.small %in% -1:40)

   branches.large <- el[["branches.large"]]
   checkTrue(is.numeric(branches.large) & branches.large %in% -1:40)

   branches.huge <- el[["branches.huge"]]
   checkTrue(is.numeric(branches.huge) & branches.huge %in% -1:40)

   bark <- el[["bark"]]
   checkTrue(is.numeric(bark) & bark == -1 | (bark >= 0 & bark <= 1))

   collapse <- el[["collapse"]]
   checkTrue(is.numeric(collapse) & (collapse == -1 | (collapse >= 0 & collapse <= 1)))

   epiphyte.overall <- el[["epiphyte.overall"]]
   checkTrue(is.numeric(epiphyte.overall) &
             (epiphyte.overall == -1 | (epiphyte.overall >= 0 & epiphyte.overall <= 1)))

   return(TRUE)

} # function valid
#----------------------------------------------------------------------------------------------------
extract <- function(name, el)
{
    tbl <- tryCatch({data.frame(
                         id=name,
                         lat = el[["lat"]],
                         long = el[["long"]],
                         dbh = el[["dbh"]],
                         yearOfDeath = el[["yearOfDeath"]],
                         yearOfDeath.evidence = el[["yearOfDeath.evidence"]],
                         failureMode = el[["failureMode"]],
                         failureHeight = el[["failureHeight"]],
                         directionOfFall = el[["directionOfFall"]],
                         branches.small = el[["branches.small"]],
                         branches.large = el[["branches.large"]],
                         branches.huge = el[["branches.huge"]],
                         bark = el[["bark"]],
                         collapse = el[["collapse"]],
                         epiphyte.overall = el[["epiphyte.overall"]],
                         description = el[["description"]],
                         notes = el[["notes"]],
                         status = el[["status"]],
                         datedReferenceTree = el[["datedReferenceTree"]],
                         date = el[["date"]])
             }, error = function(e){
                 print(e)
                 printf(" failed with %s", el$id)
                 })

    if(tbl$long > 0)
        tbl$long <- -1 * tbl$long
    tbl

} # function extract
#----------------------------------------------------------------------------------------------------
if(valid(names(x)[1], x[[1]])){
   tbl <- extract(names(x)[[1]], x[[1]])
   }
