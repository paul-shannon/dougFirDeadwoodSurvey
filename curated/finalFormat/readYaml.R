library(yaml)
library(RUnit)
library(readr)

options(digits=9)


if(interactive()){
    base.name <- "df-117"
  }else{
      args <- commandArgs(trailingOnly=TRUE)
      if(length(args) != 1)
         stop("usage Rscript readYaml.R df-xxx")
      base.name <- args[1]
      }
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
                     "ground.contact",
                     "ground.contact.notes",
                     "sag",
                     "bark",
                     "bottom.decay",
                     "bottom.decay.notes",
                     "mid.decay",
                     "mid.decay.notes",
                     "top.decay",
                     "top.decay.length",
                     "top.decay.notes",
                     "bottom.cross.section",
                     "top.cross.section",
                     "epiphytes.meter.1",
                     "epiphytes.meter.2",
                     "epiphyte.species",
                     "epiphytes.overall.moss",
                     "epiphyte.overall",
                     "epiphyte.overall.notes",
                     "bottom.cross.section")

# for(i in seq_len(length(x))){
#     actual <- names(x[[i]])
#     printf("checking tree %s", names(x)[i])
#     missing <- setdiff(expected.fields, names(x[[i]]))
#     if(length(missing) > 0){
#         printf("missing: %s", paste(missing, collapse=", "))
#         stop()
#         } # if missing
#     epiphyte.sub.1 <- x[[i]]$epiphytes.meter.1
#     stopifnot(sort(names(epiphyte.sub.1)) == c("leafCoverage", "moss"))
#     epiphyte.sub.2 <- x[[i]]$epiphytes.meter.2
#     stopifnot(sort(names(epiphyte.sub.2)) == c("leafCoverage", "moss"))
#     } # for i

   # our only nested fields are in epiphytes.meter.[12]. sure they are as expected

#----------------------------------------------------------------------------------------------------
valid <- function(id, el)
{
   printf("------------ checking %s", id)
   description <- el[[ "description"]]
   printf("--- description valid: %s", nchar(description) > 0)
   notes <- el[["notes"]]
   printf("--- notes valid: %s", is.character(notes))
   status <- el[[ "status"]]
   printf("--- status valid: %s", status %in% c("placeholder", "active", "ready"))
   datedReferenceTree <- el[[ "datedReferenceTree"]]
   printf("--- datedReferenceTree valid: %s", datedReferenceTree %in% c("TRUE", "FALSE"))
   date <- el[[ "date"]]
   date.parsable <- suppressWarnings(!is.na( parse_datetime(date, "%Y-%m-%d")))
   printf("--- date valid: %s", date.parsable)
   lat <- el[[ "lat"]]
   printf("--- lat valid: %s", is.numeric(lat) & lat > 47 & lat < 48)
   long <- el[[ "long"]]
   printf("--- long valid: %s", is.numeric(long) & long > 122 & long < 123)
   dbh <- el[[ "dbh"]]
   printf("--- dbh valid: %s", is.numeric(dbh) & (dbh == -1 | (dbh > 8 & dbh < 85)))
   yearOfDeath <- el[[ "yearOfDeath"]]
   printf("--- yearOfDeath: %s", is.numeric(yearOfDeath) &
                           (yearOfDeath == -1 | (yearOfDeath > 1930 & yearOfDeath < 2022)))
   yearOfDeath.evidence <- el[[ "yearOfDeath.evidence"]]
   printf("--- yearOfDeath.evidence valid: %s", is.character(yearOfDeath.evidence))

   failureMode <- el[[ "failureMode"]]
   printf("failureMode valid: %s", is.character(failureMode) & failureMode %in%
                                   c("NA", "uproot", "roots", "snag", "logging"))
   failureHeight <- el[[ "failureHeight"]]
   printf("failureHeight valid: %s", is.numeric(failureHeight) & failureHeight %in% -1:100)
   directionOfFall <- el[[ "directionOfFall"]]
   printf("directionOfFall valid: %s", is.numeric(directionOfFall) & directionOfFall %in% -1:360)
   branches.small <- el[[ "branches.small"]]
   printf("branches.small valid: %s", is.numeric(branches.small) & branches.small %in% -1:40)

   branches.large <- el[[ "branches.large"]]
   printf("branches.large valid: %s", is.numeric(branches.large) & branches.large %in% -1:40)

   branches.huge <- el[[ "branches.huge"]]
   printf("branches.huge valid: %s", is.numeric(branches.huge) & branches.huge %in% -1:40)

   ground.contact <- el[[ "ground.contact"]]
   printf("ground.contact valid: %s", is.numeric(ground.contact) &
                                      (ground.contact == -1  | (ground.contact >= 0 & ground.contact <= 1)))

   sag <- el[[ "sag"]]
   printf("sag valid: %s", is.numeric(sag) &
                                     (sag == -1 | (sag >= 0 & sag <= 1)))

   bark <- el[[ "bark"]]
   printf("bark valid: %s", is.numeric(bark) & (bark == -1 | (bark >= 0 & bark <= 1)))

   bottom.decay <- el[[ "bottom.decay"]]
   printf("bottom.decay valid: %s", is.numeric(bottom.decay) &
                                    (bottom.decay == -1 | (bottom.decay >= 0 & bottom.decay <= 1)))

   bottom.decay.notes <- el[[ "bottom.decay.notes"]]
   printf("bottom.decay.notes valid: %s", is.character(bottom.decay.notes))

   mid.decay <- el[[ "mid.decay"]]
   printf("mid.decay valid: %s", is.numeric(mid.decay) &
                                    (mid.decay == -1 | (mid.decay >= 0 & mid.decay <= 1)))

   mid.decay.notes <- el[[ "mid.decay.notes"]]
   printf("mid.decay.notes valid: %s", is.character(mid.decay.notes))

   top.decay <- el[[ "top.decay"]]
   printf("top.decay valid: %s", is.numeric(top.decay) &
                                 (top.decay == -1 | (top.decay >= 0 & top.decay <= 1)))

   top.decay.length <- el[[ "top.decay.length"]]
   printf("top.decay.length valid: %s", is.numeric(top.decay.length) & top.decay.length %in% -1:100)

   top.decay.notes <- el[[ "top.decay.notes"]]
   printf("top.decay.notes valid: %s", is.character(top.decay.notes))

   bottom.cross.section <- el[[ "bottom.cross.section"]]
   printf("bottom.corss.section valid: %s",
          is.character(bottom.cross.section) & bottom.cross.section %in% c("NA", "round", "oval", "depleted"))

   top.cross.section <- el[[ "top.cross.section"]]
   printf("top.ocrss.section valid: %s",
          is.character(top.cross.section) & top.cross.section %in% c("NA", "round", "oval", "depleted"))

   epiphytes.meter.1 <- el[[ "epiphytes.meter.1"]]
   moss <- epiphytes.meter.1$moss
   printf("ephiphytes.meter.1")
   printf("     moss valid: %s", is.numeric(moss) & (moss == -1 | (moss >= 0 & moss <= 1.0)))
   leafCoverage <- epiphytes.meter.1$leafCoverage
   printf("     leafCoverage valid: %s", is.numeric(leafCoverage) &
                                         (leafCoverage == -1 | (leafCoverage >= 0 & leafCoverage <= 1.0)))

   epiphytes.meter.2 <- el[[ "epiphytes.meter.2"]]
   moss <- epiphytes.meter.2$moss
   printf("ephiphytes.meter.2")
   printf("     moss valid: %s", is.numeric(moss) & (moss == -1 | (moss >= 0 & moss <= 1.0)))
   leafCoverage <- epiphytes.meter.2$leafCoverage
   printf("     leafCoverage valid: %s", is.numeric(leafCoverage) &
                                         (leafCoverage == -1 | (leafCoverage >= 0 & leafCoverage <= 1.0)))

   epiphyte.species <- el[["epiphyte.species"]]
   species <- strsplit(epiphyte.species, ", ")[[1]]
   known.species <- c("NA", "psme", "tshe", "mane", "pomu", "hodi", "lohi", "syal",
                      "syhe", "thpi", "arme", "vaov", "vapa", "gash", "ruur",
                      "qual", "quve", "quru", "quga", "acma", "rusp")

   printf("epiphyte.species valid: %s",  all(species %in%  known.species))

   epiphytes.overall.moss <- el[[ "epiphytes.overall.moss"]]
   printf("epiphytes.overall.moss valid: %s",
             is.numeric(epiphytes.overall.moss) &
             (epiphytes.overall.moss == -1 | (epiphytes.overall.moss >= 0 & epiphytes.overall.moss <= 1)))

   epiphyte.overall <- el[[ "epiphyte.overall"]]
   printf("epiphyte.overall valid: %s", is.numeric(epiphyte.overall) &
                                   (epiphyte.overall == -1 | (epiphyte.overall >= 0 & epiphyte.overall <= 1)))
} # function valid
#----------------------------------------------------------------------------------------------------
extract <- function(el)
{
    #printf("--- extracting %s to data.frame", el$id)
    missing.fields <- setdiff(expected.fields, names(el))
    if(length(missing.fields) > 0){
       printf("missing fields: %s", paste(missing.fields, collapse=", "))
       stop()
       }

    tbl <- tryCatch({data.frame(
                         description = el[["description"]],
                         notes = el[[ "notes"]],
                         status = el[[ "status"]],
                         datedReferenceTree = el[[ "datedReferenceTree"]],
                         date = el[[ "date"]],
                         lat = el[[ "lat"]],
                         long = el[[ "long"]],
                         dbh = el[[ "dbh"]],
                         yearOfDeath = el[[ "yearOfDeath"]],
                         yearOfDeath.evidence = el[[ "yearOfDeath.evidence"]],
                         failureMode = el[[ "failureMode"]],
                         failureHeight = el[[ "failureHeight"]],
                         directionOfFall = el[[ "directionOfFall"]],
                         branches.small = el[[ "branches.small"]],
                         branches.large = el[[ "branches.large"]],
                         branches.huge = el[[ "branches.huge"]],
                         ground.contact = el[[ "ground.contact"]],
                         sag = el[[ "sag"]],
                         bark = el[[ "bark"]],
                         bottom.decay = el[[ "bottom.decay"]],
                         bottom.decay.notes = el[[ "bottom.decay.notes"]],
                         mid.decay = el[[ "mid.decay"]],
                         mid.decay.notes = el[[ "mid.decay.notes"]],
                         top.decay = el[[ "top.decay"]],
                         top.decay.length = el[[ "top.decay.length"]],
                         top.decay.notes = el[[ "top.decay.notes"]],
                         bottom.cross.section = el[[ "bottom.cross.section"]],
                         top.cross.section = el[[ "top.cross.section"]],
                         epiphytes.meter.1 = el[[ "epiphytes.meter.1"]],
                         epiphytes.meter.2 = el[[ "epiphytes.meter.2"]],
                         epiphyte.species = el[["epiphyte.species"]],
                         epiphytes.overall.moss = el[[ "epiphytes.overall.moss"]],
                         epiphyte.overall = el[[ "epiphyte.overall"]])
             }, error = function(e){
                 printf(" failed with %s", el$id)
                 print(e)
                 })

    tbl

} # function extract
#----------------------------------------------------------------------------------------------------
run <- function()
{
    valid(base.name, x[[1]])
    browser()
    tbl <- extract(x[[1]])
    #rows <- lapply(x, extract)
    #printf("row count: %d", length(rows))
    #tbl <- do.call(rbind, rows)
    tbl$id <- names(x)
    tbl$long <- -1 * tbl$long

    f.out <- sprintf("tsv/%s.tsv", base.name)
    write.table(tbl, file=f.out, sep="\t", row.names=FALSE, col.names=TRUE, quote=FALSE)
    print(as.data.frame(t(tbl)))

} # run
#----------------------------------------------------------------------------------------------------
