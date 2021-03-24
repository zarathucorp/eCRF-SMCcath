library(RSQLite)
library(tibble)
#setwd("~/ShinyApps/SMC-cardio/eCRFexam")
# Create a connection object with SQLite
conn <- dbConnect(
  RSQLite::SQLite(),
  "data/data.sqlite3"
)

# Create a query to prepare the 'mtcars' table with additional 'uid', 'id',
# & the 4 created/modified columns
create_rct_query = "CREATE TABLE rct (
  pid                             TEXT PRIMARY KEY,
  'Group'                           TEXT,
  Initial                         TEXT,
  Age                             REAL,
  Sex                             TEXT,
  Height                          REAL,
  Weight                          REAL,
  BMI                             REAL,
  Smoking                         TEXT,
  AMI_Type                        TEXT,
  HTN                             TEXT,
  DM                              TEXT,
  DM_Tx                           TEXT,
  Dyslipidemia                    TEXT,
  CKD                             TEXT,
  Dialysis                        TEXT,
  Prev_Bleeding                   TEXT,
  Prev_HF_Adm                     TEXT,
  Hx_MI                           TEXT,
  Hx_PCI                          TEXT,
  Hx_CABG                         TEXT,
  Hx_CVA                          TEXT,
  Hx_AF                           TEXT,
  SBP_Cathlab_Pre_PCI             REAL,
  DBP_Cathlab_Pre_PCI             REAL,
  HR_Cathlab_Pre_PCI              REAL,
  SBP_Cathlab_Post_PCI            REAL,
  DBP_Cathlab_Post_PCI            REAL,
  HR_Cathlab_Post_PCI             REAL,
  Last_FU_Date                    DATE,
  Death                           TEXT,
  Death_Date                      DATE,
  MI                              TEXT,
  MI_Date                         DATE,
  TVMI                            TEXT,
  TVMI_Date                       DATE,
  TLR                             TEXT,
  TLR_Date                        DATE,
  TVR                             TEXT,
  TVR_Date                        DATE,
  Readmission_Total               TEXT,
  Readmission_Total_Date          DATE,
  TLF                             TEXT,
  TLF_Date                        DATE,
  created_at                      TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  created_by                      TEXT,
  modified_at                     TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  modified_by                     TEXT
)"


create_pros_query <- gsub(" rct ", " pros ", create_rct_query)

# dbExecute() executes a SQL statement with a connection object
# Drop the table if it already exists
dbExecute(conn, "DROP TABLE IF EXISTS rct")
dbExecute(conn, "DROP TABLE IF EXISTS pros")
# Execute the query created above
dbExecute(conn, create_rct_query)
dbExecute(conn, create_pros_query)


dat <- readRDS("data/eCRFexam.RDS")[, 1:29][, -1]
names(dat)[1] <- "pid"
dat[, 1] <- readRDS("data/random.RDS")[1, 1]
dat <- cbind(tibble(pid = dat[, 1]), tibble(readRDS("data/random.RDS")[1, 2]), dat[, -1],
             tibble(Last_FU_Date = as.character(as.Date(Sys.time())),
                    Death = "1",
                    Death_Date = as.character(as.Date(Sys.time())),
                    MI = "1",
                    MI_Date = as.character(as.Date(Sys.time())),
                    TVMI = "1",
                    TVMI_Date = as.character(as.Date(Sys.time())),
                    TLR = "1",
                    TLR_Date = as.character(as.Date(Sys.time())),
                    TVR = "1",
                    TVR_Date = as.character(as.Date(Sys.time())),
                    Readmission_Total = "1",
                    Readmission_Total_Date = as.character(as.Date(Sys.time())),
                    TLF = "1",
                    TLF_Date = as.character(as.Date(Sys.time()))
             ),
             tibble(created_at = as.character(lubridate::with_tz(Sys.time(), tzone = "UTC")),
                    created_by = "jinseob2kim@gmail.com",
                    modified_at = as.character(lubridate::with_tz(Sys.time(), tzone = "UTC")),
                    modified_by = "jinseob2kim@gmail.com",))

for (i in c(1, 9:23)){
  class(dat[, i]) <- "character"
}

rdat <- tibble(dat)
pdat <- tibble(dat)

#rdat[,1] <- paste0("R-", rdat[, 1])
pdat[,1] <- paste0("P-", 1)

# Fill in the SQLite table with the values from the RDS file
DBI::dbWriteTable(
  conn,
  name = "rct",
  value = rdat,
  overwrite = FALSE,
  append = TRUE
)

DBI::dbWriteTable(
  conn,
  name = "pros",
  value = pdat,
  overwrite = FALSE,
  append = TRUE
)


# List tables to confirm 'mtcars' table exists
dbListTables(conn)

 # disconnect from SQLite before continuing
dbDisconnect(conn)
