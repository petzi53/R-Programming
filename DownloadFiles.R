# Sys.setenv(LANG = "en")
library(readr)

myPath <- "/Users/petzi/Documents/_PB-Data/Programming/R/Coursera-R/getting_data"
if (!getwd() == myPath) {
        setwd(myPath)
}
if (!dir.exists("DATA")) {
        dir.create("DATA")
}
#-------------------------------------------------------------------------------
library(rio)
fileUrl.csv <-
        "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD"
download.file(
        fileUrl.csv,
        destfile = "./DATA/cameras.csv",
        method = "curl",
        quiet = TRUE,
        cacheOK = FALSE
)

dateDownloaded <- date()

library(tools)
.print.via.format(sprintf("File %s downloaded %s", # # needs tool-packages
                        list.files("./DATA"), dateDownloaded))

cameraData.csv <- import("./DATA/cameras.csv") # uses rio-packages


# ---------------- Load Excel with different sheets  ---------------------------
library(readxl)
if (!file.exists("./DATA/PIAAC_Test.xlsx")) {
        stop(message("No such file"))
}
# To load all sheets in a workbook, use lapply
myPath <- "./DATA/PIAAC_Test.xlsx"
df <- lapply(excel_sheets(myPath), read_excel, path = myPath)

# -----------------------------------------------------------------------------
library(XML)
fileUrl <- "http://www.w3schools.com/xml/simple.xml"
doc <- xmlTreeParse(fileUrl, useInternalNodes = TRUE) # differs from lecture!
rootNode <- xmlRoot(doc)
xmlName(rootNode)
names(rootNode)
xpathSApply(rootNode,"//name",xmlValue)
xpathSApply(rootNode,"//price",xmlValue)
# Another example with HTML
# Website: http://www.espn.com/nfl/team/_/name/bal/baltimore-ravens
fileUrl <- "http://espn.go.com/nfl/team/_/name/bal/baltimore-ravens"
doc <- htmlTreeParse(fileUrl, useInternalNodes = TRUE)
scores <- xpathSApply(doc, "//div[@class='score']", xmlValue)
teams <- xpathSApply(doc, "//div[@class='game-info']", xmlValue)
scores
teams

