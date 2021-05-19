#---
# Registrations since 2019 by Country of Origin of companies
# Y. Galanakis; <i.galanakis@kent.ac.uk>
# May 2021
#---

# Packages ----

packages <- c('tidyverse', 'naniar', 'haven', 'survey', 'ggpubr', 'latex2exp',
              'data.table', 'lubridate', 'ggalt', 'cowplot','animation',
              'patchwork', 'sp', 'scales', 'raster', 'rgeos', 'mapproj', 'zoo',
              'rgdal', 'maptools', 'emojifont', 'nord', 'paletteer', 'stringr','plotly')
pkg_notinstall <-  packages[!(packages %in% installed.packages()[,"Package"])]

lapply(pkg_notinstall, install.packages, dependencies = TRUE)
lapply(packages, library, character.only = TRUE)

# ReadData ----
# Create a temp. file
temp <- tempfile()
# Use `download.file()` to fetch the file into the temp. file
download.file("http://download.companieshouse.gov.uk/BasicCompanyDataAsOneFile-2021-05-01.zip",temp)
# Use unz() to extract the target file from temp. file
df<- read.csv(unz(temp, "BasicCompanyDataAsOneFile-2021-05-01.csv"))
# Remove the temp file via 'unlink()'
unlink(temp)
# Make incorporation date as date format.
df$IncorporationDate <- as.Date(df$IncorporationDate, "%d/%m/%Y")

# Countries donut ----
n_incorpC <- df %>%
  filter(IncorporationDate >= "2019-01-01") %>%
  group_by(CountryOfOrigin) %>%
  count()

library(stringr)
# keep all letters small apart from the first of each word
n_incorpC$Country <- str_to_title(n_incorpC$CountryOfOrigin)
# aggregate by country
n_incorpC <- aggregate(n_incorpC$n, by = list(n_incorpC$Country), FUN="sum")
n_incorpC <- n_incorpC %>% rename(Country = Group.1)
# if no name of country, assign NA and drop it.
n_incorpC$Country[n_incorpChina$Country==" "] <- NA
n_incorpC <- n_incorpC %>% drop_na(Country)

nonUK <- plot_ly(alpha = 0.5, n_incorpC, labels = ~Country, values = ~x,
        textposition='inside') %>%
  add_pie(hole = 0.6) 
write.csv(n_incorpC, "data/CountryOfOrigin.csv", row.names = F)

# Countries Sunburst ----

# keep only the first 1 or 2 letters before the numbers in the Postcode
df$postcodeArea <- sub("^([[:alpha:]]*).*", "\\1", df$RegAddress.PostCode)

n_incorp <-df %>%
  filter(IncorporationDate >= "2019-01-01") %>%
  group_by(postcodeArea, CountryOfOrigin) %>%
  count()

# keep all letters small apart from the first of each word
n_incorp$Country <- str_to_title(n_incorp$CountryOfOrigin)
# aggregate by country
n_incorp <- aggregate(n_incorp$n, by = list(n_incorp$Country, n_incorp$postcodeArea), FUN="sum")
n_incorp <- n_incorp %>% rename(Country = Group.1)
n_incorp <- n_incorp %>% rename(postcodeArea = Group.2)
# if no name of country, assign NA and drop it.
n_incorp$Country[n_incorp$Country==" "] <- NA
n_incorp <- n_incorp %>% drop_na(Country)

n_incorp$UK[n_incorp$Country=="United Kingdom"] <- "UK"
n_incorp$UK[n_incorp$Country!="United Kingdom"] <- "non-UK"

# assing UK contries based on postcode
postcod2country <- read.csv("data/convertedPC2country.csv")
postcod2country <- postcod2country[-c(1)]
postcod2country <- postcod2country %>% rename (postcodeArea= Postcode.area)
postcod2country <- postcod2country %>% rename (UKcountry = Country)

full <- left_join(n_incorp, postcod2country, by = "postcodeArea")
full <- full %>%
  distinct(Country, postcodeArea, .keep_all = TRUE)

# create one universal column with countries
full$countryAll <- ifelse(full$UK=="UK", full$UKcountry, full$Country)
# if we don't know the country of UK, I assign it as "UK; NA"
full$countryAll[is.na(full$countryAll)] <- "UK; NA"


full_minus <- full[-c(1:2, 5)] # drop unnessary columns

# Use the function as.sunburstDF from https://stackoverflow.com/a/58481176/4874341
as.sunburstDF <- function(DF, valueCol = NULL){
  require(data.table)
  
  colNamesDF <- names(DF)
  
  if(is.data.table(DF)){
    DT <- copy(DF)
  } else {
    DT <- data.table(DF, stringsAsFactors = FALSE)
  }
  
  DT[, root := "Total"]
  colNamesDT <- names(DT)
  
  if(is.null(valueCol)){
    setcolorder(DT, c("root", colNamesDF))
  } else {
    setnames(DT, valueCol, "values", skip_absent=TRUE)
    setcolorder(DT, c("root", setdiff(colNamesDF, valueCol), "values"))
  }
  
  hierarchyCols <- setdiff(colNamesDT, "values")
  hierarchyList <- list()
  
  for(i in seq_along(hierarchyCols)){
    currentCols <- colNamesDT[1:i]
    if(is.null(valueCol)){
      currentDT <- unique(DT[, ..currentCols][, values := .N, by = currentCols], by = currentCols)
    } else {
      currentDT <- DT[, lapply(.SD, sum, na.rm = TRUE), by=currentCols, .SDcols = "values"]
    }
    setnames(currentDT, length(currentCols), "labels")
    hierarchyList[[i]] <- currentDT
  }
  
  hierarchyDT <- rbindlist(hierarchyList, use.names = TRUE, fill = TRUE)
  
  parentCols <- setdiff(names(hierarchyDT), c("labels", "values", valueCol))
  hierarchyDT[, parents := apply(.SD, 1, function(x){fifelse(all(is.na(x)), yes = NA_character_, no = paste(x[!is.na(x)], sep = ":", collapse = " - "))}), .SDcols = parentCols]
  hierarchyDT[, ids := apply(.SD, 1, function(x){paste(x[!is.na(x)], collapse = " - ")}), .SDcols = c("parents", "labels")]
  hierarchyDT[, c(parentCols) := NULL]
  return(hierarchyDT)
}

# my sunburstDF
sunburstDF <- as.sunburstDF(full_minus, valueCol = "x")

sun<-plot_ly(alpha = 0.5, data = sunburstDF, ids = ~ids, labels= ~labels, 
        parents = ~parents, values= ~values, type='sunburst', 
        branchvalues = 'total',
        textinfo = "label+parent+percent parent+entry+value")
