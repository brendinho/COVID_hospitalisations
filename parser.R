rm(list=ls())

library(data.table)
library(dplyr)

PROJECT_FOLDER <- "/home/bren/Dropbox/COVID_hospitalisation/"
setwd(PROJECT_FOLDER)

collection <- data.table()

for(file in Sys.glob(file.path(PROJECT_FOLDER, "*clean*")))
{
    temp <- fread(file, check.names=TRUE) %>%
        dplyr::rename(
            date_report = names(.)[1],
            "under 18" = names(.)[2],
            "18 to 29" = names(.)[3],
            "30 to 39" = names(.)[4],
            "40 to 49" = names(.)[5],
            "50 to 59" = names(.)[6],
            "60 to 69" = names(.)[7],
            "70 and older" = names(.)[8],
            "all ages" = names(.)[9],
        ) %>%
        na.omit %>%
        dplyr::mutate(
            filenumber = file %>% strsplit(., '\\(') %>% .[[1]] %>% .[2] %>% strsplit(., '\\)') %>% .[[1]] %>% .[1] %>% as.numeric,
            date_report = as.Date(date_report)
        )
    
    collection <- rbind(collection, temp)
    
    # stop()
}

# collection <- 
haha <- collection %>%
    dplyr::select(-filenumber) %>%
    dplyr::arrange(date_report) %>%
    unique %>%
    rbind(., list(as.Date("2020-09-04"), 0.12, 0.34, 0.48, 0.70, 1.01, 1.61, 2.96, 1.17)) %>%
    rbind(., list(as.Date("2020-09-05"), 0.12, 0.34, 0.48, 0.70, 1.01, 1.61, 3.03, 1.14)) %>%
    rbind(., list(as.Date("2020-10-07"), 0.12, 0.38, 0.56, 0.83, 1.43, 2.09, 4.14, 1.26)) %>%
    rbind(., list(as.Date("2020-11-09"), 0.18, 0.62, 1.00, 1.68, 2.67, 4.14, 8.89, 2.46))


all_dates <- seq(min(haha$date_report), max(haha$date_report), by=1)

print(all_dates[! all_dates %in% haha$date_report])