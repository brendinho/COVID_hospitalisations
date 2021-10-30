rm(list=ls())

library(data.table)
library(dplyr)
library(ggplot2)

PROJECT_FOLDER <- "/home/bren/Documents/GitHub/COVID_hospitalisations/"
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
        # na.omit %>%
        dplyr::mutate(
            filenumber = file %>% strsplit(., '\\(') %>% .[[1]] %>% .[2] %>% strsplit(., '\\)') %>% .[[1]] %>% .[1] %>% as.numeric,
            date_report = as.Date(date_report)
        )
    
    collection <- rbind(collection, temp)
}

# stop()

collection <- collection %>%
    dplyr::select(-filenumber) %>%
    dplyr::arrange(date_report) %>%
    rbind(., list(as.Date("2020-09-04"), 0.12, 0.34, 0.48, 0.70, 1.01, 1.61, 2.96, 1.17)) %>%
    rbind(., list(as.Date("2020-09-05"), 0.12, 0.34, 0.48, 0.70, 1.01, 1.61, 3.03, 1.14)) %>%
    rbind(., list(as.Date("2020-10-07"), 0.12, 0.38, 0.56, 0.83, 1.43, 2.09, 4.14, 1.26)) %>%
    rbind(., list(as.Date("2020-11-09"), 0.18, 0.62, 1.00, 1.68, 2.67, 4.14, 8.89, 2.46)) %>%
    rbind(., list(as.Date("2020-12-11"), 0.26, 0.99, 1.66, 2.67, 4.63, 7.47, 16.4, 4.20)) %>%
    rbind(., list(as.Date("2020-12-12"), 0.26, 1.01, 1.66, 2.69, 4.71, 7.59, 16.59, 4.25)) %>%
    rbind(., list(as.Date("2020-12-13"), 0.26, 1.01, 1.68, 2.74, 4.78, 7.67, 16.73, 4.29)) %>%
    rbind(., list(as.Date("2021-01-16"), 0.27, 1.07, 1.80, 2.97, 5.08, 8.21, 18.19, 4.63)) %>%
    rbind(., list(as.Date("2021-01-17"), 0.27, 1.06, 1.80, 2.92, 5.04, 8.12, 17.94, 4.58)) %>%
    rbind(., list(as.Date("2021-02-17"), 0.21, 0.65, 1.01, 1.50, 2.50, 3.80, 7.50, 2.13)) %>%
    rbind(., list(as.Date("2021-02-18"), 0.21, 0.63, 1.00, 1.47, 2.43, 3.70, 7.25, 2.07)) %>%
    rbind(., list(as.Date("2021-03-22"), 0.15, 0.52, 0.86, 1.26, 2.02, 2.56, 3.85, 1.40)) %>%
    rbind(., list(as.Date("2021-03-23"), 0.15, 0.51, 0.86, 1.26, 2.03, 2.61, 3.82, 1.41)) %>%
    rbind(., list(as.Date("2021-04-07"),   NA, 0.64,   NA, 1.66,   NA,   NA,   NA,   NA)) %>%
    rbind(., list(as.Date("2021-04-25"), 0.18, 0.68, 1.15, 1.63, 2.45, 2.76, 3.82, 1.58)) %>%
    rbind(., list(as.Date("2021-04-26"), 0.18, 0.68, 1.14, 1.61, 2.39, 2.71, 3.75, 1.55)) %>%
    rbind(., list(as.Date("2021-05-28"), 0.13, 0.43, 0.68, 0.89, 1.23, 1.42, 2.04, 0.89)) %>%
    rbind(., list(as.Date("2021-06-16"), 0.09, 0.31, 0.45, 0.65, 0.79, 0.92, 1.34, 0.60)) %>%
    rbind(., list(as.Date("2021-07-02"), 0.08, 0.31, 0.49, 0.60, 0.82, 0.95, 1.35, 0.60)) %>%
    rbind(., list(as.Date("2021-08-04"), 0.27, 1.07, 2.05, 2.88, 3.51, 3.80, 5.35, 2.44)) %>%
    rbind(., list(as.Date("2021-09-06"), 0.50, 1.35, 2.73, 3.77, 4.98, 6.03, 8.61, 3.61)) %>%
    rbind(., list(as.Date("2021-10-09"), 0.27, 0.75, 1.37, 1.98, 2.72, 3.66, 5.91, 2.10)) %>%
    dplyr::filter(rowSums(is.na(.)) != ncol(.)) %>%
    unique()

fwrite(collection %>% dplyr::rename_with(\(x) gsub(' ','_', x)), "all_the_data.csv")
    
the_plot <- ggplot(
        melt(collection, id.vars="date_report"), 
        aes(y=value, x=date_report, group=variable, colour=variable)
    ) + 
    geom_line(size=1) +
    scale_x_date(date_breaks = "1 month") +
    theme_bw() +
    theme(
        axis.text = element_text(size=13),
        axis.text.x = element_text(angle=45, hjust=1),
        axis.title = element_text(size=14),
        legend.text = element_text(size=13),
        legend.title = element_text(size=14)
    ) +
    labs(x="Date", y="Hospitalisations per 100,000") +
    # scale_x_date(breaks = seq(min(collection$date_report), max(collection$date_report), by="1 month")) +
    # scale_y_continuous(expand = c(0,0)) +
    guides(colour=guide_legend(title="Age Cohort"))
    ggsave(the_plot, file="hospitalisations_per_100000.png", width=12, height=7)
    
    
# all_dates <- seq(min(haha$date_report), max(haha$date_report), by=1)
# print(all_dates[! all_dates %in% haha$date_report])
    

