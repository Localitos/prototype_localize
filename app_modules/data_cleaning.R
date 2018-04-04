setwd("~/GitHub/localize/app_modules/")
rm(list = ls())

source("func.R")

# df <- read.csv("stats.csv", stringsAsFactors=FALSE)
# visa.general <- read.csv("visageneral.csv", stringsAsFactors=FALSE)
# visa.documents <- read.csv("visadocuments.csv", stringsAsFactors=FALSE)
# cluster.df <- read.csv("cluster.csv", stringsAsFactors=FALSE)
# countries.list <- cluster.df %>%
#   filter(!origin == "EU/EEA") %>% select(origin)
#
# # define neighborhoods to keep
# keep <- c(
#   'Alsterdorf',
#   'Altona-Altstadt',
#   'Altona-Nord',
#   'Bahrenfeld',
#   'Barmbek-Nord',
#   'Barmbek-Süd',
#   'Billstedt',
#   'Blankenese',
#   'Eidelstedt',
#   'Eilbek',
#   'Eimsbüttel',
#   'Eppendorf',
#   'Fuhlsbüttel',
#   'HafenCity',
#   'Hamburg-Altstadt',
#   'Hamm',
#   'Harvestehude',
#   'Hoheluft-Ost',
#   'Hoheluft-West',
#   'Hummelsbüttel',
#   'Langenhorn',
#   'Lokstedt',
#   'Marienthal',
#   'Niendorf',
#   'Ohlsdorf',
#   'Ottensen',
#   'Poppenbüttel',
#   'Rahlstedt',
#   'Rotherbaum',
#   'Schnelsen',
#   'St. Georg',
#   'St. Pauli',
#   'Steilshoop',
#   'Stellingen',
#   'Sternschanze',
#   'Uhlenhorst',
#   'Wandsbek',
#   'Winterhude')
#
# df <- df %>% filter(hood %in% keep)
# district <- as.data.frame(read.csv("district_desrcription.csv", sep=";", stringsAsFactors = F))
# df <- df %>% left_join(., district, by="hood")
#
# districts <- paste("Hamburg", df$hood, sep = ",")
#
# # create empty list
# mylist <- list()
#
# for (i in 1:length(districts)) {
#   mylist[[i]] <- bb_lookup(districts[i])
# }
#
# # Convert to dataframe
# districts_bb <- do.call("rbind", mylist)
#
# # Filter ...
# districts_bb %>%
#   # keep only class "boundary"
#   filter(class == "boundary") %>%
#   # keep Eimsbüttel city-district
#   filter(!osm_id==30243) %>%
#   # keep Wandsbek
#   filter(!display_name=="Wandsbek, Hamburg, Deutschland") %>%
#   # generate new variable containing only the district name
#   mutate(hood = gsub(",.*","", display_name)) %>%
#   mutate(hood = gsub(" ","", hood)) -> districts_bb
#
# df <- df %>%
#   left_join(., districts_bb %>%
#               mutate(lat = as.numeric(lat),
#                      long = as.numeric(lon)) %>%
#               select(lat, long, bottom, top, left, right, hood) ,
#             by="hood")

# save(cluster.df, countries.list, df, visa.documents, visa.general,
#      file = "app_data.Rda")