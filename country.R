#inspired by 
# https://jkunst.com/highcharter/articles/maps.html#categorized-areas-1
#https://statsandr.com/blog/world-map-of-visited-countries-in-r/

library(highcharter)
library(dplyr)
library(webshot)
library(maps)



country <- read.delim("country_vaccine.txt",header=TRUE,check.names = F)
head(country)
dim(country)

#After having loaded the packages, we are going to use the dataset called iso3166 from the {maps} package and rename it dat. 
dat <- iso3166
head(dat)
#write.csv(dat, "country_list.txt")

#We rename the variable a3 by iso-a3:
dat <- rename(dat, "iso-a3" = a3)
head(dat)  

#filtered out the visted country
dat$visited <- ifelse(dat$`ISOname` %in% country$country, 1, 0)
head(dat)

visits <- subset(dat, dat$visited >= 1)
dim(visits)
# add INDIA
ind <- c("IN","IND", "India","India","India","2")
dat <- rbind(dat, ind)
dat <- subset(dat, dat$visited >= 1)

sort(dat$visited) # sort is to have the visited countries in alphabetical order
head(dat)
dim(dat)
#write.csv(dat, "visited.txt")

# now we'll create the dataClasses
  dta_clss <- dat %>% 
   mutate(value = cumsum(!duplicated(visited))) %>% 
  group_by(visited) %>% 
  summarise(value = unique(visited)) %>% 
  arrange(visited) %>% 
  rename(name = visited, from = visited) %>% 
    list_parse()


plot <- hcmap(
   map = "custom/world-highres3", # high resolution world map
  data = dat, 
  joinBy = "iso-a3",
  #joinBy = c("iso-a3","a2"),
  #name = "Time zone",
  #nullColor = "#DADADA",
  value = "visited",
  tooltip = list(pointFormat = "{point.name} {point.visited}")
  #dataLabels = list(enabled = TRUE, format = "{point.a2}")
) %>%
  hc_colorAxis(
    dataClassColor = "visited",
    dataClasses = dta_clss,
    minColor = "green",
    maxColor = "orange"
  ) %>% 
  hc_title(text = paste(
    "Total: ",
    (nrow(dat)-3), # some bug here #### change the number
    " countries"
  ))%>%
  hc_legend("none")

htmlwidgets::saveWidget(widget = plot, file = "plot.html")
webshot::webshot(url = "plot.html", 
                 file = "plot.png")

webshot::install_phantomjs()


