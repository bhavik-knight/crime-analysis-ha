library(reshape)
library(tidyverse)
library(ggplot2)
library(sf)
library(tmap)
library(leaflet)
library(dplyr)
library(tmaptools)

# Reading main data
frame <- read.csv("crime_unemployment_1976_2014.csv")
frame %>% drop_na() %>%
    select(
        year,
        state,
        unemployment,
        Population
    ) %>%
    filter(
        year %in% 1980:2014
    ) -> frame2

# Grouping unemployment by the year
frame2 %>% group_by(year) %>%
    summarise_each(funs(mean), unemployment:Population) -> frame3


frame2 %>% group_by(state) %>%
    summarise_each(funs(mean), unemployment:Population) -> state_unemployment

# Plotting
ggplot(frame3, aes(x=year, y=unemployment))+
    geom_point() + geom_line() + theme_classic() +
    ylab("unemployment_rate") + xlab("years (1980-2014)") +
    labs(title="Unemployment rate between 1980 and 2014")

ggplot(state_unemployment, aes(state, unemployment))+
    geom_bar(aes(fill=state), stat="identity", color="black") +
    ylab("unemployment_rate") + xlab("states") +
    labs(title="Unemployment rate state-wise")


# Loading Maps
options(scipen=999)
my_map <- st_read("shapefiles/cb_2018_us_state_500k.shp", stringsAsFactors = FALSE)
# str(my_map)

# Mapping
map_and_data <- inner_join(my_map, frame2, by = c("STUSPS"="state"))
# str(map_and_data)

# ggplot(map_and_data)+geom_sf(aes(fill=unemployment))
ggplot(map_and_data)+geom_sf(aes(fill=unemployment))+
    scale_fill_gradient(low="#56B1F7", high="#132B43") + theme_classic() +
    coord_sf(xlim=c(-180, -45), ylim=c(20,80), expand=FALSE) +
    xlab("latitude") + ylab("longitude") +
    labs(title="Unemployment Rate by States", fill="unemployment_rate")


data <- read.csv("education_report_not_completing_HS.csv")

# Selecting coloumns and eliminating few rows
data <-  select(data,X:X.5)
frame <- data[c(4:54),]
names(frame) <- c("states","1970-1979","1980-1989","1990-1999","2000-2014","2015-2019")

# Splitting "%"
frame$`1970-1979` <- str_split_fixed(frame$`1970-1979`,"%",2)[,1]
frame$`1980-1989` <- str_split_fixed(frame$`1980-1989`,"%",2)[,1]
frame$`1990-1999` <- str_split_fixed(frame$`1990-1999`,"%",2)[,1]
frame$`2000-2014` <- str_split_fixed(frame$`2000-2014`,"%",2)[,1]
# frame$`2015-2019` <- str_split_fixed(frame$`2015-2019`,"%",2)[,1]

# Converting char to int
frame$`1970-1979` <- as.numeric(as.character(frame$`1970-1979`))
frame$`1980-1989` <- as.numeric(as.character(frame$`1980-1989`))
frame$`1990-1999` <- as.numeric(as.character(frame$`1990-1999`))
frame$`2000-2014` <- as.numeric(as.character(frame$`2000-2014`))
# frame$`2015-2019` <- as.numeric(as.character(frame$`2015-2019`))

#Adding Columns
#frame$decade1 = 1970
#frame$decade2 = 1980
#frame$decade3 = 1990
#frame$decade4 = 2000
#frame$decade5 = 2015

# Manipulating frame
rowSums(frame[2:6])-> frame$decade_sum
frame %>% select(decade_sum, everything()) -> aframe
aframe[, 2:1] -> dec_state
frame[, 2:7] %>% melt(id="decade_sum", value.name="type") -> decades
inner_join(dec_state, decades) -> new_frame
new_frame[-c(17,19,21,23,25),]->new_fframe
new_fframe[-c(207,209,211,213,215),]->curr_frame

# summarise
curr_frame %>% group_by(variable) %>%
    summarise(mean = mean(value),n = n())->mean_frame

# Plotting
ggplot(mean_frame, aes(variable, mean)) +
geom_bar(aes(fill=variable), stat="identity", color="black") +
labs(title = "High School Drop-out Rates") +
xlab("years (1908-2014)") + ylab("Percentage of Drop-outs") +
theme_classic()
