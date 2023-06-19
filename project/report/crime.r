###############################################################################
# Name:             Bhavik Bhagat, Edwin Chacko
# Email:            x2020coq@stfx.ca, x2020gff@stfx.ca
# Student ID:       202002911, 202005941
# Course:           csci225, project: Crime Analysis
# System:           win11, R4.1.1, Rstudio1.4.1717
################################################################################


################################################################################
# load required libraries
library(tidyverse)
library(reshape)
library(modelr)
library(dplyr)
library(splines)
library(rmarkdown)
library(sf)
library(tmap)
library(leaflet)
library(tmaptools)
library(wesanderson)
options(warn=-1)
################################################################################


################################################################################
# load data sets
# crime_us dataset
# load crime_us_1975_2015 data
raw_crime_us <- read.csv("data/crime_us_1975_2015.csv")

# load unemployment data
raw_unemployment <- read.csv("data/crime_unemployment_1976_2014.csv")

# load homicide data
raw_homicide <- read.csv(
    "data/crime_us_homicide_1980_2014.csv",
    stringsAsFactors=FALSE,
    na.strings=c("NA", "N/A", "Uknown*", "NULL", ".P")
)

# load high school dropout data
raw_dropout <- read.csv("data/highschool_dropout.csv")
################################################################################


################################################################################
# data wrangling
# drop samples with missing values
# add new column if required
# select desired column
# filter unwanted samples
################################################################################

###############################################################################
# crime_us
raw_crime_us %>% drop_na() %>%
    mutate(
        alpha_code = str_split_fixed(agency_jurisdiction, ", ", n = 2)[, 2]
    ) %>%
    select(
        year = report_year,
        alpha_code,
        population,
        violent_crimes,
        homicides,
        rapes,
        assaults,
        robberies
    ) %>%
    filter(year %in% 1980:2009) %>%
    arrange(alpha_code) -> crime_us
#################################################################################

###############################################################################
# crime_homicide
# format the data frame in desired format
as.data.frame(
    raw_homicide %>% drop_na() %>%
    filter(
        Year %in% 1980:2009,
        Victim.Race != "Unknown",
        Victim.Sex != "Unknown",
        Perpetrator.Race != "Unknown",
        Perpetrator.Sex != "Unknown",
        Weapon != "Unknown",
    ) %>%
    select(
        Year,
        Victim.Sex:Victim.Race,
        Perpetrator.Sex:Perpetrator.Race,
        Weapon
    )
) -> homicides
################################################################################

################################################################################
# unemployment
# make column names lower case to be compatible with other data sets
names(raw_unemployment) <- tolower(names(raw_unemployment))

as.data.frame(
    raw_unemployment %>% drop_na() %>% group_by(year) %>%
        summarise_at(vars(unemployment), sum) %>%
    filter(year %in% 1980:2009)
) -> unemployment_us

as.data.frame(
    raw_unemployment %>%
    drop_na() %>%
    select(year, state, unemployment) %>%
    filter(year %in% 1980:2014)
) -> unemployment_us_by_states
################################################################################

################################################################################
# Selecting columns and eliminating few rows
raw_dropout[c(4:54), ] %>%
    select(X:X.5) -> dropouts
names(dropouts) <- c("states","1970-1979","1980-1989","1990-1999","2000-2014","2015-2019")
################################################################################

################################################################################
# summarize by years, states, but need a data frame, not tibble
as.data.frame(
    crime_us  %>%
    group_by(year) %>%
    summarise_each(funs(sum), population:robberies)
) -> crime_by_years

as.data.frame(
    raw_homicide %>%
    group_by(Year) %>%
    summarise_each(
        funs(mean), Victim.Age, Perpetrator.Age
    )
) -> homicides_victim_perpetrator_age
################################################################################

################################################################################
# helper functions
# crime rate is crime reported per 100,000 population
get_crime_rate <- function(reported_crimes, total_population) {
    crime_rate <- (reported_crimes / total_population) * 10^5
    crime_rate
}

# to scale the dataset use this function
get_normal <- function(x) {
    mu <- mean(x)
    std <- sd(x)
    score <- ((x) - mu)^2 / std
    score
}

# to convert a given year to its decade
get_decade <- function(x) {
    x - x %% 10
}
################################################################################


################################################################################
# further processing for midterm and final plots
################################################################################
# for crime-rate by the years
as.data.frame(
    crime_by_years %>%
    melt(id=c("year", "population", "violent_crimes")) %>%
    transmute(
        year,
        rate_violent_crimes=get_crime_rate(violent_crimes, population),
        crime_type=variable,
        crime_recorded=value,
        rate_crime_type=get_crime_rate(crime_recorded, population)
    )
) -> years_crime_rate

# normalized crime_rate by years
as.data.frame(years_crime_rate %>% transmute(
    year,
    rate_violent_crimes = get_normal(rate_violent_crimes),
    crime_type = crime_type,
    rate_crime_type = get_normal(log(rate_crime_type)) # check w/ log
)) -> normalized_years_crime_rate
################################################################################

################################################################################
# make decade a factor having 10 years, crime_us by years
# also summarise the data
as.data.frame(
    crime_us  %>%
        group_by(year) %>%
        summarise_at(
            .vars=c("population", "violent_crimes", "homicides",
            "rapes", "assaults", "robberies"),
            .funs=sum
        ) %>%
        mutate(decade=factor(get_decade(year)))
) -> crime_us_by_years
###############################################################################

###############################################################################
# join the crime_us and unemployment datasets on year, state (alpha_code)
merge(x = crime_us_by_years, y = unemployment_us, by="year") -> crime_unemployment
################################################################################

###############################################################################
# reshape data set to create violent crime as a factor of other crimes
# divide dataset into two parts
crime_unemployment %>% melt.data.frame (
    id.vars=c(
        "year",
        "decade",
        "population",
        "unemployment",
        "violent_crimes"
    ),
    variable_name="crime_type"
) -> crime_unemployment
###############################################################################

################################################################################
# convert crime number into crime rates
crime_unemployment %>% transmute(
    year, decade, population, unemployment_rate = unemployment,
    violent_crimes = get_crime_rate(violent_crimes, population),
    crime_rate = get_crime_rate(value, population), crime_type
) -> crime_unemployment_rate
################################################################################

################################################################################
# normalize the dataset
crime_unemployment_rate %>% transmute(
    year, decade, unemployment_rate = get_normal(unemployment_rate),
    violent_crimes = get_normal(violent_crimes),
    crime_rate = get_normal(crime_rate), crime_type
) -> scaled_crime_unemployment_rate
################################################################################

################################################################################
# processing for geomap
# Loading US Map
options(scipen=999)
us_map <- st_read("shapefiles/cb_2018_us_state_500k.shp", stringsAsFactors = FALSE)

# merge geographic map data and unemployment data of US
geomap_data <- inner_join(us_map, unemployment_us_by_states, by = c("STUSPS"="state"))
################################################################################

################################################################################
# processing for dropout
# Splitting "%"
dropouts$`1970-1979` <- str_split_fixed(dropouts$`1970-1979`,"%",2)[,1]
dropouts$`1980-1989` <- str_split_fixed(dropouts$`1980-1989`,"%",2)[,1]
dropouts$`1990-1999` <- str_split_fixed(dropouts$`1990-1999`,"%",2)[,1]
dropouts$`2000-2014` <- str_split_fixed(dropouts$`2000-2014`,"%",2)[,1]
dropouts$`2015-2019` <- str_split_fixed(dropouts$`2015-2019`,"%",2)[,1]

# Converting char to int
dropouts$`1970-1979` <- as.numeric(as.character(dropouts$`1970-1979`))
dropouts$`1980-1989` <- as.numeric(as.character(dropouts$`1980-1989`))
dropouts$`1990-1999` <- as.numeric(as.character(dropouts$`1990-1999`))
dropouts$`2000-2014` <- as.numeric(as.character(dropouts$`2000-2014`))
dropouts$`2015-2019` <- as.numeric(as.character(dropouts$`2015-2019`))

# data processing
dropouts %>% mutate(decade_sum=rowSums(dropouts[2:6])) %>%
    melt(
        id=c("states", "decade_sum"),
        variable.name=decades,
        value.name=dropout_rate
    ) -> dropouts
names(dropouts) <- c("states", "decade_sum", "decades", "dropout_rate")
################################################################################
# all pre-processing for mid, final complete
################################################################################


################################################################################
# mid-term plot revisited (making boxplot to give more info)
################################################################################
# plot several crimes over the year
ggplot(normalized_years_crime_rate, aes(year, rate_crime_type)) +
    geom_line(aes(color=crime_type)) +
    geom_point(aes(color=crime_type), shape = 15) +
    labs(
        title="Crime Rate for various types of Violent Crimes",
        x="years (1980-2014)",
        y="normalized_crime_rate (log-scale)"
    ) + theme_bw()
################################################################################

################################################################################
# boxplot of crime-rate of different types of crimes by decades
crime_unemployment_rate %>%
    ggplot(aes(x=decade, y=unemployment_rate)) +
    stat_boxplot(geom="errorbar", width=0.2) +
    geom_boxplot(aes(fill=decade), outlier.shape=NA, width=0.2) +
    theme_bw() +
    ylim(200, 400) +
    labs(
        title="Unemployment rate is decreasing by the decade.",
        fill="decades",
        x="decades",
        y="unemployment_rate (per capita)"
    )
################################################################################

################################################################################
# boxplot of unemployment by decades
crime_unemployment_rate %>%
    ggplot(aes(x=decade, y=crime_rate)) +
    stat_boxplot(geom="errorbar", width=0.4) +
    geom_boxplot(aes(fill=decade), outlier.shape=NA, width=0.4) +
    facet_wrap(.~crime_type, scales="free_y", nrow=1) +
    theme_bw() +
    labs(
        title="Trend of various type of crimes by decades.",
        fill="decades",
        x="decades",
        y="crime_rate (per capita)"
    )
################################################################################

################################################################################
# plotting geo-map
geomap_data %>%
    ggplot() + geom_sf(aes(fill=unemployment)) +
    scale_fill_gradient(low="#56B1F7", high="#132B43") + theme_bw() +
    coord_sf(xlim=c(-180, -45), ylim=c(20,80), expand=FALSE) +
    labs(
        title="Unemployment Rate by States",
        fill="unemployment_rate",
        x="latitude",
        y="longitude"
    )
################################################################################

################################################################################
# high school dropout plot
dropouts %>%
    ggplot(aes(decades, dropout_rate)) +
    geom_bar(aes(fill=decades), stat="identity") +
    labs(
        title="High school dropout rate is decreasing",
        x="years (1980-2019)",
        y="% of dropouts"
    ) + theme_bw()
################################################################################

################################################################################
# perpetrator's age, to analyze wrt education dropout_rate
homicides_victim_perpetrator_age %>%
    ggplot(aes(Year, Perpetrator.Age)) +
    geom_bar(aes(fill=Perpetrator.Age, ), stat="identity", color="darkblue") +
    labs(title="Perpetrator average age by years") + theme_bw()
################################################################################


################################################################################
# final presentation
################################################################################
# count the number of victim, perpetrator by race, sex
homicides %>%
    count(Victim.Race, Victim.Sex, Perpetrator.Race, Perpetrator.Sex) ->
    homicides_plot

# Plotting heatmap for Victim Sex and Race
homicides_plot %>%
    group_by(Victim.Race, Victim.Sex) %>%
    summarise_at("n", sum) %>%
    ggplot(aes(Victim.Race, Victim.Sex)) +
    geom_tile(aes(fill= n)) + theme_bw() +
    labs(title="Victim's Sex vs Race",
         x= "Victim's Race",
         y="Victim's Sex",
         fill= "Count") +
    scale_shape_manual(values=c(3, 16, 17)) +
    scale_color_manual(values=c('#999999','#E69F00', '#56B4E9'))


# Plotting heatmap for Perpetrator Sex and Race
homicides_plot %>%
    group_by(Perpetrator.Race, Perpetrator.Sex) %>%
    summarise_at("n", sum) %>%
    ggplot(aes(Perpetrator.Race, Perpetrator.Sex)) +
    geom_tile(aes(fill= n)) + theme_bw() +
    labs(title="Perpetrator's Sex vs Race",
         x= "Perpetrator's Race",
         y="Perpetrator's Sex",
         fill = "Count",
     ) +
    scale_shape_manual(values=c(3, 16, 17)) +
    scale_color_manual(values=c('#999999','#E69F00', '#56B4E9'))


# Plotting bar chart for weapons and victim sex
homicides %>%
    select(Weapon, Victim.Sex) %>%
    group_by(Weapon) %>%
    count(Victim.Sex) -> weapon_victim

weapon_victim %>%
    ggplot(aes(x=Weapon, y=n)) +
    geom_bar(aes(fill=Victim.Sex), stat="identity", position="fill") +
    labs(title = "Weapons Used",
         x= "Weapons",
         y="Victim Rate (female vs male)",
         fill = "Victim's Sex"
    ) +
    coord_flip() + theme_bw()


weapon_victim %>% filter(Weapon %in% c("Suffocation", "Strangulation")) %>%
    ggplot(aes(x= Weapon, y=n)) +
    geom_bar(aes(fill=Victim.Sex), stat="identity", position="dodge") +
    labs(title = "Weapons Used",
         x= "Weapons",
         y="Count",
         fill = "Victim's Sex"
    ) + theme_bw()
################################################################################


################################################################################
# model the unemployment and crime rate
# divide dataset into 3 decades to build robust models according to decades
crime_unemployment_rate %>% filter(year==1980:1989) -> data_80
crime_unemployment_rate %>% filter(year==1990:1999) -> data_90
crime_unemployment_rate %>% filter(year==2000:2009) -> data_00


# try to generate different models, linear, non-linear
lm(violent_crimes ~ unemployment_rate, data_80) -> linear_80
lm(violent_crimes ~ unemployment_rate, data_90) -> linear_90
lm(violent_crimes ~ unemployment_rate, data_00) -> linear_00

lm(violent_crimes ~ ns(unemployment_rate, 3), data_80) -> spline3_80
lm(violent_crimes ~ ns(unemployment_rate, 3), data_90) -> spline3_90
lm(violent_crimes ~ ns(unemployment_rate, 3), data_00) -> spline3_00


lm(violent_crimes ~ log(unemployment_rate), data_80) -> log_80
lm(violent_crimes ~ log(unemployment_rate), data_90) -> log_90
lm(violent_crimes ~ log(unemployment_rate), data_00) -> log_00


data_80 %>%
    data_grid(unemployment_rate=seq_range(unemployment_rate, n=100))%>%
    gather_predictions(linear_80, spline3_80, log_80,
                       .pred="predicted_violent_crimes") %>%
    mutate(decade=1980) -> grid_80

data_90 %>%
    data_grid(unemployment_rate=seq_range(unemployment_rate, n=100))%>%
    gather_predictions(linear_90, spline3_90, log_90,
                       .pred="predicted_violent_crimes") %>%
    mutate(decade=1990) -> grid_90

data_00 %>%
    data_grid(unemployment_rate=seq_range(unemployment_rate, n=100))%>%
    gather_predictions(linear_00, spline3_00, log_00,
                       .pred="predicted_violent_crimes") %>%
    mutate(decade=2000) -> grid_00


# merge all grids
merge(grid_80, grid_90, all.x=TRUE, all.y=TRUE) -> grid_8090
merge(grid_8090, grid_00, all.x=TRUE, all.y=TRUE) -> grid


# plot the models
ggplot(crime_unemployment_rate, aes(unemployment_rate, violent_crimes)) +
    geom_point() +
    geom_line(
        data=grid,
        aes(y=predicted_violent_crimes, color=model, linetype=factor(model)),
        size=1.5
    ) +
    facet_wrap(
        .~decade,
        scales="free_y",
    ) + theme_bw() + theme(legend.position="bottom") +
    labs(
        title="Models: Violent Crime-rate vs Unemployment-rate by the decades.",
        color="different models",
        linetype="different models",
        y="crime_rate (per capita)",
        x="unemployment_rate (per capita)"
    )


# generate the error in predictions, residuals
data_80 %>%
    transmute(
        decade,
        unemployment_rate,
        linear=resid(linear_80),
        spline3=resid(spline3_80),
        log=resid(log_80),
    ) %>%
    melt(id=c("decade", "unemployment_rate")) -> residuals_80
names(residuals_80) = c("decade", "unemployment_rate", "model", "residual")

data_90 %>%
    transmute(
        decade,
        unemployment_rate,
        linear=resid(linear_90),
        spline3=resid(spline3_90),
        log=resid(log_90),
    ) %>%
    melt(id=c("decade", "unemployment_rate")) -> residuals_90
names(residuals_90) = c("decade", "unemployment_rate", "model", "residual")

data_00 %>%
    transmute(
        decade,
        unemployment_rate,
        linear=resid(linear_00),
        spline3=resid(spline3_00),
        log=resid(log_00),
    ) %>%
    melt(id=c("decade", "unemployment_rate")) -> residuals_00
names(residuals_00) = c("decade", "unemployment_rate", "model", "residual")


# merge all residuals
merge(residuals_80, residuals_90, all.x=TRUE, all.y=TRUE) -> residuals_8090
merge(residuals_8090, residuals_00, all.x=TRUE, all.y=TRUE) -> residuals


# plot the residuals
ggplot(residuals, aes(unemployment_rate, residual)) +
    geom_point(aes(color=model, shape=model), size=2) + geom_hline(yintercept=0) +
    facet_wrap(.~decade, scales="free_y") +
    theme_bw() + theme(legend.position="bottom") +
    labs(
        title="Residuals for the Models: Linear, Log, Spline3",
        color="Models", shape="Models",
        x="unemployment_rate (per capita) - predictions",
        y="residuals (by various models)"
    )
################################################################################
