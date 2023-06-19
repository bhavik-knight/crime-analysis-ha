################################################################################
# load required libraries
library(tidyverse)
library(reshape)
library(modelr)
library(splines)
library(wesanderson)
options(na.action=na.warn)
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
# crime_us dataset
# load crime_us_1975_2015 data
# drop samples with missing values
# add new column if required
# select desired column
# filter unwanted samples
raw_crime_us <- read.csv("crime_us_1975_2015.csv")
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


################################################################################
# load unemployment data
raw_unemployment <- read.csv("crime_unemployment_1976_2014.csv")

# make column names lower case to be compatible with other data sets
names(raw_unemployment) <- tolower(names(raw_unemployment))

as.data.frame(
    raw_unemployment %>% drop_na() %>% group_by(year) %>%
        summarise_at(vars(unemployment), sum) %>%
    filter(year %in% 1980:2009)
) -> unemployment_us
################################################################################


################################################################################
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
# wes anderson color palette
wa = wesanderson::wes_palette("GrandBudapest1")
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
        fill="decades"
    ) + xlab("decades") + ylab("unemployment_rate (per capita)")

# boxplot of unemployment by decades
crime_unemployment_rate %>%
    ggplot(aes(x=decade, y=crime_rate)) +
    stat_boxplot(geom="errorbar", width=0.4) +
    geom_boxplot(aes(fill=decade), outlier.shape=NA, width=0.4) +
    facet_wrap(.~crime_type, scales="free_y", nrow=1) +
    theme_bw() +
    xlab("decades") + ylab("crime_rate (incidents per capita)") +
    labs(
        title="Trend of various type of crimes by decades.",
        fill="decades",
    )
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

