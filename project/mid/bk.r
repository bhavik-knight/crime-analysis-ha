# load library
library(tidyverse)
library(reshape)


# load crime_us_1975_2015 data
# drop samples with missing values
# add new column if required
# select desired column
# filter unwanted samples
raw_crime_us <- read.csv("crime_us_1975_2015.csv")
raw_crime_us %>% drop_na() %>%
    mutate(
        states_abb = str_split_fixed(agency_jurisdiction, ", ", n = 2)[, 2]
    ) %>%
    select(
        report_year,
        states_abb,
        population,
        violent_crimes,
        homicides,
        rapes,
        assaults,
        robberies
    ) %>%
    filter(
        report_year %in% 1980:2014
    ) -> crime_us


# load homicide data set
raw_homicide <- read.csv(
    "crime_us_homicide_1980_2014.csv",
    stringsAsFactors=FALSE,
    na.strings=c("NA", "N/A", "Uknown*", "NULL", ".P")
    )

raw_homicide %>% drop_na() %>%
    select(
        Year,
        Month,
        State,
        Incident,
        Crime.Solved,
        Victim.Sex:Victim.Race,
        Perpetrator.Sex:Perpetrator.Race,
        Relationship
    ) %>% filter(
        Year %in% 1980:2014
    ) -> homicides


# crime rate is crime reported per 100,000 population
get_crime_rate <- function(reported_crimes, total_population) {
    crime_rate <- (reported_crimes / total_population) * 10^5
    crime_rate
}

get_normal <- function(x) {
    mu <- mean(x)
    std <- sd(x)
    score <- ( (x) - mu ) / std
    score
}

# summarize by years, states, but need a data frame, not tibble
as.data.frame(crime_us  %>%
                  group_by(report_year) %>%
                  summarise_each(funs(sum), population:robberies)) -> crime_by_years

as.data.frame(crime_us %>%
                  group_by(states_abb) %>%
                  summarise_each(funs(mean), population:robberies)) -> crime_by_states


as.data.frame(homicides %>%
                  group_by(Year) %>%
                  summarise_each(
                      funs(mean), Victim.Age, Perpetrator.Age
                      )) -> homicides_victim_perpetrator_age

ggplot(homicides_victim_perpetrator_age, aes(Year, Perpetrator.Age)) +
    geom_bar(aes(fill=Perpetrator.Age, ), stat="identity", color="darkblue") +
    labs(title="Perpetrator average age by years") + theme_classic()

# make various type of crime as the factor of violent crimes
# split the table, reshape, and join
# split
crime_by_years[, 1:3] -> years
crime_by_states[, 1:3] -> states

# reshape the other part, make factors
crime_by_years[, 3:7] %>% melt(id="violent_crimes", value.name="type") -> crime_by_years
crime_by_states[, 3:7] %>% melt(id="violent_crimes", value.name="type") -> crime_by_states

# join - using inner join
inner_join(years, crime_by_years) -> years_data
inner_join(states, crime_by_states) -> states_data


# summarize crime_rate by years, states, need a data frame
as.data.frame(years_data %>% transmute(
    year = report_year,
    rate_violent_crimes = (get_crime_rate(violent_crimes, population)),
    crime_type = variable,
    crime_recorded = value,
    rate_crime_type = (get_crime_rate(crime_recorded, population))
)) -> years_crime_rate

as.data.frame(states_data %>% transmute(
    states_abb = states_abb,
    rate_violent_crimes = (get_crime_rate(violent_crimes, population)),
    crime_type = variable,
    crime_recorded = value,
    rate_crime_type = (get_crime_rate(crime_recorded, population))
)) -> states_crime_rate


# normalized crime_rate by years, states
as.data.frame(years_crime_rate %>% transmute(
    year = year,
    rate_violent_crimes = get_normal(rate_violent_crimes),
    crime_type = crime_type,
    rate_crime_type = get_normal(log10(rate_crime_type)) # check w/ log
)) -> normalized_years_crime_rate

as.data.frame(states_crime_rate %>% transmute(
    states_abb = states_abb,
    rate_violent_crimes = get_normal(rate_violent_crimes),
    crime_type = crime_type,
    rate_crime_type = get_normal(rate_crime_type)
)) -> normalized_states_crime_rate


# summarize crime by years data based on types of crime
years_crime_rate %>%
    group_by(crime_type) %>%
    summarise_each(funs(sum), rate_crime_type) -> pie_crime_data

# plot pie chart of violent crime rate
ggplot(pie_crime_data, aes(rate_crime_type, factor(crime_type))) +
    geom_bar(aes(fill = crime_type), width=1, stat="identity") +
    coord_polar(theta="x")


# time series of violent crimes
ggplot(years_crime_rate, aes(year, rate_violent_crimes)) +
    geom_point() + geom_line() + theme_classic() +
    labs(title="Violent Crime Rate by Years") +
    xlab("years (1980-2014)") + ylab("crime_rate (per capita)")


# plot data over the year
ggplot(years_crime_rate, aes(year, rate_crime_type)) +
    # geom_point(aes(color=crime_type)) + geom_smooth() +
    geom_bar(aes(fill=crime_type), width=0.8, stat="identity") +
    xlab("years (1980-2014)") + ylab("crime_rate (per capita)") +
    labs(title="Violent Crime Rate by Years") + theme_classic()

ggplot(states_crime_rate, aes(states_abb, rate_crime_type)) +
    # geom_point(aes(color=crime_type)) + geom_smooth() +
    geom_bar(aes(fill=crime_type), width=0.8, stat="identity") +
    labs(title="Violent Crime Rate by States") + ylab("crime_Rate (per capita)") +
    xlab("States") + theme_classic()

# plot several crimes over the year
#geom_line(aes(y=rate_violent_crimes)) + geom_point(aes(y=rate_violent_crimes)) +
ggplot(normalized_years_crime_rate, aes(year, rate_crime_type)) +
    geom_line(aes(y=rate_violent_crimes)) + geom_point(aes(y=rate_violent_crimes)) +
    geom_line(aes(color=crime_type)) +
    geom_point(aes(color=crime_type), shape = 15) +
    labs(title="Crime Rate for various types of Violent Crimes") +
    ylab("normalized_crime_rate (log-scale)") + xlab("years (1980-2014)") +
    theme_classic()

