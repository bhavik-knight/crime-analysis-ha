# load library
library(tidyverse)
library(reshape)


# to scale the dataset use this function
get_normal <- function(x) {
    mu <- mean(x)
    std <- sd(x)
    score <- ( (x) - mu ) / std
    score
}


# to convert a given year to its decade
get_decade <- function(x) {
    x - x %% 10
}

# Load the dataset
raw_homicide <- read.csv(
    "crime_us_homicide_1980_2014.csv",
    stringsAsFactors=FALSE,
    na.strings=c("NA", "N/A", "Uknown*", "NULL", ".P")
)

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
        Victim.Sex:Victim.Race,
        Perpetrator.Sex:Perpetrator.Race,
        Weapon
    ) -> homicides


homicides %>%
    count(Victim.Race, Victim.Sex, Perpetrator.Race, Perpetrator.Sex) ->
    homicides_plot


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
    geom_bar(aes(fill=Victim.Sex), stat="identity") +
    labs(title = "Weapons Used",
         x= "Weapon Used",
         y="Count",
         fill = "Victim's Sex"
    ) +
    coord_flip() + theme_bw()
