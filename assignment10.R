
library(tidyverse)
library(sf)
library(tidycensus)
library(tigris)
##census_api_key("3d920503f6b6f180fa61efea4c86474d76a412cf", install = T)
df_census_ma <- get_acs(geography = "county",
                     state = "MA",
                     year = 2022,
                     variables = "DP02_0066PE", # Pop >=25 with Bachelors
                     output = "wide",
                     geometry = TRUE)
head(df_census_ma )

st_crs(df_census_ma )


df_census_ma <- df_census_ma  |>
  st_transform(crs = 4326)

st_crs(df_census_ma)

## create base map
base_map <- ggplot() +
  geom_sf(data = df_census_ma,
          aes(fill = DP02_0066PE),
          color = "black",
          size = 0.1) +
  labs(fill = str_wrap("Percent Population with graduate's or professional", 20)) +
  scale_fill_gradient(low = "#999985", high = "#ffff29") +
  theme_minimal()

## call base map by itself
base_map
# layer 2
df_ipeds <- read_csv("data/mapping-api-data.csv")

head(df_ipeds)

df_ipeds <- df_ipeds |> 
  st_as_sf(coords = c("LONGITUD", "LATITUDE"))

head(df_ipeds)

st_crs(df_ipeds)


df_ipeds <- df_ipeds |> 
  st_set_crs(4326) 

st_crs(df_ipeds)
point_map <- base_map +
  geom_sf(data = df_ipeds |> filter(FIPS == 25), 
          aes(size = LEXPTOT),
          alpha = 0.8,
          shape = 15, 
          fill = "black", 
          color = "white") + 
  labs(size = "Total expenditures for Library")+
  scale_size_continuous(labels = scales::comma_format())

## show new map
point_map



