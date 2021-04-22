## Tidy Tuesday project - Netflix
## Tereza Lausová
## 20.4.2021

## libraries
require(maps)
library(tidyverse)
library(here)
library(patchwork)

## data
tuesdata <- tidytuesdayR::tt_load('2021-04-20')
netflix <- tuesdata$netflix
world_map <- map_data("world")

## theme
theme_set(theme_dark())
theme_update(
  rect = element_rect(fill = "#221f1f"),
  line = element_line(colour = "#5d5d5d"),
  text = element_text(colour = "#f5f5f1"),
  title = element_text(colour = "#f5f5f1"),
  axis.text = element_text(colour = "#f5f5f1"),
  panel.background = element_rect(fill = "#221f1f"),
  panel.grid = element_line(colour = "#5d5d5d")
)

## convert columns with multiple values to lists
netflix$country <- str_split(string = netflix$country, pattern = ", ") %>%
  purrr::map(., ~str_remove(., pattern = ","))
netflix$listed_in <- str_split(string = netflix$listed_in, pattern = ", ") %>%
  purrr::map(., ~str_remove(., pattern = ","))
netflix$cast <- str_split(string = netflix$cast, pattern = ", ") %>%
  purrr::map(., ~str_remove(., pattern = ","))

## Harmonizing country labels
### Historical shows were omitted, NAs were retained - for now
netflix_countries <- netflix$country %>% 
  unlist() %>% 
  unique()
netflix_countries[!netflix_countries %in% (world_map$region %>% unique() %>% sort())]
world_map$region[world_map$region == "Vatican"] <- "Vatican City"
world_map$region[world_map$region == "USA"] <- "United States"
world_map$region[world_map$region == "UK"] <- "United Kingdom"
# world_map$region[world_map$region == "China"] <- "China & Hong Kong"
netflix <- netflix %>% 
  mutate(country = purrr::map(country, ~if_else(. %in% c("Hong Kong", "China"), "China & Hong Kong", .)))

## Join the world data and netflix data
world_netflix <- left_join(world_map %>% 
                             group_by(region) %>%
                             nest(map_data = c(long,lat,order,group,subregion)), 
                           netflix %>% 
                             unnest(country) %>%
                             group_by(country) %>%
                             nest(netflix_data = c(show_id,type,title,director,cast,date_added,release_year,rating,duration,listed_in,description)), 
                           by = c("region" = "country"))

## Countries with at least one netflix movie/TV show
ggplot(world_netflix %>% filter(!is.null(netflix_data[[1]])), aes(x = long, 
                          y = lat, 
                          group = group)) +
  geom_polygon(fill="#e50914", colour = "#221f1f", size = 0.1)


## Number of movies per country
wrap_plots(
  world_netflix %>% 
  filter(!is.null(netflix_data[[1]])) %>%
  mutate(n_of_shows = n_distinct(netflix_data[[1]])) %>%
  unnest(map_data) %>%
  ggplot(aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill=n_of_shows)) +
  scale_fill_gradient(low = "#63050a", high = "#e50914"),
netflix %>%
  unnest(country) %>%
  group_by(country) %>%
  summarise(n_of_shows = n_distinct(show_id)) %>%
  arrange(desc(n_of_shows)) %>%
  head(10) %>%
  ggplot() +
  geom_bar(aes(x=reorder(country,-n_of_shows), y=n_of_shows, fill=n_of_shows), stat = "identity")+
  scale_fill_gradient(low = "#63050a", high = "#e50914")
) +
  patchwork::plot_layout(ncol = 1, nrow = 2, heights = c(3,1))



## Types of movies per country
maps <- world_netflix %>% 
  filter(!is.null(netflix_data[[1]])) %>%
  unnest(netflix_data) %>%
  select(region, map_data, listed_in, show_id) %>%
  unnest(listed_in) %>%
  group_by(region,listed_in) %>%
  mutate(n_of_shows = n_distinct(show_id)) %>%
  select(-show_id) %>%
  group_by(listed_in) %>%
  group_split() %>%
  map(., ~distinct(.,region, .keep_all = TRUE) %>%
        unnest(.,cols = map_data)) %>%
  map(., ~ggplot(.,aes(x=long, y=lat, group=group)) +
        geom_polygon(aes(fill=n_of_shows)) +
        scale_fill_gradient(low = "#63050a", high = "#e50914") +
        labs(title = .$listed_in[1]) +
        coord_cartesian(xlim = c(-180,180), ylim=c(-90,90)))

bars <- netflix %>%
  unnest(country) %>%
  unnest(listed_in) %>%
  group_by(country,listed_in) %>%
  summarise(n_of_shows = n_distinct(show_id)) %>%
  ungroup() %>%
  group_by(listed_in) %>%
  group_split() %>%
  map(., ~arrange(.,desc(n_of_shows)) %>%
        head(5)
      ) %>%
  map(., ~ggplot(.) +
        geom_bar(aes(x=reorder(country,-n_of_shows), y=n_of_shows, fill=n_of_shows), stat = "identity")+
        scale_fill_gradient(low = "#63050a", high = "#e50914") +
        guides(fill = "none")#+
        # labs(title = .$listed_in[1])
      )

pdf(here("2021-04-20/out/maps_by_genre.pdf"), width = 6, height = 6)
map2(maps, bars, .f = function(x,y) {
  wrap_plots(x,y) +
    patchwork::plot_layout(ncol = 1, nrow = 2, heights = c(3,1))
})
dev.off()



## Director/actor with widest span

## Genres in countries
pdf(here("2021-04-20/out/genres_in_countries.pdf"), width = 6, height = 3)
netflix %>%
  unnest(country) %>%
  unnest(listed_in) %>%
  group_by(listed_in,country) %>%
  summarise(n_of_shows = n_distinct(show_id)) %>%
  ungroup() %>%
  group_by(country) %>%
  group_split() %>%
  map(., ~arrange(.,desc(n_of_shows))
  ) %>%
  map(., ~ggplot(.) +
        geom_bar(aes(x=reorder(listed_in,-n_of_shows), y=n_of_shows, fill=n_of_shows), stat = "identity")+
        scale_fill_gradient(low = "#63050a", high = "#e50914") +
        guides(fill = "none") +
        labs(title = .$country[1]) +
        theme(axis.text.x = element_text(angle = 45))
  )
dev.off()

## Most common genre in a country
non_genre_labels <- c("International TV Shows",
                      "International Movies",
                      "British TV Shows",
                      "Korean TV Shows",
                      "Spanish-Language TV Shows")
palette_genres <- c("Action & Adventure" = "#e50914", # red
                    "Comedies" = "#FF9505", # yellow
                    "TV Comedies" = "#FF9505", # yellow
                    
                    "Anime Series" = "#EE92C2", # light pink
                    "Romantic TV Shows" = "#A01A7D", # deep pink
                    "Classic Movies" = "#BCA371", # golden
                    
                    "Crime TV Shows" = "#BEC5AD", # gray
                    "Dramas" = "#686963", # dark red
                    "TV Dramas" = "#686963", # dark red
                    "Horror Movies" = "#9D9C62", # dark green
                    "Thrillers" = "#086788", # dark blue
                    
                    "Documentaries" = "#149911", # green
                    "Independent Movies" = "#2C8C99", # blue-green
                    
                    "Children & Family Movies" = "#C8E0F4", # blue
                    "Kids' TV" = "#508AA8" # light blue
)

world_netflix %>% 
  filter(!is.null(netflix_data[[1]])) %>%
  unnest(netflix_data) %>%
  select(region, map_data, listed_in, show_id) %>%
  unnest(listed_in) %>%
  filter(!listed_in %in% non_genre_labels) %>%
  group_by(region,listed_in) %>%
  mutate(n_of_shows = n_distinct(show_id)) %>%
  select(-show_id) %>%
  group_by(region) %>%
  arrange(n_of_shows, .by_group = TRUE) %>%
  top_n(1) %>%
  distinct(region,.keep_all = TRUE) %>%
  mutate(listed_in = factor(listed_in, levels = c("Action & Adventure", # red
                                                  "Comedies", # yellow
                                                  "TV Comedies", # yellow
                                                  
                                                  "Anime Series", # light pink
                                                  "Romantic TV Shows", # deep pink
                                                  "Classic Movies", # golden
                                                  
                                                  "Crime TV Shows", # gray
                                                  "Dramas", # dark red
                                                  "TV Dramas", # dark red
                                                  "Horror Movies", # dark green
                                                  "Thrillers", # dark blue
                                                  
                                                  "Documentaries", # green
                                                  "Independent Movies", # blue-green
                                                  
                                                  "Children & Family Movies", # blue
                                                  "Kids' TV") # light blue
  )) %>%
  unnest(map_data) %>%
  ggplot(aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill=listed_in)) +
  scale_fill_manual(values = palette_genres) +
  # labs(title = .$listed_in[1]) +
  coord_cartesian(xlim = c(-180,180), ylim=c(-90,90))


