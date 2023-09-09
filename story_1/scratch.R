library(tidyverse)
library(readxl)

## read the census population data
## territories from: https://www.census.gov/library/stories/2021/10/first-2020-census-united-states-island-areas-data-released-today.html
census <- read_csv('NST-EST2022-ALLDATA.csv')
census <- census %>% filter(STATE > 0)

census <- census %>% select(NAME, POPESTIMATE2022)
census$NAME <- census$NAME %>% str_to_upper()
colnames(census) <- c("state", "population")
census %>% arrange(desc(population)) %>% print(n=100)

territories <- data.frame(
    state=c("AMERICAN SAMOA","GUAM","NORTHERN MARIANA ISLANDS","US VIRGIN ISLANDS"),
    population = c(49710 , 153836, 47329 ,87146)
)

census <- rbind(census, territories)

sum_population <- sum(census$population)

census <- census %>% mutate(estimate_allocation = population / sum_population)



# read Infrastructure Investment and Jobs excel data
iija <- read_excel("IIJA FUNDING AS OF MARCH 2023(1).xlsx")
colnames(iija) <- c("state", "allocation")
sum_allocation <- sum(iija$allocation)
iija <- iija %>% mutate (actual_allocation = allocation / sum_allocation)



total <- census %>% inner_join(iija)

election <- read_csv("2020-election.csv")
election$state <- election$state %>% str_to_upper()
total <- total %>% inner_join(election) %>% print (n=10)

total <- total %>% mutate (bias = estimate_allocation - actual_allocation)
print(total, n=100)

#t %>% select(state, bias, biden, ratio_allocation, ratio_population) %>% print(n=1000)
total <- total %>% mutate (bias = ifelse(actual_allocation > estimate_allocation *1.1, 1, 0))
total <- total %>% mutate (biden = ifelse(biden > 0, 1, 0))

library(plotly)

# give state boundaries a white border
l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

### Biden electoral votes
election_figure <- plot_geo(total, locationmode = 'USA-states')
election_figure <- election_figure %>% add_trace(
    z = ~biden, locations = ~code,
    color = ~biden, colors = 'Blues'
  )
election_figure <- election_figure %>% colorbar(title = "Biden votes")
election_figure <- election_figure %>% layout(
    title = '2020 election_figure',
    geo = g
  )

election_figure



## IIJA bias funding
iija_figure <- plot_geo(total, locationmode = 'USA-states')
iija_figure <- iija_figure %>% add_trace(
    z = ~bias, locations = ~code,
    color = ~bias, colors = 'Greens'
  )
iija_figure <- iija_figure %>% colorbar(title = "Funding Bias ")
iija_figure <- iija_figure %>% layout(
    title = '2020 iija_figure',
    geo = g
  )

iija_figure

