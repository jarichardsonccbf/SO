library(tidyverse, data.table)

products <- read.csv("data/B_Products.csv")

so <- read.csv("data/B_SO_RacetracAndOthers_2017_20190729_dates.csv")

# Did duplicate material Number products switch number at some point in time? ----

# get "duplicate" product material number lists
product.dupes <- products %>% 
  group_by(Name) %>% 
  mutate(count = n()) %>% 
  filter(count > 1) %>%
  ungroup() %>% 
  select(MaterialNo) %>% 
  rename(Material.No = MaterialNo)

so.duplicates <- so %>% 
  semi_join(product.dupes, "Material.No")

orders.of.duplicates <- so.duplicates %>%
  group_by(OrderDate, Material.No, Material.Name) %>% 
  summarise(order.qty = sum(Ordered.Qty)) %>% 
  ungroup() %>% 
  mutate(OrderDate = as.Date(as.character(OrderDate), "%Y%m%d"))

orders.of.duplicates %>% 
  ggplot(aes(x = Material.Name, y = OrderDate, color = factor(Material.No))) + 
  geom_jitter() + 
  theme(axis.text.x = element_text(angle = 90)) +
  coord_flip()

from <- c("147294", "146294", "147292", "146297", "146296", "147293", "146295", "128259", "151764", "151762")

to   <- c("152940", "152950", "152935", "152952", "152957", "152965", "152951", "152196", "154534", "154535")

cbind(from, to)
