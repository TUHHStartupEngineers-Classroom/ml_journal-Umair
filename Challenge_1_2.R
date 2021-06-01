#install.packages("plotly")

library(tidyverse)
library(tidyquant)
library(broom)
library(umap)


# STOCK PRICES
sp_500_prices_tbl <- read_rds("sp_500_prices_tbl.rds")
sp_500_prices_tbl


# SECTOR INFORMATION
sp_500_index_tbl <- read_rds("sp_500_index_tbl.rds")
sp_500_index_tbl




sp_500_prices_tbl %>% glimpse()


sp_500_daily_returns_tbl <- sp_500_prices_tbl %>% 
  group_by(symbol) %>% 

#filter(date >= as.Date("2018-01-04"))
   filter(year(date)>"2018") %>% 
  select(symbol, date, adjusted)


sp_500_daily_returns_tbl <- sp_500_daily_returns_tbl %>% 


mutate(adjusted_lag= lag(adjusted, n=1, dafault =NA))%>% 
  filter(!is.na(adjusted_lag))%>%


mutate(pct_return= (((adjusted-adjusted_lag)/adjusted))*100)%>%

select(symbol, date, pct_return)



sp_500_daily_returns_tbl <- read_rds("sp_500_daily_returns_tbl.rds")
sp_500_daily_returns_tbl



stock_date_matrix_tbl <- sp_500_daily_returns_tbl %>%


  select(symbol, date, pct_return) %>%
  pivot_wider(names_from = date, values_from = pct_return, values_fill = 0) %>%
  ungroup()


stock_date_matrix_tbl <- read_rds("stock_date_matrix_tbl.rds")

kmeans_obj <- stock_date_matrix_tbl %>%
  select(-symbol) %>%
  kmeans(centers = 4, nstart = 20)


broom::glance(kmeans_obj)

kmeans_mapper <- function(center = 3) {
  stock_date_matrix_tbl %>%
    select(-symbol) %>%
    kmeans(centers = center, nstart = 20)
}

kmeans_mapped_tbl <- tibble(centers = 1:30) %>%
  mutate(k_means = centers %>% map(kmeans_mapper)) %>%
  mutate(glance  = k_means %>% map(glance))





# 2.4 Skree Plot ----

kmeans_mapped_tbl %>%
  unnest(glance) %>%
  select(centers, tot.withinss) %>%
  
  # Visualization
  ggplot(aes(centers, tot.withinss)) +
  geom_point(color = "#2DC6D6", size = 4) +
  geom_line(color = "#2DC6D6", size = 1) +
  # Add labels (which are repelled a little)
  ggrepel::geom_label_repel(aes(label = centers), color = "#2DC6D6") + 
  
  # Formatting
  labs(title = "Skree Plot",
       subtitle = "Measures the distance each of the customer are from the closes K-Means center",
       caption = "Conclusion: Based on the Scree Plot, we select 4 clusters to segment the customer base.")




####################UMAP#######################



k_means_mapped_tbl <- read_rds("k_means_mapped_tbl.rds")

##Applying UMAP to get 4 variables


umap_obj <- stock_date_matrix_tbl %>%
  select(-symbol) %>%
  umap()


##Putting 4 variables in x y columns 

umap_results_tbl <- umap_obj$layout %>%
  as_tibble(.name_repair = "unique") %>% 
  set_names(c("x", "y")) %>%
  bind_cols(stock_date_matrix_tbl %>% select(symbol))




umap_results_tbl %>%
  ggplot(aes(x, y)) +
  ggplot2::geom_point(alpha(0.5))
  #geom_point(alpha(0.5)) + 
ggrepel::geom_label_repel(aes(label = symbol), size = 3)
theme_tq()
labs(title = "Skree Plot")



k_means_mapped_tbl <- read_rds("k_means_mapped_tbl.rds")
umap_results_tbl   <- read_rds("umap_results_tbl.rds")


## assigning cluster info to symbol

kmeans_obj <- kmeans_mapped_tbl %>%
  pull(k_means) %>%
  pluck(10)




kmeans_10_clusters_tbl <- kmeans_obj %>% 
  augment(stock_date_matrix_tbl) %>%

  select(symbol, .cluster)



umap_kmeans_10_results_tb   <- umap_results_tbl %>%
  left_join(kmeans_10_clusters_tbl)



umap_kmeans_results_tbl   <- sp_500_index_tbl %>%

left_join(umap_kmeans_10_results_tb)%>%
select(symbol, company, sector)



umap_kmeans_10_results_tb  %>%
  mutate(label_text = str_glue("Customer: {symbol}
                                 Cluster: {.cluster}")) %>%
  
  ggplot(aes(V1, V2, color = .cluster)) +
  
  # Geometries
  geom_point() +
  ggrepel::geom_label_repel(aes(label = label_text), size = 2, fill = "#282A36") +
  
  # Formatting
  scale_color_manual(values=c("#2d72d6", "#2dc6d6", "#2dd692", "#000000",  "#00FF00", "#00FFFF",
    "#C0C0C0", "#800000", "#800080", "#000080")) +
  labs(title = "Customer Segmentation: 2D Projection")









#sp_500_tbl <- sp_500_daily_returns_tbl %>% 
  ##  left_join(sp_500_index_tbl) %>% 
  #  left_join(sp_500_prices_tbl)

#sp_500_daily_returns_tbl <- sp_500_tbl %>% 
##  group_by(symbol)

#filter(date >= as.Date("2018-01-04"))
#   filter(year(date)>2018)%>%
#  select(symbol, date, adjusted)