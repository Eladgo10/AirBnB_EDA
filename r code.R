library(tidyverse)
library(lubridate)
library(patchwork)
#setwd("C:/Users/97254/EinBDW_github/datasets")

listing <- read_csv("data/listings_clean.csv")
calander <- read_csv("data/calendar_clean.csv")


z <- listing %>% 
  filter(host_since == "2015-12-13") %>% 
  select(host_since) %>% 
  pull

dates <- listing %>% 
  mutate(month = month(host_since) , year = year(host_since)) %>% 
  group_by(year , month) %>% 
  summarise(n = n())



dates <- dates %>% 
  unite("Merged", year:month , remove = F) %>% 
  mutate(Merged = ym(Merged)) %>% 
  arrange(month) %>% 
  mutate(month = month.name[month])


p_dates <- ggplot(dates , aes(x = Merged , y = n)) +
  geom_line(aes(col = month)) +
  geom_vline(xintercept = z[1] , linetype = 'dashed' , col = 'red' ) +
  annotate("segment" , x = max(listing$host_since) - 50 , y = 100 ,
           xend = z[1] + 20 , yend = 80 , col = 'red' ,
           size = 1 ,arrow = arrow()) +
  annotate("text" , x = max(listing$host_since) - 60 ,
           y = 105 , color = 'red' , size = 3 ,
           label = "New Harvard Study \n13-12-2015") +
  labs(
    title = "New Hosts Over the Years" ,
    subtitle = "Increasing Trend of the Number of new hosts, Untill New Harvard Study \nAbout Racial Discrimination in AirBNB" ,
    x = "Date of New Host" , 
    y = "Number of New Hosts"
  )



abc <- listing %>% 
  select(host_response_time) %>% 
  filter(host_response_time != "N/A")


p_response <- ggplot(abc , aes(x = host_response_time , fill = host_response_time)) +
  geom_bar() +
  labs(
    title = "Host Response Time" ,
    subtitle = "Are Hosts Invested in the Property" ,
    x = "host response time"
  ) + 
  theme(plot.subtitle = element_text(size = 15) , text = element_text(size = 18),
        plot.title = element_text(size = 18))

p_response

p_price <- ggplot(listing , aes(x = price_dollars )) +
  geom_density(fill = 'lightblue') +
  theme_light() +
  annotate("segment" , x = max(listing$price_dollars) - 400 ,
           y  = 0.001 , xend = max(listing$price_dollars) ,
           yend = 0.0001 , arrow = arrow() ,
           size = 0.6 , col = 'red') +
  annotate("segment" , x = sort(listing$price_dollars , decreasing = T)[2] - 400 ,
           y  = 0.001 , xend = sort(listing$price_dollars , decreasing = T)[2] ,
           yend = 0.0001 , arrow = arrow() ,
           size = 0.6 , col = 'red') +
  annotate("text" , x = max(listing$price_dollars) - 400 ,
           y = 0.0013 , color = 'red' , size = 3 ,
           label = "4000$") +
  annotate("text" , x = sort(listing$price_dollars , decreasing = T)[2] - 400 ,
           y = 0.0013 , color = 'red' , size = 3 ,
           label = "3000$") +
  labs(
    title = "Price Distribuiton" ,
    subtitle = "Two Big Outliers , One is 4000$ \nand One is 3000$" ,
    x = "price" ,
    y = 'density'
  ) +
  theme(plot.subtitle = element_text(size = 15) , text = element_text(size = 15),
        plot.title = element_text(size = 18))
  
  

p_log <- ggplot(listing , aes(x = log(price_dollars ))) +
  geom_density(fill = 'lightblue') +
  theme_light() +
  geom_vline(xintercept = mean(log(listing$price_dollars)) ,
             linetype = "dashed" , col = 'red' ,
             size = 0.5) +
  labs(
    title = "Log Price Distribuition" , 
    subtitle = "Log Price Have Normal Distribuition \nWith Mean of 5" ,
    x = "Log Price"
  ) +
  theme(plot.subtitle = element_text(size = 12) ,
        plot.title = element_text(size = 15))

p_dates
p_response
p_price | p_log


################################################################################

b <- listing %>% 
  group_by(property_type) %>% 
  summarise(n = n() , price = mean(price_dollars) , 
            popularity = mean(reviews_per_month , na.rm = T))

b <- b %>% 
  filter(n > 20) %>% 
  mutate(mean_rev = price*popularity) %>% 
  arrange(-price)

ggplot(b , aes(x = property_type , y = price , col = factor(n))) +
  geom_point(size = 4) +
  geom_text(aes(label = round(popularity,2)) , hjust = -0.5 ,
            size = 3) +
  theme_light() +
  labs(
    title = "What Property is the Most Profitible? " ,
    subtitle = "Number Beside the Points Represents Average \nReviews Per Month" ,
    x = "Property Type" ,
    y = "Average price for stay" 
  ) +
  scale_color_discrete(name = "Amount of a Property \nif Bigger than 20") +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
    plot.subtitle = element_text(size = 12) ,
        plot.title = element_text(size = 15)) +
  scale_x_discrete(limits = b$property_type)


a <- listing %>% 
  group_by(neighbourhood) %>% 
  summarise(mean = mean(review_scores_location , na.rm = T) ,
            n = n()) %>% 
  drop_na() %>% 
  arrange(-mean)

ggplot(a , aes(neighbourhood , mean , col = neighbourhood)) +
  geom_point() +
  scale_x_discrete(limits = a$neighbourhood , 
                   breaks = a$neighbourhood[1:15]) +
  theme(axis.text.x = element_text(angle = 45 , size = 10 , 
                                   vjust = 1 , hjust = 1) ,
        axis.text.y = element_text(size = 10),
        legend.position = 'none' ,
        axis.title.y = element_text(size = 15) , 
        plot.subtitle = element_text(size = 12)) +
  geom_text(data = a[1:15,] , aes(label = n) , 
            size = 3 , vjust = 2 , fontface = "bold" , col = "black") +
  scale_color_manual(breaks = a$neighbourhood , 
                     values = c(rep('green' , 15) , rep('red' , 14))) +
  labs(
    title = "Best Neighbourhoods" , 
    subtitle = "15 Neighbourhoods With Best Location Scores \nWith the Number of Properties in Neighbourhood" , 
    x = "Neighbourhood" , 
    y = "Average Location Score"
  )


data_for_reviews <- listing %>% 
  drop_na(beds) %>% 
  drop_na(review_scores_rating) %>% 
  drop_na(price_dollars) %>% 
  drop_na(property_type) %>% 
  filter(beds <=5 & beds != 0 , review_scores_rating >= 40) %>% 
  select(beds , review_scores_rating , price_dollars , property_type)


ggplot(data_for_reviews , aes(x = review_scores_rating , y = log(price_dollars))) +
  geom_point(size = 0.5) +
  facet_wrap(~beds) +
  labs(
    title = "Number of Beds and Ratings Affect on Price" ,
    subtitle = "When The Number of Beds is Bigger Than 2 \nWe Can See The Affect of Ratings" ,
    y = "Log Price" , 
    x = "Overall Review Score"
  )

library(corrplot)
cor_mat <- listing %>% 
  select(starts_with("review")) %>%
  select(-reviews_per_month) %>% drop_na() %>% rename_with(~gsub('review_scores_',"", .x)) %>% 
  cor()


# Get upper triangle of the correlation matrix
get_upper_tri <- function(cor_mat){
  cor_mat[lower.tri(cor_mat)]<- NA
  return(cor_mat)
}

upper_tri <- get_upper_tri(cor_mat)
upper_tri <- round(upper_tri, 2)

# Melt the correlation matrix
library(reshape2)
melted_cormat <- melt(upper_tri, na.rm = TRUE)

# Heatmap
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+ 
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(plot.title = element_text(size = 18),plot.subtitle = element_text(size = 15) ,axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 14, hjust = 1), axis.text.y = element_text(size = 14), legend.text = element_text(size = 14)) +
  coord_fixed() +  geom_text(aes(Var2, Var1, label = value), color = "black", size = 5) + 
 labs(title = "Which Category Rating Is Most Correlated With Overall Rating Of A Property?", 
      subtitle = "Correlation matrix heatmap for the categories rating") + 
  xlab(element_blank()) + ylab(element_blank())




############################################################3

dddd <- calander %>% 
  mutate(month = month(date) , year = year(date)) %>% 
  group_by(listing_id ,year,month) %>% 
  count(available_category) %>% 
  filter(available_category == 0) %>% 
  summarise(n = mean(n) , pct = n/30)


dddd_mean <- dddd %>% 
  group_by(month , year) %>% 
  summarise(mean = mean(pct)) %>% 
  arrange(year , month)


prev_diff <- dddd_mean$mean - lag(dddd_mean$mean)
prev_diff <- prev_diff[-c(1 , length(prev_diff))]
plot(prev_diff)

mean(prev_diff)

dat_neigh <- listing %>% 
  mutate(year = year(host_since)) %>% 
  filter(!is.na(neighbourhood)) %>% 
  group_by(year , neighbourhood) %>% 
  count()

func <- function(nei){
  vec <- dat_neigh %>% 
    filter(neighbourhood == nei) %>% 
    pull()
  out <- vec/lag(vec)
  out <- out[!is.na(out)]
  out
}

nei_vec <- unique(dat_neigh$neighbourhood)

list_nei <- map(nei_vec , func)

mean_lag_nei <- sapply(list_nei , mean)
names(mean_lag_nei) <- nei_vec
mean_lag_nei


ggplot(dat_neigh , aes(year , neighbourhood)) +
  geom_point(aes(size = n , col = as.factor(year)))
  

dates_nei <- calander %>% 
  inner_join(listing , by = c("listing_id" = "id")) %>% 
  select(listing_id , price_dollars.x , date , property_type)

dates_nei <-  dates_nei %>% 
  mutate(month = month(date) , year = year(date)) %>% 
  group_by(listing_id ,year,month) %>% 
  mutate(price_dollars.x =  gsub("[\\$,]", "", price_dollars.x)) %>% 
  mutate(price_dollars.x = as.numeric(gsub("\\,", "", price_dollars.x)))

dates_nei <- dates_nei %>% 
  group_by(year , month , property_type) %>% 
  summarise(price = mean(price_dollars.x , na.rm = T))

vec_property <- unique(dates_nei$property_type)

func_prop <- function(prop){
  vec <- dates_nei %>% 
    filter(property_type == prop) %>% 
    pull()
  out <- vec/lag(vec)
  out <- out[!is.na(out)]
  out
}

list_prop <- map(vec_property , func_prop)

mean_lag_prop <- sapply(list_prop , mean)
names(mean_lag_prop) <- vec_property
mean_lag_prop

####################################
listing %>% 
  select(beds , price_dollars  , host_is_superhost , review_scores_accuracy , 
         review_scores_cleanliness , review_scores_checkin , review_scores_communication , 
         review_scores_location , review_scores_value, id) %>% 
  filter(is.na(review_scores_checkin)) %>% 
  glimpse()

id_calender <- calander %>% 
  filter(listing_id %in% id_vec)
  


listing %>% 
  slice_max(beds , n=10 , with_ties = F) %>% 
  select(beds, price_dollars , review_scores_rating , 
         reviews_per_month , property_type , neighbourhood)




listing %>% 
  filter(!id %in% id_vec) %>% 
  group_by(neighbourhood) %>% 
  summarise(mean_price = mean(price_dollars , na.rm  = T)) %>% 
  arrange(-mean_price)



beds_data <- listing %>% 
  select(beds)

ggplot(beds_data , aes(beds)) +
  geom_bar() + labs(title = "Beds Distrabution")  + theme(text= element_text(size = 18), ,axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16))



listing %>% 
  filter(beds == 16) %>% 
  select(property_type , summary) %>% 
  pull(summary)
