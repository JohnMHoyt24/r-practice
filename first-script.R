library(tidyverse)
#First fake data will be generated
sales <- data.frame(
                  neighborhood = c("Peaceful Glen", "Peaceful Glen", "Peaceful Glen",
                    "Peaceful Glen", "Pleasant Valley", "Pleasant Valley",
                    "Pleasant Valley", "Pleasant Valley", "Pleasant Valley"),
                  property_class = c("Dwelling", "Dwelling", "Mobile Home",
                                     "Mobile Home", "Commercial", "Commercial",
                                     "Dwelling", "Dwelling", "Mobile Home"),
                  sales_price = c(22900, 349900, 235000, 29000, 340000, 940250,
                                  510000, 429382, 9000)
                  )
head(x = sales, n = 5) #Displays the first five rows of data

total_sales <- nrow(sales) #Displays the number of rows of data

dwelling_sales <- filter(sales,
                         property_class == "Dwelling") #Keep just Dwelling sales

head(x = dwelling_sales, n = 5)

#Average price by neighborhood
dwelling_sales %>%
  group_by(neighborhood) %>%
  summarize(
    n_sales = n(),
    mean_price = mean(sales_price),
    median_price = median(sales_price)
  ) %>%
  arrange(median_price)

#plot all sales prices by neighborhood
dwelling_sales %>%
  ggplot(aes(sales_price)) +
  geom_histogram() +
  scale_x_log10(labels = scales::label_dollar()) +
  facet_wrap(~ neighborhood, scale = "free_y") +
  labs(
    x = "Sales price",
    y = "Number of sales"
  )

#Created a function that finds the percentage of sales that are Dwellings
ds_ts <- (nrow(dwelling_sales) / total_sales) * 100

#Took object, from the above function, and rounded the percentage to two decimal places
rounded_dsts <- round(ds_ts, digits = 2)

