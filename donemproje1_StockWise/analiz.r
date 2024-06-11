library("readxl")
library(dplyr)

# fetch the data that is already random
df <- read_xlsx("random_sample(1).xlsx")

# Remove rows with quantity below zero
df <- df %>%
  filter(Quantity > 0) 

# Remove rows with unit price equal to zero
df <- df %>%
  filter(UnitPrice > 0)

# Omit rows with customerID equal to NA
df <- subset(df, !is.na(CustomerID))

# function that checks if it is real stock code
# what real stock code means is
# having a hexadecimal type code
is_stock_code <- function(str) {
  grepl("^[0-9A-Fa-f]+$", str)
}

# Select only those which stock code values 
# to be real one
df <- subset(df, is_stock_code(StockCode))

head(df,3)

# convert invoice date info from character to Date 
df$InvoiceDate <- as.Date(df$InvoiceDate, format = "%m/%d/%Y")

# get quarter info based on the month in the year
# get_quarter <- function(date) {
# as.numeric(format(date, "%m")) %/% 3 + 1
# }

# df$Quarter <- sapply(df$InvoiceDate, get_quarter)

# built-in function for quarter info
df$Quarter <- quarters(df$InvoiceDate)

class(df$Quarter)

subset_by_quarter <- function(df, quarter){
  
  df_quarter <- subset(df, Quarter == quarter)
  df_quarter
}

df_q1 <- subset_by_quarter(df, "Q1")
df_q2 <- subset_by_quarter(df, "Q2")
df_q3 <- subset_by_quarter(df, "Q3")
df_q4 <- subset_by_quarter(df, "Q4")

# extract function
#df_q1 <- subset(df, Quarter == "Q1")
#df_q2 <- subset(df, Quarter == "Q2")
#df_q3 <- subset(df, Quarter == "Q3")
#df_q4 <- subset(df, Quarter == "Q4")


get_revenue <- function(df){
  
  df_grouped_revenue <- df %>%
    group_by(StockCode) %>%
    summarise(TotalRevenue = sum(Quantity * UnitPrice), TotalQuantity = sum(Quantity))
  
  df_grouped_revenue
} 

df_grouped_q1 <- get_revenue(df_q1)
df_grouped_q2 <- get_revenue(df_q2)
df_grouped_q3 <- get_revenue(df_q3)
df_grouped_q4 <- get_revenue(df_q4)

#extract function

#df_grouped_q1 <- df_q1 %>%
#group_by(StockCode) %>%
#summarise(TotalRevenue = sum(Quantity * UnitPrice))

#df_grouped_q2 <- df_q2 %>%
#group_by(StockCode) %>%
#summarise(TotalRevenue = sum(Quantity * UnitPrice))

#df_grouped_q3 <- df_q3 %>%
#group_by(StockCode) %>%
#summarise(TotalRevenue = sum(Quantity * UnitPrice))

#df_grouped_q4 <- df_q4 %>%
#group_by(StockCode) %>%
#summarise(TotalRevenue = sum(Quantity * UnitPrice))

sort_by_revenue <- function(df){
  
  df_sorted <- df %>%
    arrange(desc(TotalRevenue))  
  
  df_sorted
}

df_grouped_q1_sorted <- sort_by_revenue(df_grouped_q1)
df_grouped_q2_sorted <- sort_by_revenue(df_grouped_q2)
df_grouped_q3_sorted <- sort_by_revenue(df_grouped_q3)
df_grouped_q4_sorted <- sort_by_revenue(df_grouped_q4)

#df_grouped_q1_sorted <- df_grouped_q1 %>%
#arrange(desc(TotalRevenue))

#df_grouped_q2_sorted <- df_grouped_q2 %>%
#arrange(desc(TotalRevenue))

#df_grouped_q3_sorted <- df_grouped_q3 %>%
#arrange(desc(TotalRevenue))

#df_grouped_q4_sorted <- df_grouped_q4 %>%
#arrange(desc(TotalRevenue))


CLASS_A_PERCENT <- 20
CLASS_B_PERCENT <- 50
CLASS_C_PERCENT <- 30

# add class info based on the revenue in that quarter
add_class_info <- function(df){
  
  n_class_a <- ceiling(nrow(df) * CLASS_A_PERCENT / 100)
  n_class_b <- ceiling(nrow(df) * (CLASS_A_PERCENT + CLASS_B_PERCENT) / 100)
  
  df$Class <- "C"
  
  df$Class[1:n_class_a] <- "A"
  df$Class[(n_class_a + 1): n_class_b] <- "B"
  
  df
}


df_grouped_q1_sorted_with_class <- add_class_info(df_grouped_q1_sorted)
df_grouped_q2_sorted_with_class <- add_class_info(df_grouped_q2_sorted)
df_grouped_q3_sorted_with_class <- add_class_info(df_grouped_q3_sorted)
df_grouped_q4_sorted_with_class <- add_class_info(df_grouped_q4_sorted)

joined_q1_q2 <- merge(df_grouped_q1_sorted_with_class, df_grouped_q2_sorted_with_class, by = "StockCode", suffixes = c(".q1", ".q2"))
joined_q2_q3 <- merge(df_grouped_q2_sorted_with_class, df_grouped_q3_sorted_with_class, by = "StockCode", suffixes = c(".q2", ".q3"))
joined_q3_q4 <- merge(df_grouped_q3_sorted_with_class, df_grouped_q4_sorted_with_class, by = "StockCode", suffixes = c(".q3", ".q4"))

get_ascii_value <- function(char) {
  as.integer(charToRaw(char))
}

joined_q1_q2$Change <- mapply(function(x, y) get_ascii_value(x) - get_ascii_value(y), joined_q1_q2$Class.q1, joined_q1_q2$Class.q2)
joined_q2_q3$Change <- mapply(function(x, y) get_ascii_value(x) - get_ascii_value(y), joined_q2_q3$Class.q2, joined_q2_q3$Class.q3)
joined_q3_q4$Change <- mapply(function(x, y) get_ascii_value(x) - get_ascii_value(y), joined_q3_q4$Class.q3, joined_q3_q4$Class.q4)

quarter_class_map <- c("A" = 3, "B" = 2, "C" = 1)

joined_q1_q2_predicted <- joined_q1_q2 %>%
  mutate(PredictedQuantity.q2 = as.integer(TotalQuantity.q1 * (quarter_class_map[Class.q1] + Change / 2))) %>%
  select(StockCode, TotalQuantity.q1, PredictedQuantity.q2)

joined_q2_q3_predicted <- joined_q2_q3 %>%
  mutate(PredictedQuantity.q3 = as.integer(TotalQuantity.q2 * (quarter_class_map[Class.q2] + Change / 2))) %>%
  select(StockCode, TotalQuantity.q2, PredictedQuantity.q3)

joined_q3_q4_predicted <- joined_q3_q4 %>%
  mutate(PredictedQuantity.q4 = as.integer(TotalQuantity.q3 * (quarter_class_map[Class.q3] + Change / 2))) %>%
  select(StockCode, TotalQuantity.q3, PredictedQuantity.q4)

