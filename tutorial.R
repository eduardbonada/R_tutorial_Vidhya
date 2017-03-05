# R Tutorial - Analytics Vidhya
#
# https://www.analyticsvidhya.com/blog/2016/02/complete-tutorial-learn-data-science-scratch/#one

#### FEATURES DESCRIPTION ####

# Item_Identifier               Unique product ID
# Item_Weight                   Weight of product
# Item_Fat_Content              Whether the product is low fat or not
# Item_Visibility               The % of total display area of all products in a store allocated to the particular product
# Item_Type                     The category to which the product belongs
# Item_MRP                      Maximum Retail Price (list price) of the product
# Outlet_Identifier             Unique store ID
# Outlet_Establishment_Year     The year in which store was established
# Outlet_Size                   The size of the store in terms of ground area covered
# Outlet_Location_Type          The type of city in which the store is located
# Outlet_Type                   Whether the outlet is just a grocery store or some sort of supermarket
# Item_Outlet_Sales             Sales of the product in the particulat store. This is the outcome variable to be predicted.

#### LOAD DATA ####

train <- read.csv("Train.csv")
test <- read.csv("Test.csv")


#### EXPLORE DATA ####

# check dimesions in data set
dim(train)
dim(test)

# check the variables and their types
str(train)
#str(test)

# Get some insights from the data
summary(train)
# Item_Fat_Content needs a review of the levels
# Item_Visibility has 0 value and this is not possible
# Outlet_Size has some empty levels
# Item weight has 1463 NA values

# inspect missing values globally
table(is.na(train))
# there is 1463 NA 

# inspect missing values per column
colSums(is.na(train)) 
# Item_Weight has all 1463 NA

library(ggplot2)

# scatter plot to see how visibility affects
ggplot(train, aes(x= Item_Visibility, y = Item_Outlet_Sales)) +
    geom_point(size = 2.5, color="navy") + xlab("Item Visibility") +
    ylab("Item Outlet Sales") + 
    ggtitle("Item Visibility vs Item Outlet Sales")
# Item_Visibility < 0.2 has much better Item_Outlet_Sales

# bar plot to see which outlet sells more
ggplot(train, aes(Outlet_Identifier, Item_Outlet_Sales)) + 
    geom_bar(stat = "identity", color = "purple") +
    theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
    ggtitle("Outlets vs Total Sales") + 
    theme_bw()
# there is two outlets with very low sales (OUT010, OUT019)
# there is one outlet selling more (OUT027)

# bar plot to see how different types sell
ggplot(train, aes(Item_Type, Item_Outlet_Sales)) + 
    geom_bar( stat = "identity") +
    theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "navy")) + 
    xlab("Item Type") + 
    ylab("Item Outlet Sales")+
    ggtitle("Item Type vs Sales")
# Fruits & Vegetables and Snack Foods sell more

# box plot to see how different types are priced
ggplot(train, aes(Item_Type, Item_MRP)) +
    geom_boxplot() +
    ggtitle("Box Plot") + 
    theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "red")) + 
    xlab("Item Type") + 
    ylab("Item MRP") + 
    ggtitle("Item Type vs Item MRP")

#### FIX MISSING VALUES ####

# combine train and test to avoid repeating the fix operations
test$Item_Outlet_Sales <- 1  # add missing 'y' column in test data set
combi <- rbind(train, test)  # combine the two data sets

# fill missing Item_Weight with the median of the rest of values
combi$Item_Weight[is.na(combi$Item_Weight)] <- median(combi$Item_Weight, na.rm = TRUE)
table(is.na(combi$Item_Weight))  # check there is no NA values any more

# fill Item_Visibility = 0 with the median
combi$Item_Visibility <- ifelse(combi$Item_Visibility == 0,
                                median(combi$Item_Visibility), combi$Item_Visibility)
table(combi$Item_Visibility == 0)  # check there is no 0 values any more

# correct empty Outlet_Size
levels(combi$Outlet_Size)[1] <- "Other"

# correct (rename) Item_Fat_Content levels
library(plyr)  # Tools for Splitting, Applying and Combining Data
combi$Item_Fat_Content <- revalue(combi$Item_Fat_Content,
    c("LF" = "Low Fat", "reg" = "Regular", "low fat" = "Low Fat"))  # rename levels
table(combi$Item_Fat_Content)  # check we only have two levels

#### FEATURE ENGINEERING ####

library(dplyr)  # Data manipulation library (similar to SQL joins, etc)

# Add a variable that contains the number of items sold by that Outlet (Outlet_Count)
# Identifying best-selling outlets might be a good way to predict sales of an item
# Transform factor variables into counts is useful
outlet_count <- combi %>% group_by(Outlet_Identifier) %>%  tally()  # group by Outlet_Identifier and then sum (like a SQL group by clause)
names(outlet_count)[2] <- "Outlet_Count" # set the name of the 'n' column
combi <- full_join(outlet_count, combi, by = "Outlet_Identifier")  # join the combi data_set and the count data (like a SQL join)

# Add a variable that contains the number of times that an item is being sold in all Outlets (Item_Count)
# Identifying those products sold more frequently might be a good way to predict sales of an item
item_count <- combi %>% group_by(Item_Identifier) %>%  tally()  # group by Item_Identifier and then sum (like a SQL group by clause)
names(item_count)[2] <- "Item_Count" # set the name of the 'n' column
combi <- merge(item_count, combi, by = 'Item_Identifier')  # merge item_count into combi
#combi <- full_join(item_count, combi, by = 'Item_Identifier')  # why not a full_join as we did  before?

# Transform date-related features: Establishment Year => Years active
active_years <- combi %>% distinct(Outlet_Establishment_Year) %>% mutate(Active_Years = 2013 - Outlet_Establishment_Year)  # select unique establishment years and change them
combi <- full_join(active_years, combi, by = 'Outlet_Establishment_Year')

# Create a new feature that identifies if item is 'Food', 'Drink' or 'Non-Consumable' (using the first two characters of Item_Identifier)
# This can also be done reducing Item_Type into only three levels (Fod, Drink, Non-Consumable)
# The idea is to reduce the categorical variables into less levels
type <- substr(combi$Item_Identifier,1,2)  # substring of two first characters
type <- gsub('FD', 'Food', type)  # substitute strings
type <- gsub('DR', 'Drink', type)  # substitute strings
type <- gsub('NC', 'Non-Consumable', type)  # substitute strings
table(type)  # check everything
table(combi[combi$Item_Type_New == 'Food',]$Item_Type)  # check it matches with Item_Type
table(combi[combi$Item_Type_New == 'Drink',]$Item_Type)  # check it matches with Item_Type
table(combi[combi$Item_Type_New == 'Non-Consumable',]$Item_Type)  # check it matches with Item_Type
combi$Item_Type_New <- type  # finally add colum

# Encode the categorical variables into numerical: Factors of two levels => just make it binary
combi$Item_Fat_Content <- ifelse(combi$Item_Fat_Content == "Regular",1,0)

# Encode the categorical variables into numerical: Factors of more levels => one-hot encoding
# commenting out because it is not needed for the linear model (the same lm call does it itself)
# library(dummies)
# combi <- dummy.data.frame(combi, names = c('Outlet_Size','Outlet_Location_Type','Outlet_Type', 'Item_Type_New'),  sep='_')  # performs one-hot encoding and removes original

# let's see if we find out some correlated features that are giving us some modelling difficulties
# cor(new_train)
# there are some variables uncorrelated (specially Outlet_Count and Outlet_Type_Grocery Store)

#### LINEAR REGRESSION ####

# Drop columns of features that we will not use for the prediction (alreeady converted to other columns, identifier columns, not significant...)
combi_LR <- select(combi, -c(Item_Identifier, Outlet_Identifier, Outlet_Establishment_Year, Outlet_Count, Item_Count, Item_Type_New))  # Item_Fat_Content, Item_Type
# str(combi1)

# split back the combi data frame into train and test
new_train <- combi_LR[1:nrow(train),]
new_test <- combi_LR[-(1:nrow(train)),]

# Create Linear Regression Model
linear_model <- lm(Item_Outlet_Sales ~ ., data = new_train)
summary(linear_model)

# Create some plots to better interpret linear model
plot(linear_model)
