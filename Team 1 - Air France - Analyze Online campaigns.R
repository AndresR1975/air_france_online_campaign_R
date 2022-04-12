# Functions to prepare the dataframe

# read the dataset and named as my_df as a DataFrame

library(readxl)
Air_France_Case_Spreadsheet_Supplement_Doubleclick <- read_excel("Hult - Courses/5 - Data Science R/Team Project/Air France Case Spreadsheet Supplement Doubleclick.xlsx")
my_df <- as.data.frame(Air_France_Case_Spreadsheet_Supplement_Doubleclick)                         # Secure that is a Dataframe object

###########################################################################################
# Libraries

library(ggplot2)
library(plotly)
library(splitstackshape)
library(class)
library(lattice)
library(caret)
library(rpart)
library(rpart.plot)
library(mlbench)

###########################################################################################
# function for checking for missing values

checkin_missing <- function(df){
  sum_missing <- sum(is.na.data.frame(my_df))          # calculating total of missing values
  cols_with_NA <- c()                                  # creating a vector for index with NA
  for (j in 1:ncol(df)){
    for (i in 1:nrow(df)){
      if(is.na(df[i,j])){
        cols_with_NA <- c(cols_with_NA, colnames(df[j]))
      }
    }
  }
  
  return(table(cols_with_NA))                         # retuns a table with columns that has N.A
}

checkin_missing (df = my_df)


# Just Bid Strategy has missing value. It's a categorical variable.
# No need or base to fill missing data


###########################################################################################
################## creating new variables and to improve the dataframe

#calculating the balance 

my_df$balance <- my_df$Amount - my_df$`Total Cost`

# creating a new variable for binary balance
for (i in 1:nrow(my_df)){
  if (my_df$balance[i] >0){
    my_df$balance_binary[i] <- 1} else {
      my_df$balance_binary[i] <- 0
    } # closing if
} # closing for loop

# creating a new variable for branded and unbranded keywords 
my_df$brand_unbrand <- as.numeric(grepl(pattern = "Air France|Airfrance|France Branded", x = my_df$`Keyword Group`)) 
# 1=branded keyword and 0=unbranded keyword 

for (i in 1:nrow(my_df)){
  if (my_df$`Total Volume of Bookings`[i] > 0){
    my_df$AmountperBooking[i] <- my_df$Amount[i]/my_df$`Total Volume of Bookings`[i]
  } else{
    my_df$AmountperBooking[i] <- 0
  } # closing the else if
  
} #closing the foor loop


############################################################################################

#descriptive summary of Balance

# net revenue:
positive_balance <- my_df[which(my_df$balance > 0), ] 
n_pos_balance <- nrow(positive_balance) # output -> 324
zero_balance <- my_df[which(my_df$balance == 0), ] 
n_zero_balance <- nrow(zero_balance) # output -> 0
negative_balance <- my_df[which(my_df$balance < 0), ] 
n_neg_balance <- nrow(negative_balance) # output -> 4186

# percentage of observations that generate a positive revenue:
n_pos_balance/nrow(my_df)*100  

# Conclusion:only 7.18% of campaigns generate a positive revenue


# visualizing:
balances <- c(n_pos_balance, n_neg_balance)
labls <- c("Positive", "Negative")
pie(balances, labels = labls, main="Net Revenue Generated Through Campaigns")


# total number of campaigns:
nrow(my_df) # 4510

# unique values across campaigns:
length(unique(my_df$Keyword, incomparables = FALSE)) # 2079 unique keywords
length(unique(my_df$`Keyword Group`, incomparables = FALSE)) # 348 unique keywords IDs
length(unique(my_df$`Publisher Name`, incomparables = FALSE)) # 7 unique publisher names
length(unique(my_df$`Bid Strategy`, incomparables = FALSE)) # 9 unique bid strategies
length(unique(my_df$Campaign, incomparables = FALSE)) # 24 unique types of campaigns - 3 of which are branded
length(unique(my_df$Category, incomparables = FALSE)) # 124 unique categories



############################################################################################
# Running the correlation matrix

# df_num <- Filter(is.numeric, my_df) it's no necessary to create a new dataframe until a explicity requirement 
corr_matrix <- round(cor(Filter(is.numeric, my_df), method = "pearson"),2)


#############
# we have different coefficients correlation. We can use the higher to predict certain value or
# balance 
#############



############################################################################################################
# normalization the data

# function to normalize
my_normal <- function(x){
  my_min <- min(x, na.rm = TRUE)
  my_max <- max(x, na.rm = TRUE)
  normalized <- (x-my_min)/(my_max-my_min)
  return(normalized)
}

# normalizing numeric columns
my_df$search_engin_norm         <- my_normal(x=my_df$`Search Engine Bid`)
my_df$Clicks_norm               <- my_normal(x=my_df$Clicks)
my_df$ClickChar_norm            <- my_normal(x=my_df$`Click Charges`)
my_df$avgcotperclick_norm       <- my_normal(x=my_df$`Avg. Cost per Click`)
my_df$Impressions_norm          <- my_normal(x=my_df$Impressions)
my_df$Engineclick_norm          <- my_normal(x=my_df$`Engine Click Thru %`)
my_df$Avg_pos_norm              <- my_normal(x=my_df$`Avg. Pos.`)
my_df$trans_conv_norm           <- my_normal(x=my_df$`Trans. Conv. %`)
my_df$Total_cost_tran_norm      <- my_normal(x=my_df$`Total Cost/ Trans.`)
my_df$Amount_norm               <- my_normal(x=my_df$Amount)
my_df$Total_cost_norm           <- my_normal(x=my_df$`Total Cost`)
my_df$Tot_Vol_book_norm         <- my_normal(x=my_df$`Total Volume of Bookings`)
my_df$balance_norm              <- my_normal(x=my_df$balance)
my_df$AmountperBooking_norm     <- my_normal(x=my_df$AmountperBooking)


#####################################################################################
# Creating Dummy Variables for Publisher


for (i in 1:nrow(my_df)){
  if (my_df$`Publisher Name`[i] == "Google - US"){
    my_df$dummy_GoogleUS[i] <- 1} else {
      my_df$dummy_GoogleUS[i] <- 0
    }
}




################################################################################################
# Sampling the training and testing data

# creating a variable for the stratified sampling with three stratas
# Strata 1: Positive balance
# strata 2: Negative balance with positive Amount
# Strata 3: Negative balance with zero Amount

my_df$strat <- rep(0, nrow(my_df))

for (i in 1:nrow(my_df)){
  if(my_df$balance_binary[i] ==1){my_df$strat[i] <- 2} 
  if((my_df$balance_binary[i]) ==0 & (my_df$Amount[i] >0)) {my_df$strat[i] <- 1}
  
}

training_testing <- stratified(as.data.frame(my_df),
                               group = 42, size = 0.7, bothSets = TRUE)

training_stratified <- training_testing$SAMP1
testing_stratified <- training_testing$SAMP2


#################################################################################################
############ LOGISTIC REGRESSIONS
# Use the variables with higher correlations doesn't lead to a significan logisticn regression model

# L.G. N1: Building the model using the training datset
# defining the data source

my_logit_balance_binary_no1 <- glm(balance_binary ~ `Search Engine Bid`+Clicks+`Trans. Conv. %`,  
                                   data=my_df,  
                                   family="binomial")  

summary(my_logit_balance_binary_no1) 

my_prediction_training_balanceb <- predict(my_logit_balance_binary_no1, training_stratified, type = "response")

confusionMatrix(data = as.factor(as.numeric(my_prediction_training_balanceb>0.28)),            # >0.5 is the TRUE
                reference = as.factor(as.numeric(training_stratified$balance_binary))) 


print(my_logit_balance_binary_no1$coefficients)

logit_coefficient_training <- c(unlist(my_logit_balance_binary_no1$coefficients[1]), 
                                unlist(my_logit_balance_binary_no1$coefficients[2]),
                                unlist(my_logit_balance_binary_no1$coefficients[3]), 
                                unlist(my_logit_balance_binary_no1$coefficients[4]))


#  L.G. N2: Testing the model using the testing dataset

my_prediction_testing_balanceb <- predict(my_logit_balance_binary_no1, testing_stratified, 
                                          type = "response")

confusionMatrix(data = as.factor(as.numeric(my_prediction_testing_balanceb>0.28)),            # >0.5 is the TRUE
                reference = as.factor(as.numeric(testing_stratified$balance_binary)))    # defining the data source



############
# L.G. N3: Building the model using normalized dataset

my_logit_balance_binary_norm <- glm(balance_binary ~ search_engin_norm + Clicks_norm +
                                      trans_conv_norm,  
                                    data=my_df,  
                                    family="binomial")  

summary(my_logit_balance_binary_norm) 

my_prediction_training_balanceb <- predict(my_logit_balance_binary_no1, training_stratified, 
                                           type = "response")

confusionMatrix(data = as.factor(as.numeric(my_prediction_training_balanceb>0.28)),            # >0.5 is the TRUE
                reference = as.factor(as.numeric(training_stratified$balance_binary))) 

logit_coefficient_training_norm <- c(unlist(my_logit_balance_binary_norm$coefficients[1]), 
                                     unlist(my_logit_balance_binary_norm$coefficients[2]),
                                     unlist(my_logit_balance_binary_norm$coefficients[3]), 
                                     unlist(my_logit_balance_binary_norm$coefficients[4]))


# L.G. N4: Testing the normalized model using testing dataset

my_prediction_testing_balanceb_norm <- predict(my_logit_balance_binary_norm, testing_stratified, 
                                               type = "response")

confusionMatrix(data = as.factor(as.numeric(my_prediction_testing_balanceb_norm>0.28)),            # >0.5 is the TRUE
                reference = as.factor(as.numeric(testing_stratified$balance_binary)))    # 


###################################################################  I'M HERE
# Creating the dataset of interest
# True positive predictions and false negative prediction

training_stratified$Binary_predicted <- round(my_prediction_training_balanceb,0)
testing_stratified$Binary_predicted <- round(my_prediction_testing_balanceb,0)

subset1 <- subset(training_stratified, balance_binary == 1 & Binary_predicted ==0)
subset2 <- subset(testing_stratified, balance_binary == 1 & Binary_predicted ==0)
false_negative <- rbind(subset1, subset2)

subset3 <- subset(training_stratified, balance_binary == 1 & Binary_predicted ==1)
subset4 <- subset(testing_stratified, balance_binary == 1 & Binary_predicted ==1)
tru_positive <- rbind(subset3, subset4)

df_of_interest <- rbind(tru_positive, false_negative)

table(tru_positive$`Publisher Name`)
table(false_negative$`Publisher Name`)
table(df_of_interest$`Publisher Name`)


#######################################################################################

my_df$match_factor <- as.numeric(as.factor(my_df$`Match Type`))
my_df$`Match Type`   <-as.factor(my_df$`Match Type`)
my_df$`Publisher Name` <- as.factor(my_df$`Publisher Name`)


pivot_totals <- as.data.frame(my_df %>% 
                                select(`Publisher Name`, `Search Engine Bid`, Clicks, `Click Charges`,
                                       `Avg. Cost per Click`, Impressions,`Engine Click Thru %`,`Trans. Conv. %`,
                                       `Total Cost/ Trans.`,Amount,`Total Cost`,`Total Volume of Bookings`,`Match Type`)%>% 
                                group_by(`Publisher Name`,`Match Type`) %>% 
                                summarise(Total_Bids= sum(`Search Engine Bid`), Total_Clicks = sum(Clicks),
                                          Total_Click_Charges = sum(`Click Charges`), Total_Impressions = sum(Impressions),
                                          Total_Revenue = sum(Amount), Total_Cost = sum(`Total Cost`),
                                          Total_Bookings = sum(`Total Volume of Bookings`)))

#####################################################################################################

# analyzing categorical variables and plotting:

# goals:
# maximize amount
# maximize balance
# minimize total cost

# based on results from regression model:
# maximize clicks
# maximize trans. con. %
# minimize search engine cost 


### effectiveness of brand vs unbrand
ggplot(my_df, aes(x = `brand_unbrand`, y = `Clicks`, fill=`brand_unbrand`)) +
  geom_bar(stat = "identity")
ggplot(my_df, aes(x = `brand_unbrand`, y = `Click Charges`, fill=`brand_unbrand`)) +
  geom_bar(stat = "identity")
ggplot(my_df, aes(x = `brand_unbrand`, y = `Amount`, fill=`brand_unbrand`)) +
  geom_bar(stat = "identity")
ggplot(my_df, aes(x = `brand_unbrand`, y = `Total Cost`, fill=`brand_unbrand`)) +
  geom_bar(stat = "identity")
ggplot(my_df, aes(x = `brand_unbrand`, y = `Total Cost/ Trans.`, fill=`brand_unbrand`)) +
  geom_bar(stat = "identity")
ggplot(my_df, aes(x = `brand_unbrand`, y = `balance`, fill=`brand_unbrand`)) +
  geom_bar(stat = "identity")
ggplot(my_df, aes(x = `brand_unbrand`, y = `Total Volume of Bookings`, fill=`brand_unbrand`)) +
  geom_bar(stat = "identity")

# observations: 
# While including the brand generates less clicks on average, the number of bookings made is more for these campaigns
# Click charges, average cost per trans and total cost are all significantly less for these than for campaigns that do not include brand name 
# Revenue generated and net profit are both considerably higher for campaigns including brand names

# conclusion: Brand name included in campaign -> more profitable

# recommendation: Include brand name?

### publishers:

# most effective:
ggplot(my_df, aes(x = `Publisher Name`, y = `balance`, fill=`Publisher Name`)) +
  geom_bar(stat = "identity")

# best match type for each publisher
ggplot(my_df, aes(x = `Publisher Name`, y = `balance`, fill=`Match Type`)) +
  geom_bar(stat = "identity")
# recommendation: use match type - 
# Exact for Google - Global
# Broad for Google - US, MSN global, and MSN US
# Advanced for Overture-Global and Yahoo-US
# Standard for Overture-US

# bid strategy

# best bid strategy for each publisher:
ggplot(my_df, aes(x = `Publisher Name`, y = `Clicks`, fill=`Bid Strategy`)) +
  geom_bar(stat = "identity")
ggplot(my_df, aes(x = `Publisher Name`, y = `Avg. Cost per Click`, fill=`Bid Strategy`)) +
  geom_bar(stat = "identity")
ggplot(my_df, aes(x = `Publisher Name`, y = `Amount`, fill=`Bid Strategy`)) +
  geom_bar(stat = "identity")
ggplot(my_df, aes(x = `Publisher Name`, y = `Total Cost`, fill=`Bid Strategy`)) +
  geom_bar(stat = "identity")
ggplot(my_df, aes(x = `Publisher Name`, y = `balance`, fill=`Bid Strategy`)) +
  geom_bar(stat = "identity")
# observations:
# Google US -> most impactful -> generates most clicks and highest revenues
# but also, airfrance spents most money on it
# how to minimize costs?
# currently Position 5-10 is most commonly used - has high costs, and also contributes most to losses
# Position 1-4 generates most revenue, and profits
#  recommendation: Reallocate budget from Position 5-10 to Position 1-4 for Google US


### Match Type, Bran_unbrand and Balance

ggplot(my_df, aes(x=`Match Type`, y=`brand_unbrand`)) +
  geom_raster(aes(fill = `balance`))



#############################################################################
############ plotting variables  NOT APPROVED YET

# HISTOGRAM REVENUE PER PUBLISHER
# Amount per publisher
# original plot
ggplot(my_df, aes(x = `Publisher Name`, y = `Amount`, fill=`Match Type`)) +
  geom_bar(stat = "identity")
# ggplotly
plot_amount_perpubliserh <- ggplot(my_df, aes(x = `Publisher Name`, y = `Amount`, fill=`Match Type`)) +
                              geom_bar(stat = "identity")
ggplotly(plot_amount_perpubliserh)


# Balance per publisher
# original plot
ggplot(my_df, aes(x = `Publisher Name`, y = `balance`, fill=`Match Type`)) +
  geom_bar(stat = "identity")

plot_balanceperpublisher <- ggplot(my_df, aes(x = `Publisher Name`, y = `balance`, fill=`Match Type`)) +
                            geom_bar(stat = "identity")
ggplotly(plot_balanceperpublisher)


# Clicks per publisher
ggplot(my_df, aes(x = `Publisher Name`, y = `Clicks`, fill=`Match Type`)) +
  geom_bar(stat = "identity")

plot_balanceperpublisher <- ggplot(my_df, aes(x = `Publisher Name`, y = `Clicks`, fill=`Match Type`)) +
  geom_bar(stat = "identity")
ggplotly(plot_balanceperpublisher)


# Total Cost per publisher
ggplot(my_df, aes(x = `Publisher Name`, y = `Total Cost`, fill=`Match Type`)) +
  geom_bar(stat = "identity")

plot_balanceperpublisher <- ggplot(my_df, aes(x = `Publisher Name`, y = `Total Cost`, fill=`Match Type`)) +
  geom_bar(stat = "identity")
ggplotly(plot_balanceperpublisher)



# CAMPAIGN SEGMENTATION

plot_campaign <- ggplot(my_df, aes(x = `Publisher Name`, y = `Campaign`)) +
                geom_count(stat = "sum", position = "identity", aes(color = ..n..))+
                guides(color = 'legend')
ggplotly(plot_campaign)



# Match Type and Balance

ggplot(my_df, aes(x = `Match Type`, y = `balance`)) +
  geom_bar(stat = "identity")



