machine_data <- fread(file = '~/Desktop/final_data/machine_data.csv',
                      sep = ',',
                      dec = '.',
                      header = TRUE)
machine_data <- setDT(machine_data)
machine_data

#exercise1:
#Machines:

#How many machines are there?
nrow(machine_data) #2495


#What percentage of them are small?

aux <- machine_data[small_machine == 1]
perc_small_machines = (nrow(aux)/2495)*100
perc_small_machines
#or
(sum(machine_data$small_machine)/2495)*100 #38.44%

#How are they distributed in terms of location type?
unique(machine_data$location_type)
aux <- machine_data[complete.cases(location_type)]
unique(aux$location_type)
x = nrow(aux[location_type == 'others']) #691
y = nrow(aux[location_type == 'transport']) #1460
z = nrow(aux[location_type == 'petrol station']) #343

#products:

product_data <- fread(file = '~/Desktop/final_data/product_data.csv',
                      dec = '.',
                      sep = ',',
                      header = TRUE)
product_data <- setDT(product_data)
product_data

#How many products are there? Which category has the highest number of products?
n_of_prod <- unique(nrow(product_data))
n_of_prod #63

unique(product_data$category)
nrow(product_data[category == 'Water'])                          #5
nrow(product_data[category == 'Milk based'])                     #4
nrow(product_data[category == 'Salty'])                          #8
nrow(product_data[category == 'Juice, tea and smoothies'])       #8
nrow(product_data[category == 'Carbonates and energy drinks'])   #13
nrow(product_data[category == 'Cookies, pastries and cereals'])  #7
nrow(product_data[category == 'Sugar candy'])                    #7
nrow(product_data[category == 'Chocolate based'])                #11

#the category with the highest number of products is Carbonates and energy drinks

#Which category has the highest and lowest average price? And within snacks or drinks?
product_data

aux1 <- product_data[category == 'Water']
mean(aux1$price) #3.26
aux2 <- product_data[category == 'Milk based']
mean(aux2$price) #3.43
aux3 <- product_data[category == 'Salty']
mean(aux3$price) #2.73
aux4 <- product_data[category == 'Juice, tea and smoothies']
mean(aux4$price) #2.86
aux5 <- product_data[category == 'Carbonates and energy drinks']
mean(aux5$price) #3.26
aux6 <- product_data[category == 'Cookies, pastries and cereals']
mean(aux6$price) #2.39
aux7 <- product_data[category == 'Sugar candy']
mean(aux7$price) #2.30
aux8 <- product_data[category == 'Chocolate based']
mean(aux8$price) #2.31
#the average price within category of products goes for Milk based

aux <- product_data[type_drink_snack == 'drink']
mean(aux$price) #3.18
aux <- product_data[type_drink_snack == 'snack']
mean(aux$price) #2.42
#drinks are on average more expensive than snacks

#Transactional data:

transactional_data <- fread(file = '~/Desktop/final_data/transactional_data.csv',
                      sep = ',',
                      dec = '.',
                      header = TRUE)

transactional_data <- setDT(transactional_data)
transactional_data

#Restricting the transactional data to March 2017, what's the average daily items
#among small and big machines? Why do you think there is such a difference?
#Give at least 2 reasons. 

aux <- transactional_data[, Month_Yr := format(date, '%Y-%m')]
aux <- aux[Month_Yr == '2017-03']
Active_Days <- aux[, length(unique(date)), machine]
Active_Days
n_trans <- count(aux, machine)
n_trans
aux1 <- merge(n_trans, Active_Days, by = 'machine')
Machine_Data_Compl_March <- merge(machine_data, aux1, by = 'machine')
summary(Machine_Data_Compl_March)
S_M <- Machine_Data_Compl_March[small_machine == 1]
B_M <- Machine_Data_Compl_March[small_machine == 0]
S_M <- S_M[, Avg_Daily_Items := n/V1]
S_M
B_M <- B_M[, Avg_Daily_Items := n/V1]
B_M
mean(S_M$Avg_Daily_Items) #7.7
mean(B_M$Avg_Daily_Items) #9.9
auxx <- merge(transactional_data, machine_data, by = 'machine')
auxx <- setDT(auxx)
r <- count(auxx, location_type)
r
q <- count()
library(dplyr)
unique(auxx$machine)


  #exercise2:

#Is there a general trend in the number of snacks
#and drinks as the months progress from January
#to April? Is it the same for snacks and drinks? Why 
#do you think that might be so?

#There appears to be a constant and stable trend in the amount of snacks
#sold per day with no general variation. Oppositely, if we look at the drinks
#we can see that the closer we get to summer the more drinks are sold per day. 
#This is probably due to the increase in temperatures which leads to a higher 
#willingness to buy a drink at a vendor machine.

#Is there a shorter time period trend as well? Is it the same for snacks
#and drinks? What do you think might be the cause?

#In the shorter term the sales for both snacks and drinks seem to be very
#volatile. This is due to the fact that most of the machines are located close
#to means of transportation, therefore these swings coincide with people commuting/
#travelling only during certain days of the week.

#exercise3:

summary(machine_data$income_average)

################Create new data table for model################################
Tot_Active_Days <- transactional_data[, length(unique(date)), machine]
Tot_Active_Days
Tot_N_Trans <- count(transactional_data, machine)
Tot_N_Trans
aux <- merge(Tot_N_Trans, Tot_Active_Days, by = 'machine')
Machine_Data_Compl <- merge(machine_data, aux, by = 'machine')
Machine_Data_Compl <- Machine_Data_Compl[, Avg_Daily_Items := n/V1]
Machine_Data_Compl
new_dt <- merge(Machine_Data_Compl, transactional_data, by = 'machine')
new_dt <- new_dt[, week_days := weekdays(date)]
x <- count(new_dt, week_days)
x
hist(x$n)
help("hist")
###############################################################################

#Are there outliers? How would you treat them? Provide code with your answer
#Can you give three possibilities on how to treat the NA cases? Which option would
#you choose and why? Provide code with your answer.

Machine_Data_Compl$income_average[is.na(Machine_Data_Compl$income_average)] <- median(Machine_Data_Compl$income_average, na.rm = TRUE)
Machine_Data_Compl$income_average[is.na(Machine_Data_Compl$income_average)] <- mean(Machine_Data_Compl$income_average, na.rm = TRUE)
#drop them
arcvi_descriptive(Machine_Data_Compl$income_average, Machine_Data_Compl$Avg_Daily_Items)

#by getting rid of the outliers with the median we get rid of the huge decrease in the target variable due 
#to this class. Also we do not take into account the noise created by outliers.

boxplot(machine_data$income_average)
x <- quantile(Machine_Data_Compl$income_average, 0.89)
x
y <- quantile(Machine_Data_Compl$income_average, 0.015)
y
aux <- Machine_Data_Compl[income_average < x & income_average > y]
boxplot(aux$income_average)
nrow(aux)
nrow(Machine_Data_Compl)
Machine_Data_Compl <- Machine_Data_Compl[income_average < x & income_average > y]
is.na(machine_data$income_average)
boxplot(Machine_Data_Compl$income_average)

#look at the boxplot and how it changes when we get rid of extreme values. In the end, we decided to get
#rid of all the values below the 1st percentile and above the 90th.


#Exercise5:

#Do all variables show statistical significance? Which ones doesn’t? How do you
#know? 


Machine_Data_Compl <- Machine_Data_Compl[, New_Var := ifelse(is.na(train_AvgDailyPassengers),1,0)]
length(Machine_Data_Compl$income_average)

model1 <- glm(Avg_Daily_Items ~ small_machine +
                income_average +
                total_number_of_routes_600 +
                num_hotels_45 +
                New_Var +
                num_vendex_nearby_300, data = Machine_Data_Compl)
summary(model1)
#by looking at the p-values of the given coefficients of the model built
#all the coefficients are statistically significant with a p-value
#lower than 0.001. The only 3 variables that are not significant are the 
#total number of routes, the number of Vendex machines nearby and the income average

#Build another linear model but this time instead of using the variables
#“total_number_of_routes_600 use the log of that variable in base 10 calling it
#“log_transport”. Does this new variable show statistical significance?

Machine_Data_Compl <- Machine_Data_Compl[, log_transport := log(total_number_of_routes_600)]

model2 <- glm(Avg_Daily_Items ~ small_machine +
                income_average +
                log_transport +
                num_hotels_45 +
                New_Var +
                num_vendex_nearby_300, data = Machine_Data_Compl)
summary(model2)

#by using the logarithm of the previous variable now all the coefficients are statistically significant,
#apart from income average

#c
#all factors being equal, small machines tend to sell 1.7 items less compared to big machines on average on 
#daily basis
#d
#the effect on sales by nearby machines is negative, exactly equal to -0.1128 for every average unit in sales
#e
final_model <- glm(Avg_Daily_Items ~ small_machine +
                log_transport +
                num_hotels_45 +
                New_Var +
                num_vendex_nearby_300, data = Machine_Data_Compl)
summary(final_model)
x <- predict(model2, newdata = Machine_Data_Compl)
x <- sort(x, decreasing = TRUE)
Twenty_Percent = round(length(x)*0.2)
Twenty_Percent
Top <- x[1:Twenty_Percent]
Bottom <- x[(length(x)-Twenty_Percent):length(x)]           
Agg_sales_Top <- sum(Top)
Agg_sales_Bottom <- sum(Bottom)
Ratio_Top_Bottom <- Agg_sales_Top/Agg_sales_Bottom #2.18

Prediction_Data <- data.frame(small_machine = c(0,0), 
                              log_transport = c(log(20), log(10)),
                              num_hotels_45 = c(2,0),
                              New_Var = c(1,0),
                              num_vendex_nearby_300 = c(0,3))

predict(final_model, newdata = Prediction_Data)

#I would choose to allocate the Vendex Machine in the first location
#because the estimated average daily sales are of 9.84, while of only
#8.51 for the former one
