library(stringr)
library(lubridate)
library(tidyverse)
library(modelr)
library(sp)
library(leaflet)
library(geosphere)
library(knitr)
library(rpart)

df = read.csv("transactiondata.csv")

summary(df)
str(df)

# change the format of date column
df$date<- as.Date(df$date,format = "%d/%m/%Y")

# the dateset only contain records for 91 days, one day is missing
DateRange <- seq(min(df$date), max(df$date), by = 1)
DateRange[!DateRange %in% df$date]

# 2018-08-16 transactions are missing
# derive weekday and hour data of each transaction
df$extraction = as.character(df$extraction)
df$hour = hour(as.POSIXct(substr(df$extraction,12,19),format="%H:%M:%S"))
df$weekday = weekdays(df$date)

# confirm the one -to -one link of account_id and customer_id
df %>% select(account,customer_id) %>%
  unique() %>%
  nrow()
# split customer & merchant lat_long into individual columns for analysis
dfloc = df[,c("long_lat","merchant_long_lat")]
dfloc<- dfloc %>% separate("long_lat", c("c_long", "c_lat"),sep=' ')
dfloc<- dfloc %>% separate("merchant_long_lat", c("m_long", "m_lat"),sep=' ')
dfloc<- data.frame(sapply(dfloc, as.numeric))
df <- cbind(df,dfloc)

# check the range of customer location
# filtering out transactions for those who don't reside in Australia
# Reference: http://www.ga.gov.au/scientific-topics/national-location-information/dimensions/con
tinental-extremities
df_temp <- df %>%
  filter (!(c_long >113 & c_long <154 & c_lat > (-44) & c_lat < (-10)))
length(unique(df_temp$customer_id))

# check the distribution of missing values
apply(df, 2, function(x) sum(is.na(x)| x == ''))
# check the number of unique values for each column
apply(df, 2, function(x) length(unique(x)))


# filtering out purchase transactions only
# assuming purchase transactions must be associated with a merchant (have a merchant Id)
df_temp <- df %>% filter(merchant_id != '' )

# it turned out that is equivilent to excluding following categories of transactions
df_csmp <- df %>%filter(!(txn_description %in% c('PAY/SALARY',"INTER BANK", "PHONE BANK","PAYMEN
T")))
summary(df_csmp)

# visualise the distribution of transaction amount
hist(df_csmp$amount[!df_csmp$amount %in% boxplot.stats(df_csmp$amount)$out], #exclude outliers
     xlab= 'Transaction Amount', main = 'Histogram of purchase transaction amount')


# Visualise customers’average monthly transaction volume.

df2 <- df %>%
  group_by(customer_id) %>%
  summarise(mon_avg_vol = round(n()/3,0))
hist(df2$mon_avg_vol,
     xlab= 'Monthly transaction volume', ylab='No. of customers', main = "Histogram of customer
s' monthly transaction volume")

# Visualise transaction volume over an average week.
df3 <- df %>%
  select(date,weekday) %>%
  group_by(date,weekday) %>%
  summarise(daily_avg_vol = n()) %>%
  group_by(weekday) %>%
  summarise(avg_vol=mean(daily_avg_vol,na.rm=TRUE ))
df3$weekday <- factor(df3$weekday, levels=c( "Monday","Tuesday","Wednesday",
                                             "Thursday","Friday","Saturday","Sunday"))
ggplot(df3,aes(x=weekday, y=avg_vol)) +geom_point()+geom_line(aes(group = 1))+
  ggtitle('Average transaction volume by weekday') +
  labs(x='Weekday',y='Transaction volume')

# visualize transaction volume over an average week.
df4 <- df %>%
  select(date,hour) %>%
  group_by(date,hour) %>%
  summarize(trans_vol=n()) %>%
  group_by(hour) %>%
  summarize(trans_vol_per_hr = mean(trans_vol,na.rm=TRUE))
ggplot(df4,aes(x=hour,y=trans_vol_per_hr))+geom_point()+geom_line(aes(group = 1))+
  ggtitle('Average transaction volume by hour') +
  labs(x='Hour',y='Transaction volume') + expand_limits( y = 0)

# exclude the single foreign customer whose location information was incorrectly stored (i.e latitude 573)

df_temp <- df_csmp %>%
  filter (c_long >113 & c_long <154 & c_lat > (-44) & c_lat < (-10))
dfloc = df_temp [,c("c_long", "c_lat","m_long", "m_lat")]
dfloc<- data.frame(sapply(dfloc, as.numeric))
dfloc$dst <- distHaversine(dfloc[, 1:2], dfloc[, 3:4]) / 1000
hist(dfloc$dst[dfloc$dst<100], main = "Distance between customer and merchants",xlab= 'Distance
(km)' )

#To validate, we could further plot the location of the customer and the merchants he/she trades with on a map.

merch_dist <- function (id ){
  ### This function takes in a customer Id and plot the location of the customer and all
  ### merchants he/she have traded with.
  cus_icon<- makeAwesomeIcon(icon = 'home', markerColor = 'green')
  l = subset (df_csmp[,c("customer_id","m_long","m_lat")], customer_id == id)
  l <- l[c("m_long","m_lat")]
  cus_loc <- unique(subset (df_csmp[,c("customer_id","long_lat")], customer_id == id))
  cus_loc <- cus_loc %>% separate("long_lat", c("long", "lat"),sep=' ')
  df_t = data.frame(longtitude = as.numeric(l$m_long), latitude = as.numeric(l$m_lat))
  coordinates(df_t) <- ~longtitude+latitude
  leaflet(df_t) %>% addMarkers() %>% addTiles() %>%
    addAwesomeMarkers(
      lng=as.numeric(cus_loc$long), lat=as.numeric(cus_loc$lat),
      icon = cus_icon)
}


# firstly check the salary payment frequency of each customer
df_inc = data.frame(customer_id= unique(df_csmp$customer_id)) 

#create a data frame to store result

# create a mode function that will be used to find out what is the salary payment frequency
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# loop through all salary payment for each customer
# assume the salary level is constant for each customer over the observed period
for (i in seq(nrow(df_inc))){
  trans_data <- df[df$customer_id == df_inc$customer_id[i]
                   & df$txn_description=='PAY/SALARY',c("amount","date")] %>%
    group_by(date) %>%
    summarise(amount = sum(amount))
  total_s <- sum(trans_data$amount)
  count = dim(trans_data)[1]
  if ( count == 0){
    df_inc$freq[i] = NA
    df_inc$level[i] = NA
  } else {
    s=c()
    lvl = c()
    for (j in seq(count-1)){
      s = c(s,(trans_data$date[j+1]-trans_data$date[j]))
      lvl = c(lvl,trans_data$amount[j])
    }
    lvl = c(lvl,tail(trans_data$amount,n=1))
    df_inc$freq[i] = Mode(s)
    df_inc$level[i] = Mode(lvl)
  }
}
df_inc$annual_salary= df_inc$level / df_inc$freq *365.25

# visualise the distribution of customers' annual salary
hist(df_inc$annual_salary[!is.na(df_inc$annual_salary)],breaks=c(seq(28000,140000,by = 10000)),
     main = "Histogram of customers' annual salary", xlab= 'Income($)')


df_cus <-df_csmp %>% # use df_csmp to summarize customers' consumption behavior
  select (customer_id,gender,age,amount,date,balance) %>%
  group_by(customer_id) %>%
  mutate(avg_no_weekly_trans= round(7*n()/length(unique(df$date)),0),max_amt = max(amount),
         no_large_trans = sum(amount>100), # an arbitrary $100 benchmark is selected based on the
         # transaction amount histogram created in task 1.3
         use_no_day=length(unique(date)),
         avg_trans_amt = mean(amount, na.rm =TRUE),
         med_bal = median(balance,na.rm=TRUE)) %>%
  select(-c("amount","date","balance")) %>%
  unique()
# create additional features
df_cus$age_below20 <- ifelse(df_cus$age<20,1,0)
df_cus$age_btw20n40 <- ifelse(df_cus$age>=20 & df_cus$age <40,1,0)
df_cus$age_btw40n60 <- ifelse(df_cus$age>=40 & df_cus$age <60,1,0)
# investigate the state where customers live
# assume they live where most transactions occured (indicated by merchant_state)
df_region <-df_csmp %>%
  group_by(customer_id,merchant_state) %>%
  summarize(trans_count=n()) %>%
  group_by(customer_id) %>%
  mutate (no_state = n()) %>%
  filter(trans_count == max(trans_count))
# For equal number of transactions between multiple States, pick the most likely State
n_occur = data.frame(table(df_region$customer_id))
cus_id_rep = n_occur$Var1[n_occur$Freq > 1]
state_by_cust_no <- rev(names(sort(table(df_region$merchant_state),rev = TRUE)))
t = data.frame(customer_id = cus_id_rep, merchant_state=NA)
for (i in seq(length(cus_id_rep))){
  s = df_region$merchant_state[df_region$customer_id == cus_id_rep[i]]
  for (state in state_by_cust_no){
    if (state %in% s){
      t[i,2] = state
      break
    }
  }
}
df_region <- df_region[!(df_region$customer_id %in% cus_id_rep), c(1,2)] %>%
  as.data.frame() %>%
  rbind(t) %>%
  rename( State = merchant_state)
# merge all the features into single dataframe
df_cus <- df_cus %>% merge(df_inc) %>%
  merge(df_region)
# extract relevant features
df_cus_attr <- df_cus %>%
  select("gender","annual_salary","age","avg_no_weekly_trans","max_amt",
         "no_large_trans", "use_no_day","avg_trans_amt","med_bal","State")
plot(df_cus_attr)