# Title       : Homework Formatting Data.R
# Description : This is an online retail script and its formatting process with RFM method.
# Objective   : To format data (after cleaning)
# Data source : https://archive.ics.uci.edu/ml/datasets/online+retail


# load library
library(tidyverse)
library(lubridate)
library(DataExplorer)

# load data
online_retail <- read.csv("C:/Users/10460/Documents/R/hwcleansingformatting/Online_Retail.csv", header=TRUE, sep=";")

# preview first 6 rows of data
head(online_retail)

# preview last 6 rows of data
tail(online_retail)

# summary of the data
summary(online_retail)

plot_missing(online_retail)

# Set benih random
set.seed(10)

# sampling data
ol_sample <- onlineretail[sample(1:nrow(onlineretail), size=10000),]
summary(ol_sample)

# We are going to clean ol_sample data.

# checking variable data
str(ol_sample)
plot_str(ol_sample)

# From the output, are the variables in the right format?

# Check out the InvoiceDate, it is in character type format, it should be in Date datatype. You have to convert it to date friendly format. Find out the documentation of lubridate package on Help!

# Convert the data type for InvoiceDate variable from chr to Date (POSIXct)
ol_sample$InvoiceDate <- dmy_hm(ol_sample$InvoiceDate)

# Are there any missing data?
plot_missing(ol_sample)

# Customer ID have 25% missing value. What are you going to do with that? Please ask your data owner! (in this case, we think that those empty data because the customer has no customer ID or bought as a guest)

# if you want to drop, simply use this command:
ol_sample_drop <- ol_sample[!is.na(ol_sample$CustomerID),]

# if you want to keep rows with empty customerID, simply use this command, replace with unique value that did not yet input.
max_CustID <- max(ol_sample[!is.na(ol_sample$CustomerID),]$CustomerID)
ol_sample_imput <- ol_sample
ol_sample_imput$CustomerID[is.na(ol_sample_imput$CustomerID)] <- sample(max_CustID+10000:max_CustID+10000+length(ol_sample$CustomerID), size=sum(is.na(ol_sample_imput$CustomerID)), replace=F)
length(ol_sample[is.na(ol_sample$CustomerID),])

# check missing data again
plot_missing(ol_sample_drop)
plot_missing(ol_sample_imput)

# Quiz      : Please make a tidy table from produk, transaksi, and profil_pelanggan table, thus contain the following variables using ol_sample_drop data frame:
# CustomerID | Recency | Frequency | Amount
# Recency   : jumlah hari ini s.d. terakhir bertransaksi (dalam hari)
# Frequency : jumlah transaksi yang terjadi dalam 6 bulan terakhir
# Monetary  : jumlah uang yang dibelanjakan oleh Customer ID unik

frequency <- ol_sample_drop %>% group_by(CustomerID) %>% summarise(frequency = n_distinct(InvoiceNo))

monetary <- ol_sample_drop %>% group_by(CustomerID) %>% summarise(monetary=sum(UnitPrice*Quantity))
recency <- ol_sample_drop %>% group_by(CustomerID) %>% arrange(desc(InvoiceDate)) %>%   filter(row_number()==1) %>% mutate(recency = as.numeric(as.duration(interval(InvoiceDate,ymd("2011-12-31"))))/86400) %>% select(CustomerID, recency)

# Quiz join ketiga nya.
# (hint: dengan menggunakan left_join function)

df_rfm <- recency %>% left_join(frequency,by="CustomerID") %>% left_join(monetary,by="CustomerID")

write.csv(df_rfm,"online_retail_clean.csv",quote=F,row.names = F)

# Check summary df_rfm, can you check the median of each recency, frequency, and monetary?
summary(df_rfm)

# Median of recency, frequensi, dan monetary known.

# Encode with this condition: If more than median, then high, if less than median then low, and assign as variable: df_rfm_encod

med_recency <- median(df_rfm$recency)
med_freq <- median(df_rfm$frequency)
med_monetary <- median(df_rfm$monetary)

df_rfm_encod <- df_rfm %>% mutate(recency=ifelse(recency<=med_recency,0,1),
                                  frequency=ifelse(frequency<=med_freq,0,1),
                                  monetary=ifelse(monetary<= med_monetary,0,1))

write.csv(df_rfm_encod,"online_retail_cluster.csv",quote = F,row.names = F)


# Create new dataframe which consists of customerID | country
origin <- ol_sample_drop %>% select(CustomerID, Country) %>% distinct()

# Join with df_rfm_encod with origin data frame and assign as df_rfm_full

df_rfm_full <- df_rfm_encod %>% left_join(origin,by="CustomerID")

# What are top 3 countries which have the most low recent and high frequency customers?
country <- df_rfm_full %>% filter(recency==0 & frequency==1) %>% group_by(recency,frequency,Country) %>% summarise(total_customer = n_distinct(CustomerID))  %>% arrange(desc(total_customer)) %>% top_n(3)

#################Bonus################

# We would like to change country column from United Kingdom into DKI Jakarta, Germany to Jawa Barat, EIRE to Banten, and Spain to Jawa Tengah, France to DI Yogyakarta, Sqitzerland to Jawa Timur, others to Luar Jawa, we can also use mutate to create Province column.

# select, filter, mutate, and group_by in one shot! :)
profil_pelanggan <- ol_sample_drop %>%
  select(CustomerID,Country) %>%
  mutate(Province=
           ifelse(Country=='United Kingdom','DKI Jakarta',
                  ifelse(Country=='Germany','Jawa Barat',
                         ifelse(Country=='EIRE','Banten',ifelse(
                           Country=='Spain','Jawa Tengah',
                           ifelse(Country=='France','DI Yogyakarta',
                                  ifelse(Country=='Switzerland','Jawa Timur','Luar Jawa'))))))) %>%
  select(CustomerID, Province) %>% unique() %>% na.omit()

produk <- ol_sample_drop %>% select(StockCode,Description,UnitPrice) %>% distinct() %>% mutate(UnitPrice2=20000*UnitPrice) %>% select(StockCode,Description,UnitPrice2) %>% group_by(StockCode) %>% filter(row_number()==1)

transaksi <- ol_sample_drop %>% select(InvoiceDate,InvoiceNo,StockCode,Quantity,UnitPrice,CustomerID) %>% distinct() %>% filter(CustomerID>10000)

# write into table
write.csv(transaksi,"transaksi.csv",row.names = F)
write.table(produk,"produk.csv",row.names = F,sep = "|",quote = F)
write.csv(profil_pelanggan,"profile_pelanggan.csv",row.names = F,quote=F)

# Exercise of tidyr (changing profil_pelanggan from long to wide)
profil_pelanggan$jumlah <- 1
profil_pelanggan_to_wide <- spread(profil_pelanggan,Province,jumlah)

# Exercise of tidyr (changing profil_pelanggan from wide to long)
profil_pelanggan_to_long <- gather(profil_pelanggan_to_wide,Province,Jumlah,-CustomerID,na.rm = T)
head(profil_pelanggan_to_long)

data_rfm_onlineretail <- ol_sample_drop %>%
  select(CustomerID,recency, frequency, monetary) %>% unique() %>% na.omit()

data_rfm <- recency %>% left_join(frequency,by="CustomerID") %>% left_join(monetary,by="CustomerID")

