library(dplyr)
library(data.table)
library(stringr)
library(lubridate)
library(tidyr)
library(ggplot2)

odat = fread("CourseProject_Order.csv")
ldat = fread("CourseProject_Logistics.csv")
mdat = fread("CourseProject_Merchant.csv")
gc()

head(odat)
head(ldat)
head(mdat)

## Converting to Time
odat$day = ymd(odat$day)
ldat$day = ymd(ldat$day)
mdat$day = ymd(mdat$day)
odat$pay_timestamp = ymd_hms(odat$pay_timestamp)
ldat$timestamp = ymd_hms(ldat$timestamp)

## Merchant Data
unique(mdat$merchant_id)
length(unique(mdat$merchant_id))

result = mdat %>% group_by(merchant_id) %>% summarize(avguv=mean(pcuv)) %>% arrange(desc(avguv))

m42 = mdat %>% filter(merchant_id==42) %>% select(day,avgLogisticScore,avgServiceQualityScore) %>% arrange(day)
qplot(m42$day,m42$avgLogisticScore)
qplot(m42$day,m42$avgServiceQualityScore)
m323 = mdat %>% filter(merchant_id==323) %>% select(day,avgLogisticScore,avgServiceQualityScore) %>% arrange(day)
qplot(m323$day,m323$avgLogisticScore)
qplot(m323$day,m323$avgServiceQualityScore)

odat %>% filter(merchant_id==42) %>% group_by(if_cainiao) %>% summarize(norder=n_distinct(order_id))
odat %>% filter(merchant_id==323) %>% group_by(if_cainiao) %>% summarize(norder=n_distinct(order_id))
# Two major merchants show very different pattern

# Now, does order fulfillment time decrease when we use Cainiao service?
# First, how can we calculate order fulfillment time?
# Fulfillment time = SIGNED (in logistics data) - pay_timestampe (in order data)
signed_data = ldat %>% filter(action=="SIGNED") %>% select(order_id,timestamp)
colnames(signed_data) = c("order_id","SIGNED_time")
head(signed_data)

odat = odat %>% left_join(signed_data)
head(odat)
odat = odat %>% mutate(totaltime = as.numeric(SIGNED_time-pay_timestamp)/3600) # Default: second --> change it to hours

# Now, let's compare
odat %>% group_by(if_cainiao) %>% summarize(avgTotalTime=mean(totaltime)) # about 10 hours faster!

# But... really? 10 hours?
filtered_odat = odat %>% filter(promise==0)
filtered_odat %>% group_by(if_cainiao) %>% summarize(avgTotalTime=mean(totaltime)) # Still faster!

# Let's look deeper
odat %>% group_by(if_cainiao,promise) %>% summarize(norder=n_distinct(order_id)) # Hmm!

# Are we done? Is that it? What should we do now?
# what can we do more?

# for example, we can investigate
lcdat = ldat %>% select(order_id,logistic_company_id) %>% distinct()
result = lcdat %>% group_by(logistic_company_id) %>% summarize(norder=n_distinct(order_id)) %>% arrange(desc(norder))
sum(result$norder) # the first two LCs cover 74% of total orders! (we have 38 LCs)
result = result %>% mutate(LCsize=ifelse(norder>200000,"Large","Small"))

odat = odat %>% left_join(lcdat)
odat = odat %>% left_join(result)
head(odat)

odat %>% group_by(LCsize,if_cainiao) %>% summarize(Avgtotaltime=mean(totaltime)) # 10 hrs vs. 7.6 hrs

last_city = ldat %>% filter(action=="SIGNED") %>% select(order_id,city_id)
result = last_city %>% group_by(city_id) %>% summarize(norder=n_distinct(order_id)) %>% arrange(desc(norder))
last_city = last_city %>% mutate(Citysize = ifelse(city_id==234|city_id==133,"BigCity","SmallCity"))
odat = odat %>% left_join(last_city)

odat %>% group_by(LCsize,Citysize,if_cainiao) %>% summarize(Avgtotaltime=mean(totaltime)) # BigCity SmallLC case!!
odat %>% group_by(LCsize,Citysize,if_cainiao) %>% summarize(norder=n_distinct(order_id)) # Enough data points
# What is so special about BigCity orders delievered by small LCs? Any existing findings in the literature?
# Any logical explanation? Would this be a potential risk for Cainiao in terms of sustainability?

# See? the question gets endless. You need to spend a lot of time exploring the data file!
# e.g., Segmenting the total fulfillment time: pre-delivery, last-mile delivery, ...
# Within-facility and Between-facility time?
# Any merchant-related effects?
# Effects of starting point? (cityID associated with GOT event)
# Popular vs. less-popular items?




