library(magrittr)
library(dplyr)
library(ggplot2)

load("/Users/emh444/Downloads/temp-send/campaign_donations.rdata")

tops <- rbind(ohf, cle, shf, bks, dr, vlp, lj, an)

tops$DONORNAME[tops$DONORNAME == "Lynn Johnson"] <- "Ms. Lynn Johnson"

tops <- tops %>%
  group_by(DONORNAME, year) %>%
  summarise(number = length(unique(ID)), 
            total = sum(AMT))
tops <- tops %>%
  group_by(DONORNAME) %>%
  mutate(TotalCount = sum(number),
         TotalDonations = sum(total))

tops$year <- factor(tops$year)

ggplot(tops, aes(year, total, group = DONORNAME)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = factor(DONORNAME))) +
  scale_fill_manual(values = pal) +
  scale_x_discrete(limits = levels(tops$year))

ggplot(tops, aes(year, total, group = DONORNAME)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = factor(DONORNAME))) +
  scale_fill_manual(values = pal) +
  scale_x_discrete(limits = levels(tops$year))


max <- camp %>%
  group_by(year) %>%
  summarise(big_gift = max(AMT))

max05 <- subset(camp, year == "2005" & AMT >= 10000000)
max06 <- subset(camp, year == "2006" & AMT >= 5250000)
max07 <- subset(camp, year == "2007" & AMT >= 2500000)
max08 <- subset(camp, year == "2008" & AMT >= 58135866)
max09 <- subset(camp, year == "2009" & AMT >= 6654833)
max10 <- subset(camp, year == "2010" & AMT >= 18000000)
max11 <- subset(camp, year == "2011" & AMT >= 30369822)
max12 <- subset(camp, year == "2012" & AMT >= 5000000)
max13 <- subset(camp, year == "2013" & AMT >= 7577494)
max14 <- subset(camp, year == "2014" & AMT >= 10000000)
max15 <- subset(camp, year == "2015" & AMT >= 3260000)

maxx <- rbind(max05, max06, max07, max08, max09, max10, max11, max12, max13, max14, max15)

save(maxx, file = "/Users/emh444/Downloads/temp-send/max.rdata")
write.csv(maxx, "/Users/emh444/Desktop/R/donors.csv")

mil <- subset(camp, AMT >= 1000000)

ggplot(mil, aes(gift_date, AMT)) + 
  geom_point() +
  geom_smooth()


#########################################################
## creating data sets with all contributions about each maxx donor
ohf <- subset(camp, DONORNAME == "Osteopathic Heritage Foundations")
cle <- subset(camp, DONORNAME == "Cleveland Clinic Health System")
shf <- subset(camp, DONORNAME == "Scripps Howard Foundation")
bks <- subset(camp, DONORNAME == "Estate of Beth K. Stocker")
dr <- subset(camp, DONORNAME == "Estate of Dolores Russ")
vlp <- subset(camp, DONORNAME == "Dr. Violet L. Patton")
lj <- subset(camp, DONORNAME == "Ms. Lynn Johnson")
lj2 <- subset(camp, DONORNAME == "Lynn Johnson")
lj <- rbind(lj, lj2)
lj <- subset(lj, DONORTYPE != "Friend")
an <- subset(camp, DONORNAME == "Anonymous")

#########################################################
## plots

pal <- c('#1f3d7a', '#9daf72', '#566047', '#562f32', '#462d44', '#859731', '#640e27', '#33001a')

maxx$DONORNAME <- as.factor(maxx$DONORNAME)

ggplot(maxx, aes(year, AMT, fill = factor(DONORNAME))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = pal) +
  ggtitle("The largest gifts to Ohio University, 2005-2015") +
  xlab("Year of Donation") +
  ylab("Amount Given")


ggplot(ohf, aes(gift_date, AMT)) +
  geom_point(colour = '#640e27', size = 3) + 
  ggtitle("Donations from Osteopathic Heritage Foundation") +
  xlab("Gift Date") + 
  ylab("Amount of Individual Donation")

ggplot(bks, aes(gift_date, AMT)) +
  geom_point(colour = '#562f32', size = 3) +
  ggtitle("Donations from the Estate of Beth K. Stocker") +
  xlab("Gift Date") +
  ylab("Amount of Individual Donation")

ggplot(cle, aes(gift_date, AMT)) +
  geom_point(colour = '#9daf72', size = 3) +
  ggtitle("Donations from the Cleveland Clinic Health System") +
  xlab("Gift Date") +
  ylab("Amount of Individual Donation")

ggplot(dr, aes(gift_date, AMT)) +
  geom_point(colour = '#462d44', size = 3) +
  ggtitle("Donations from the Estate of Dolores Russ") +
  xlab("Gift Date") +
  ylab("Amount of Individual Donation")

ggplot(shf, aes(gift_date, AMT)) +
  geom_point(colour = '#33001a', size = 3) +
  ggtitle("Donations from the Scripps Howard Foundation") +
  xlab("Gift Date") +
  ylab("Amount of Individual Donation")

ggplot(vlp, aes(gift_date, AMT)) +
  geom_point(colour = '#566047', size = 3) +
  ggtitle("Donations from Dr. Violet L. Patton") +
  xlab("Gift Date") +
  ylab("Amount of Individual Donation")

ggplot(lj, aes(gift_date, AMT)) +
  geom_point(colour = '#859731', size = 3) +
  ggtitle("Donations from Lynn Johnson") +
  xlab("Gift Date") +
  ylab("Amount of Individual Donation")

ggplot(an, aes(gift_date, AMT)) +
  geom_point(colour = '#1f3d7a', size = 3) +
  ggtitle("Anonymous Donations") +
  xlab("Gift Date") +
  ylab("Amount of Individual Donation")

save(an, bks, cle, dr, lj, maxx, ohf, shf, vlp, tops, file = "app_max/max-data.rdata")

# Promise Lives campaign didn't start until 07/01/2007, but apparently OU started collecting donations before then
b4 <- subset(camp, year==2005 | year==2006 | GIFTDATE == 20070518)
sum(b4$AMT)