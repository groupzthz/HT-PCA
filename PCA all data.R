# Main data structure
###
library("data.table")
# Time manipulation
library("lubridate")
# Mapping vector values
library("plyr")
library("tidyverse")
library("psych")


####set WD####

###upload data####
list.files()


dat.1 <- read.csv("allhens.uncorrected.wide.CSV1.csv")

names(dat.1)
summary(dat.1)

dat.2 <- read.csv("allhens.uncorrected.wide.CSV1.2.csv")

summary(dat.2)
head(dat.2)
names(dat.2)
dat.2 <- dat.2 %>%
  rename(sleeptier = Tier,
         Hen = Hen.x) %>% 
  select(X, Hen, dinb, sire, date1, transitions, tierduration_1, tierduration_2, tierduration_3, tierduration_4, tierduration_5, times_1, times_2, times_3, times_4, times_5, sleeptier)

dat.3 <- read.csv("allhens.uncorrected.wide.CSV1.3.csv")

summary(dat.3)
head(dat.3)
names(dat.3)
dat.3 <- dat.3[, c(-1)]
dat.4 <- read.csv("allhens.uncorrected.wide.CSV2.csv")

names(dat.4)
dat.4 <- dat.4[, c(-1)]
summary(dat.4)
head(dat.4)

dat.5 <- read.csv("allhens.uncorrected.wide.CSV2.2.csv")

names(dat.5)
dat.5 <- dat.5[, c(-1)]

summary(dat.5)
head(dat.5)

dat.6 <- read.csv("allhens.uncorrected.wide.CSV2.3.csv")

names(dat.6)
dat.6 <- dat.6[, c(-1)]
summary(dat.6)
head(dat.6)

dat <- rbind(dat.1, dat.2, dat.3, dat.4, dat.5, dat.6)

head(dat)

dat <- dat %>% 
  mutate(date1 = as.Date(date1),
         month = month(date1))

head(dat)
names(dat)

dat.pca <- dat %>% 
  select(6:17)

str(dat.pca)

dat.pca <- as.data.table(dat.pca)

summary(dat.pca)

dat.pca <- dat.pca %>% 
  mutate(transitions = as.numeric(transitions),
         tierduration_1 = as.numeric(tierduration_1),
         tierduration_2 = as.numeric(tierduration_2),
         tierduration_3 = as.numeric(tierduration_3),
         tierduration_4 = as.numeric(tierduration_4),
         tierduration_5 = as.numeric(tierduration_5),
         times_1 = as.numeric(times_1),
         times_2 = as.numeric(times_2),
         times_3 = as.numeric(times_3),
         times_4 = as.numeric(times_4),
         times_5 = as.numeric(times_5))

pca.1 <- principal(dat.pca, nfactors = 4, rotate = "varimax", scores = T)

summary(pca.1)

pca.1
