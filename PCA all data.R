# Main data structure
###
library("data.table")
# Time manipulation
library("lubridate")
# Mapping vector values
library("plyr")
library("tidyverse")
library("psych")


####set WD or open project####

###upload data####
list.files()

###data is in large chunks because of the large amount of raw data###
#first chunk#
dat.1 <- read.csv("processed_data/allhens.uncorrected.wide.CSV1.csv")

names(dat.1)

dat.1 <- dat.1 %>% relocate(sleeptier, .after = times_5)
#second chunk#
dat.2 <- read.csv("processed_data/allhens.uncorrected.wide.CSV1.2.csv")

names(dat.2)
###saved wrongly from clean data and redoing some minor column name issues###

dat.2 <- dat.2 %>%
  rename(sleeptier = Tier,
         Hen = Hen.x) %>% 
  select(X, Hen, dinb, sire, date1, transitions, tierduration_1, tierduration_2, tierduration_3, tierduration_4, tierduration_5, times_1, times_2, times_3, times_4, times_5, sleeptier)

#third chunk#
dat.3 <- read.csv("processed_data/allhens.uncorrected.wide.CSV1.3.csv")

names(dat.3)

summary(dat.3)

dat.3 <- dat.3[, c(-1)]

str(dat.3)

head(dat.3)

dat.3 <- dat.3 %>% 
  mutate(across(where(is.character), str_trim))

dat.3 <- dat.3 %>% 
  separate(tierduration_1, into = c("dur_1", "sec"), sep = "^\\S*\\K\\s+")

dat.3 <- dat.3 %>% 
  separate(tierduration_2, into = c("dur_2", "sec"), sep = "^\\S*\\K\\s+")

dat.3 <- dat.3 %>% 
  separate(tierduration_3, into = c("dur_3", "sec"), sep = "^\\S*\\K\\s+")

dat.3 <- dat.3 %>% 
  separate(tierduration_4, into = c("dur_4", "sec"), sep = "^\\S*\\K\\s+")

dat.3 <- dat.3 %>% 
  separate(tierduration_5, into = c("dur_5", "sec"), sep = "^\\S*\\K\\s+")


dat.3 <- dat.3 %>% 
  rename(tierduration_1 = dur_1,
         tierduration_2 = dur_2,
         tierduration_3 = dur_3,
         tierduration_4 = dur_4,
         tierduration_5 = dur_5) %>% 
  mutate(tierduration_1 = as.numeric(tierduration_1),
         tierduration_2 = as.numeric(tierduration_2),
         tierduration_3 = as.numeric(tierduration_3),
         tierduration_4 = as.numeric(tierduration_4),
         tierduration_5 = as.numeric(tierduration_5))

dat.3 <- dat.3 %>% 
  select(-c("sec"))

#fourth chunk#
dat.4 <- read.csv("processed_data/allhens.uncorrected.wide.CSV2.csv")

names(dat.4)

dat.4 <- dat.4[, c(-1)]

#fifth chunk#
dat.5 <- read.csv("processed_data/allhens.uncorrected.wide.CSV2.2.csv")

names(dat.5)

dat.5 <- dat.5[, c(-1)]

#sixth chunk#
dat.6 <- read.csv("processed_data/allhens.uncorrected.wide.CSV2.3.csv")

names(dat.6)

dat.6 <- dat.6[, c(-1)]

#combining data#
dat <- rbind(dat.1, dat.2, dat.3, dat.4, dat.5, dat.6)

#making month data for separating data for future PCAs#

dat <- dat %>% 
  mutate(date1 = as.Date(date1),
         month = month(date1))

summary(dat)
head(dat)
names(dat)

#moving sleeping tier before transitions#
dat <- dat %>% relocate(sleeptier, .before = transitions)

#strange tier1 data for hen 18180#

dat.test <- dat %>% 
  filter(tierduration_1 < 54000,
         tierduration_2 < 54000,
         tierduration_3 < 54000,
         tierduration_4 < 54000,
         tierduration_5 < 54000)

names(dat)

dat <- dat %>% 
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

dat <- dat %>%
  rowwise() %>% 
  mutate(tiersum = sum(c(tierduration_1, tierduration_2, tierduration_3, tierduration_4, tierduration_5)))

summary(dat$tiersum)

dat.small <- dat %>% 
  filter(tiersum < 54000)

names(dat.small)
dat.small <- as.data.table(dat.small)

setkeyv(dat.small.1, c("Hen", "date1"))

dat.small.1 <- dat.small

setDT(dat.small.1)[ , ID := .GRP, by = month]
  group_by(month) %>% 
 dplyr::mutate(month.1 = cur_group_id())




dat.bad <- dat %>% 
  filter(tiersum > 54000)

length(unique(dat.bad$Hen))
#variables to use in PCA#
#not corrected for rate, but should only be an issue early or late in year with different amounts of time light and WG#

dat.pca <- dat.small %>% 
  select(6:16)

summary(dat.pca)
names(dat.pca)

which(dat$tierduration_2 == 129176, arr.ind = TRUE)

dat[257067,]


dat.pca.1 <- dat.pca %>% drop_na(tierduration_1)

dat.pca.1 <- dat.pca.1[complete.cases(dat.pca.1),]

str(dat.pca)

dat.pca.1 <- as.data.table(dat.pca.1)

pca.1 <- principal(dat.pca.1, nfactors = 4, rotate = "varimax", scores = T)
pc.fa <- fa(dat.pca.1, nfactors = 4, rotate = "varimax", scores = T)

pca.1
pc.fa

plot(pca.1$values, type = "l")

fa.parallel(dat.pca.1, fa = "pc", nfactors = 4, n.iter = 1000)

nFactors=4 # to compare models with up to 3 factors
BICs = rep(NA,nFactors) # define the vector that BIC values go in
names(BICs) = 1:nFactors # name its values according to #factors
for(iFactors in 1:nFactors) {
  pc_ht <- fa(dat.pca, nfactors=iFactors, rotate="varimax")
  BICs[iFactors] = pc_ht$objective - log(pc_ht$nh) * pc_ht$dof
}
BICs

install.packages("ecostats")
dat.pca.1 <- as.data.frame(dat.pca.1)

par(mfrow = c(4,3), mar = c(3,3,2,1), mgp  = c(1.75, 0.75, 0))
for(iVar in 1:11){
  datIvar = dat.pca.1[,iVar]
  plot(lm(datIvar ~ pca.1$scores), which = 1, n.sim = 99 )
  
  
test <- dat %>% 
  group_by(month) %>% 
  group_map(~ principal(dat.pca.1, nfactors = 4, rotate = "varimax", scores = T))
}

t.1 <- dat.small.1 %>% 
  filter(ID == 1)

names(t.1)

t.1.1 <- t.1 %>% 
  select(6:16)
summary(t.1.1)

t.1.1 <- t.1.1 %>% 
  select(-c(tierduration_1, times_1))
t.1.p <- principal(t.1.1, nfactors = 4, rotate = "varimax", scores = T)

t.1.p

t.2 <- dat.small.1 %>% 
  filter(ID == 2)

t.2.1 <- t.2 %>% 
  select(6:16)

t.2.p <- principal(t.2.1, nfactors = 4, rotate = "varimax", scores = T)

t.2.p

t.3 <- dat.small.1 %>% 
  filter(ID == 3)

t.3.1 <- t.3 %>% 
  select(6:16)

t.3.p <- principal(t.3.1, nfactors = 4, rotate = "varimax", scores = T)

t.3.p


t.4 <- dat.small.1 %>% 
  filter(ID == 4)

t.4.1 <- t.4 %>% 
  select(6:16)

t.4.p <- principal(t.4.1, nfactors = 4, rotate = "varimax", scores = T)

t.4.p

t.5 <- dat.small.1 %>% 
  filter(ID == 5)

t.5.1 <- t.5 %>% 
  select(6:16)

t.5.p <- principal(t.5.1, nfactors = 4, rotate = "varimax", scores = T)

t.5.p


t.6 <- dat.small.1 %>% 
  filter(ID == 6)

t.6.1 <- t.6 %>% 
  select(6:16)

t.6.p <- principal(t.6.1, nfactors = 4, rotate = "varimax", scores = T)

t.6.p


t.7 <- dat.small.1 %>% 
  filter(ID == 7)

t.7.1 <- t.7 %>% 
  select(6:16)

t.7.p <- principal(t.7.1, nfactors = 4, rotate = "varimax", scores = T)

t.7.p

t.8 <- dat.small.1 %>% 
  filter(ID == 8)

t.8.1 <- t.8 %>% 
  select(6:16)

t.8.p <- principal(t.8.1, nfactors = 4, rotate = "varimax", scores = T)

t.8.p

t.9 <- dat.small.1 %>% 
  filter(ID == 9)

t.9.1 <- t.9 %>% 
  select(6:16)

t.9.p <- principal(t.9.1, nfactors = 4, rotate = "varimax", scores = T)

t.9.p

max(dat.small.1$ID)

t.10 <- dat.small.1 %>% 
  filter(ID == 10)

t.10.1 <- t.10 %>% 
  select(6:16)

t.10.p <- principal(t.10.1, nfactors = 4, rotate = "varimax", scores = T)

t.10.p

test.try <- dat.small.1 %>% 
  group_by(ID) %>% 
  nest() %>% 
  mutate(mod = modify(data, ~ principal(.x %>% select(6:16), nfactors = 4, rotate = "varimax", scores = T)))
  

#Need to checkout what is going on with these huge row sums#
#6 hens from pen 18 and 20#

write.table(dat.bad, "HT.large.row.sums.csv", quote = T,
            sep=",", row.names = F)
