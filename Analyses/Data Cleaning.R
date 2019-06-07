## PrEP Indications   ##
## Cleaning Script    ##
## 2019-06-06         ##

# Package and setup -----------------------
rm(list = ls())

## Install ART-Net Data Package
# remotes::install_github("EpiModel/ARTnetData", ref = "f07ba02", force = TRUE)

library(dplyr)
library(tidyr)
library(readxl)
library(MASS)
library(haven)
library(tidyverse)
library(ARTnetData)

# Read in datasets
artnet <- ARTnetData::ARTnet.wide
artnetLong <- ARTnetData::ARTnet.long
artnet2 <- artnet


# Variables --------------------------------------------------------------------
# Make HIV Status categorical variable
artnet2$hivstatus <- rep(NA, nrow(artnet2))
artnet2$hivstatus[which(artnet2$hiv3 == 0)] <- "Negative"
artnet2$hivstatus[which(artnet2$hiv3 == 1)] <- "Positive"
artnet2$hivstatus[which(artnet2$hiv3 == 2)] <- "Unknown"

# Make Region a categorical variable
artnet2$region <- rep(NA, nrow(artnet2))
artnet2$region[which(artnet2$REGCODE == 1)] <- "Northeast"
artnet2$region[which(artnet2$REGCODE == 2)] <- "Midwest"
artnet2$region[which(artnet2$REGCODE == 3)] <- "South"
artnet2$region[which(artnet2$REGCODE == 4)] <- "West"

# Age category
artnet2$age.cat <- rep(NA, nrow(artnet))
artnet2$age.cat[artnet2$age >= 15 & artnet$age <= 24] <- "15-24"
artnet2$age.cat[artnet2$age >= 25 & artnet$age <= 34] <- "25-34"
artnet2$age.cat[artnet2$age >= 35 & artnet$age <= 44] <- "35-44"
artnet2$age.cat[artnet2$age >= 45 & artnet$age <= 54] <- "45-54"
artnet2$age.cat[artnet2$age >= 55 & artnet$age <= 65] <- "55-65"
artnet2$age.cat[artnet2$age > 65] <- "66+"

# Partner HIV status
artnetLong$partstatus <- rep(NA, nrow(artnetLong))
artnetLong$partstatus[artnetLong$p_hiv == 2] <- "Unknown"
artnetLong$partstatus[artnetLong$p_hiv == 1] <- "Positive"
artnetLong$partstatus[artnetLong$p_hiv == 0] <- "Negative"

# Calculate total degree
# Create mean degree variable
l <- artnetLong
l$ONGOING <- as.numeric(l$ONGOING)
l$ongoing2 <- ifelse(l$ONGOING %in% c(88, 99), 0, l$ONGOING)
l$ongoing2[which(is.na(l$ONGOING))] <- 0
l$ONGOING <- NULL

# Total
df <- l %>%
  filter(RAI == 1 | IAI == 1 | ROI == 1 | IOI == 1) %>% # filter activity type
  filter(ptype %in% 1:2) %>% # filter partnership type
  group_by(AMIS_ID) %>%
  summarise(totdegree = sum(ongoing2))
df4 <- l %>%
  filter(RAI == 1 | IAI == 1 | ROI == 1 | IOI == 1) %>% # filter activity type
  filter(ptype == 1) %>% # filter partnership type
  group_by(AMIS_ID) %>%
  summarise(maintotdegree = sum(ongoing2))
df7 <- l %>%
  filter(RAI == 1 | IAI == 1 | ROI == 1 | IOI == 1) %>%
  # filter(p_RAI == 1 | p_IAI == 1) %>% # filter activity type
  filter(ptype == 2) %>%
  # filter(ptype %in% 1) %>% # filter partnership type
  group_by(AMIS_ID) %>%
  summarise(castotdegree = sum(ongoing2))

# Create merged dataframes
artnet2 <- left_join(artnet2, df, by = "AMIS_ID")
artnet2 <- left_join(artnet2, df4, by = "AMIS_ID")
artnet2 <- left_join(artnet2, df7, by = "AMIS_ID")
table(artnet2$totdegree, useNA = "always")
table(artnet2$maintotdegree, useNA = "always")
table(artnet2$castotdegree, useNA = "always")

# If missing degree values, then set to 0
artnet2$totdegree <- ifelse(is.na(artnet2$totdegree), 0, artnet2$totdegree)
artnet2$maintotdegree <- ifelse(is.na(artnet2$maintotdegree), 0, artnet2$maintotdegree)
artnet2$castotdegree <- ifelse(is.na(artnet2$castotdegree), 0, artnet2$castotdegree)

# Indications ------------------------------------------------------------------
# Set up individual components of indications
artnet2$prep_part12mo <- rep(0, nrow(artnet))
artnet2$prep_part6mo <- rep(0, nrow(artnet))
artnet2$prep_hiv <- rep(0, nrow(artnet))
artnet2$prep_nonmonog <- rep(0, nrow(artnet))
artnet2$prep_sti <- rep(0, nrow(artnet))
artnet2$prep_uai <- rep(0, nrow(artnet))

# Set up actual indications
artnet2$prepind_uai <- rep(0, nrow(artnet))
artnet2$prepind_sti <- rep(0, nrow(artnet))
artnet2$prepind_any <- rep(0, nrow(artnet))

# Denominator: not known to be positive
sum(artnet2$hivstatus == "Negative") #3726

# artnetLong$part6mo <- table(difftime(artnetLong$SUB_DATE, artnetLong$p_end.date) > 182)

# Revise age category coding
artnet2$age.cat[which(artnet2$age < 18)] <- "15-17"
artnet2$age.cat[which(artnet2$age >= 18 & artnet2$age < 25)] <- "18-24"

# USPHS Guidelines  ---------
# Adult man
# Without acute or established HIV infection
# Any male sex partners in past 6 months (if also has sex with women, see Box B2)
# Not in a monogamous partnership with a recently tested, HIV-negative man
# AND at least one of the following
# Any anal sex without condoms (receptive or insertive) in past 6 months
# A bacterial STI (syphilis, gonorrhea, or chlamydia) diagnosed or reported in past 6
# months
#
# Indications
# Common: HIV-negative, sexually active in past 6 months
# Common: multiple ongoing partners | (partner # = 1 & partner is not negative)
# Indication 1: Any CAI
# Indication 2: Any STI in past 6 months


# Subsetting to factors ------------------------------------------
# Condition 1: Age > 18
adults <- as.numeric(artnet2[which(artnet2$age >= 18), "AMIS_ID"]) #4759 men
artnet2$prep_adult[which(artnet2$AMIS_ID %in% adults)] <- 1

# Condition 2: HIV-negative man
negativeids <- as.numeric(artnet2[which(artnet2$hivstatus == "Negative"), "AMIS_ID"]) # 3726 men
artnet2$prep_hiv[which(artnet2$AMIS_ID %in% negativeids)] <- 1

# Condition 3: IDs of those who had a male partner
twelvemonthids <- as.numeric(unique(artnetLong[which(difftime(artnetLong$SUB_DATE,
                                                              artnetLong$end.date,
                                                              units = "days") <= 365),
                                               "AMIS_ID"])) #4596 men
artnet2$prep_part12mo[which(artnet2$AMIS_ID %in% twelvemonthids)] <- 1

sixmonthids <- as.numeric(unique(artnetLong[which(difftime(artnetLong$SUB_DATE,
                                                           artnetLong$end.date,
                                                           units = "days") <= 182),
                                            "AMIS_ID"])) #4435 men
artnet2$prep_part6mo[which(artnet2$AMIS_ID %in% sixmonthids)] <- 1

# Condition 4: Not in monogamous partnership with recently tested, HIV-negative man
### Version A: More than one ongoing partner
gt2partids <- as.numeric(unique(artnet2$AMIS_ID[which(artnet2$totdegree > 1)])) #1290 men with >1 partners

### Version B: Partner number = 1 and partner is not negative
onepartids <- as.numeric(unique(artnet2$AMIS_ID[which(artnet2$totdegree == 1)])) #2182 men with one partner
onepartids2 <- as.numeric(unique(artnetLong[which((artnetLong$AMIS_ID %in% onepartids) &
                       (artnetLong$partstatus == "Unknown" |
                          artnetLong$partstatus == "Positive" |
                          is.na(artnetLong$partstatus))), "AMIS_ID"])) # 1004 men with one partner who is not HIV-negative
### Combine Version A and B
partners <- unique(c(gt2partids, onepartids2)) # 2294 men not in monog
artnet2$prep_nonmonog[which(artnet2$AMIS_ID %in% partners)] <- 1

#TODO: Check condom-protected values
# CAI in past 6 months
df <- artnetLong %>%
  # filter(AMIS_ID %in% prepeligv3) %>%
  filter(difftime(SUB_DATE, end.date) <= 182) %>%
  filter(anal.acts.week > 0) %>%
  filter(anal.acts.week.cp < anal.acts.week) %>% # Changed to CP acts
  group_by(AMIS_ID)
cai <- as.numeric(unique(df$AMIS_ID)) #2224 men who meet other criteria
artnet2$prep_uai[which(artnet2$AMIS_ID %in% cai)] <- 1

# Recent STI
recentsti <- unique(as.numeric(artnet2$AMIS_ID[which(artnet2$BSTIA == 1 |
                                                       artnet2$BSTIB == 1 |
                                                       artnet2$BSTIC == 1)])) #567 men
artnet2$prep_sti[which(artnet2$AMIS_ID %in% recentsti)] <- 1

# Common factors
# Changed to negative men
# prepeligv1 <- intersect(adults, negativeids) # men >18 and negative
prepeligv1 <- negativeids # 3726 negative men

# Vectors of people
prepeligv212mo <- intersect(prepeligv1, twelvemonthids) # men, negative, and active in last 6 months # equiv to CDC denominator
prepeligv26mo <- intersect(prepeligv1, sixmonthids) # men, negative, and active in last 6 months # equiv to CDC denominator

# Using 12 months now
prepeligv3 <- intersect(prepeligv26mo, partners) # men, negative, active in last 6 months, non-monog # USPHS base
prepeligv4 <- intersect(prepeligv3, cai) # men, negative, active in last 6 months, non-monog, CAI
prepeligv5 <- intersect(prepeligv3, recentsti) # men, negative, active in last 6 months, non-monog, STI
prepeligv6 <- intersect(prepeligv3, c(cai, recentsti)) # men, negative, active in last 6 months, non-monog, CAI or STI

# PrEP Indications
artnet2$prepind_uai[which(artnet2$AMIS_ID %in% prepeligv4)] <- 1
artnet2$prepind_sti[which(artnet2$AMIS_ID %in% prepeligv5)] <- 1
artnet2$prepind_any[which(artnet2$AMIS_ID %in% prepeligv6)] <- 1
