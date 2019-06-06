## PrEP Indications   ##
## Cleaning Script    ##
## 2019-06-06         ##


# Package and setup -----------------------
rm(list = ls())
library(dplyr)
library(tidyr)
library(readxl)
library(MASS)
library(haven)
library(tidyverse)

# Read in datasets
artnet <- readRDS("Input/artnet4shiny.rda")
artnetLong <- readRDS("Input/artnetlong4shiny.rda")
artnet2 <- artnet

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
