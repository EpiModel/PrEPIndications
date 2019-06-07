## PrEP Indications   ##
## Table 2 Script     ##
## 2019-06-06         ##

# Load packages ---------
rm(list = ls())
library(dplyr)
library(tidyr)
library(readxl)
library(MASS)
library(haven)
library(tidyverse)

# Read in cleaning and datasets ---------

## Data Cleaning/Management Script
source("Analyses/Data Cleaning.R", echo = FALSE)

# Vectors ---------------------
# Column 1 - Overall
num <- nrow(artnet2)
numwhite <- length(which(artnet2$race.cat == "white"))
numblack <- length(which(artnet2$race.cat == "black"))
numhisp <- length(which(artnet2$race.cat == "hispanic"))
numother <- length(which(artnet2$race.cat == "other"))
numne <- length(which(artnet2$region == "Northeast"))
nummw <- length(which(artnet2$region == "Midwest"))
numsouth <- length(which(artnet2$region == "South"))
numwest <- length(which(artnet2$region == "West"))
num1517 <- length(which(artnet2$age.cat == "15-17"))
num1824 <- length(which(artnet2$age.cat == "18-24"))
num2534 <- length(which(artnet2$age.cat == "25-34"))
num3544 <- length(which(artnet2$age.cat == "35-44"))
num4554 <- length(which(artnet2$age.cat == "45-54"))
num5565 <- length(which(artnet2$age.cat == "55-65"))

# Column 2 - adult
numadult <- length(which(artnet2$prep_adult == 1))
numwhiteadult <- length(which(artnet2$race.cat == "white" &
                                artnet2$prep_adult == 1))
numblackadult <- length(which(artnet2$race.cat == "black" &
                                artnet2$prep_adult == 1))
numhispadult <- length(which(artnet2$race.cat == "hispanic" &
                               artnet2$prep_adult == 1))
numotheradult <- length(which(artnet2$race.cat == "other" &
                                artnet2$prep_adult == 1))
numneadult <- length(which(artnet2$region == "Northeast" &
                             artnet2$prep_adult == 1))
nummwadult <- length(which(artnet2$region == "Midwest" &
                             artnet2$prep_adult == 1))
numsouthadult <- length(which(artnet2$region == "South" &
                                artnet2$prep_adult == 1))
numwestadult <- length(which(artnet2$region == "West" &
                               artnet2$prep_adult == 1))
num1517adult <- length(which(artnet2$age.cat == "15-17" &
                               artnet2$prep_adult == 1))
num1824adult <- length(which(artnet2$age.cat == "18-24" &
                               artnet2$prep_adult == 1))
num2534adult <- length(which(artnet2$age.cat == "25-34" &
                               artnet2$prep_adult == 1))
num3544adult <- length(which(artnet2$age.cat == "35-44" &
                               artnet2$prep_adult == 1))
num4554adult <- length(which(artnet2$age.cat == "45-54" &
                               artnet2$prep_adult == 1))
num5565adult <- length(which(artnet2$age.cat == "55-65" &
                               artnet2$prep_adult == 1))

# Column 3 - HIV-neg
numhivneg <- length(which(artnet2$prep_hiv == 1))
numwhitehivneg <- length(which(artnet2$race.cat == "white" &
                                 artnet2$prep_hiv == 1))
numblackhivneg <- length(which(artnet2$race.cat == "black" &
                                 artnet2$prep_hiv == 1))
numhisphivneg <- length(which(artnet2$race.cat == "hispanic" &
                                artnet2$prep_hiv == 1))
numotherhivneg <- length(which(artnet2$race.cat == "other" &
                                 artnet2$prep_hiv == 1))
numnehivneg <- length(which(artnet2$region == "Northeast" &
                              artnet2$prep_hiv == 1))
nummwhivneg <- length(which(artnet2$region == "Midwest" &
                              artnet2$prep_hiv == 1))
numsouthhivneg <- length(which(artnet2$region == "South" &
                                 artnet2$prep_hiv == 1))
numwesthivneg <- length(which(artnet2$region == "West" &
                                artnet2$prep_hiv == 1))
num1517hivneg <- length(which(artnet2$age.cat == "15-17" &
                                artnet2$prep_hiv == 1))
num1824hivneg <- length(which(artnet2$age.cat == "18-24" &
                                artnet2$prep_hiv == 1))
num2534hivneg <- length(which(artnet2$age.cat == "25-34" &
                                artnet2$prep_hiv == 1))
num3544hivneg <- length(which(artnet2$age.cat == "35-44" &
                                artnet2$prep_hiv == 1))
num4554hivneg <- length(which(artnet2$age.cat == "45-54" &
                                artnet2$prep_hiv == 1))
num5565hivneg <- length(which(artnet2$age.cat == "55-65" &
                                artnet2$prep_hiv == 1))

# Column 4 - Male partner in last 6 months
nump6mo <- length(which(artnet2$prep_part6mo == 1))
numwhitep6mo <- length(which(artnet2$race.cat == "white" &
                               artnet2$prep_part6mo == 1))
numblackp6mo <- length(which(artnet2$race.cat == "black" &
                               artnet2$prep_part6mo == 1))
numhispp6mo <- length(which(artnet2$race.cat == "hispanic" &
                              artnet2$prep_part6mo == 1))
numotherp6mo <- length(which(artnet2$race.cat == "other" &
                               artnet2$prep_part6mo == 1))
numnep6mo <- length(which(artnet2$region == "Northeast" &
                            artnet2$prep_part6mo == 1))
nummwp6mo <- length(which(artnet2$region == "Midwest" &
                            artnet2$prep_part6mo == 1))
numsouthp6mo <- length(which(artnet2$region == "South" &
                               artnet2$prep_part6mo == 1))
numwestp6mo <- length(which(artnet2$region == "West" &
                              artnet2$prep_part6mo == 1))
num1517p6mo <- length(which(artnet2$age.cat == "15-17" &
                              artnet2$prep_part6mo == 1))
num1824p6mo <- length(which(artnet2$age.cat == "18-24" &
                              artnet2$prep_part6mo == 1))
num2534p6mo <- length(which(artnet2$age.cat == "25-34" &
                              artnet2$prep_part6mo == 1))
num3544p6mo <- length(which(artnet2$age.cat == "35-44" &
                              artnet2$prep_part6mo == 1))
num4554p6mo <- length(which(artnet2$age.cat == "45-54" &
                              artnet2$prep_part6mo == 1))
num5565p6mo <- length(which(artnet2$age.cat == "55-65" &
                              artnet2$prep_part6mo == 1))

# Column 5 - Not in a monogamous relationship with a HIV-negative partner
numnonmonog <- length(which(artnet2$prep_nonmonog == 1))
numwhitenonmonog <- length(which(artnet2$race.cat == "white" &
                                   artnet2$prep_nonmonog == 1))
numblacknonmonog <- length(which(artnet2$race.cat == "black" &
                                   artnet2$prep_nonmonog == 1))
numhispnonmonog <- length(which(artnet2$race.cat == "hispanic" &
                                  artnet2$prep_nonmonog == 1))
numothernonmonog <- length(which(artnet2$race.cat == "other" &
                                   artnet2$prep_nonmonog == 1))
numnenonmonog <- length(which(artnet2$region == "Northeast" &
                                artnet2$prep_nonmonog == 1))
nummwnonmonog <- length(which(artnet2$region == "Midwest" &
                                artnet2$prep_nonmonog == 1))
numsouthnonmonog <- length(which(artnet2$region == "South" &
                                   artnet2$prep_nonmonog == 1))
numwestnonmonog <- length(which(artnet2$region == "West" &
                                  artnet2$prep_nonmonog == 1))
num1517nonmonog <- length(which(artnet2$age.cat == "15-17" &
                                  artnet2$prep_nonmonog == 1))
num1824nonmonog <- length(which(artnet2$age.cat == "18-24" &
                                  artnet2$prep_nonmonog == 1))
num2534nonmonog <- length(which(artnet2$age.cat == "25-34" &
                                  artnet2$prep_nonmonog == 1))
num3544nonmonog <- length(which(artnet2$age.cat == "35-44" &
                                  artnet2$prep_nonmonog == 1))
num4554nonmonog <- length(which(artnet2$age.cat == "45-54" &
                                  artnet2$prep_nonmonog == 1))
num5565nonmonog <- length(which(artnet2$age.cat == "55-65" &
                                  artnet2$prep_nonmonog == 1))

# Column 6 - Recent CAI
numcai <- length(which(artnet2$prep_uai == 1))
numwhitecai <- length(which(artnet2$race.cat == "white" & artnet2$prep_uai == 1))
numblackcai <- length(which(artnet2$race.cat == "black" & artnet2$prep_uai == 1))
numhispcai <- length(which(artnet2$race.cat == "hispanic" & artnet2$prep_uai == 1))
numothercai <- length(which(artnet2$race.cat == "other" & artnet2$prep_uai == 1))
numnecai <- length(which(artnet2$region == "Northeast" & artnet2$prep_uai == 1))
nummwcai <- length(which(artnet2$region == "Midwest" & artnet2$prep_uai == 1))
numsouthcai <- length(which(artnet2$region == "South" & artnet2$prep_uai == 1))
numwestcai <- length(which(artnet2$region == "West" & artnet2$prep_uai == 1))
num1517cai <- length(which(artnet2$age.cat == "15-17" & artnet2$prep_uai == 1))
num1824cai <- length(which(artnet2$age.cat == "18-24" & artnet2$prep_uai == 1))
num2534cai <- length(which(artnet2$age.cat == "25-34" & artnet2$prep_uai == 1))
num3544cai <- length(which(artnet2$age.cat == "35-44" & artnet2$prep_uai == 1))
num4554cai <- length(which(artnet2$age.cat == "45-54" & artnet2$prep_uai == 1))
num5565cai <- length(which(artnet2$age.cat == "55-65" & artnet2$prep_uai == 1))

# Column 7 - Recent STI
numsti <- length(which(artnet2$prep_sti == 1))
numwhitesti <- length(which(artnet2$race.cat == "white" & artnet2$prep_sti == 1))
numblacksti <- length(which(artnet2$race.cat == "black" & artnet2$prep_sti == 1))
numhispsti <- length(which(artnet2$race.cat == "hispanic" & artnet2$prep_sti == 1))
numothersti <- length(which(artnet2$race.cat == "other" & artnet2$prep_sti == 1))
numnesti <- length(which(artnet2$region == "Northeast" & artnet2$prep_sti == 1))
nummwsti <- length(which(artnet2$region == "Midwest" & artnet2$prep_sti == 1))
numsouthsti <- length(which(artnet2$region == "South" & artnet2$prep_sti == 1))
numweststi <- length(which(artnet2$region == "West" & artnet2$prep_sti == 1))
num1517sti <- length(which(artnet2$age.cat == "15-17" & artnet2$prep_sti == 1))
num1824sti <- length(which(artnet2$age.cat == "18-24" & artnet2$prep_sti == 1))
num2534sti <- length(which(artnet2$age.cat == "25-34" & artnet2$prep_sti == 1))
num3544sti <- length(which(artnet2$age.cat == "35-44" & artnet2$prep_sti == 1))
num4554sti <- length(which(artnet2$age.cat == "45-54" & artnet2$prep_sti == 1))
num5565sti <- length(which(artnet2$age.cat == "55-65" & artnet2$prep_sti == 1))

# Column 8 - Sexually active, HIV-negative denominator
numprepdenom <- length(which(artnet2$prep_part12mo == 1 & artnet2$prep_hiv == 1))
numwhiteprepdenom <- length(which(artnet2$race.cat == "white" &
                                    artnet2$prep_part12mo == 1 &
                                    artnet2$prep_hiv == 1))
numblackprepdenom <- length(which(artnet2$race.cat == "black" &
                                    artnet2$prep_part12mo == 1 &
                                    artnet2$prep_hiv == 1))
numhispprepdenom <- length(which(artnet2$race.cat == "hispanic" &
                                   artnet2$prep_part12mo == 1 &
                                   artnet2$prep_hiv == 1))
numotherprepdenom <- length(which(artnet2$race.cat == "other" &
                                    artnet2$prep_part12mo == 1 &
                                    artnet2$prep_hiv == 1))
numneprepdenom <- length(which(artnet2$region == "Northeast" &
                                 artnet2$prep_part12mo == 1 &
                                 artnet2$prep_hiv == 1))
nummwprepdenom <- length(which(artnet2$region == "Midwest" &
                                 artnet2$prep_part12mo == 1 &
                                 artnet2$prep_hiv == 1))
numsouthprepdenom <- length(which(artnet2$region == "South" &
                                    artnet2$prep_part12mo == 1 &
                                    artnet2$prep_hiv == 1))
numwestprepdenom <- length(which(artnet2$region == "West" &
                                   artnet2$prep_part12mo == 1 &
                                   artnet2$prep_hiv == 1))
num1517prepdenom <- length(which(artnet2$age.cat == "15-17" &
                                   artnet2$prep_part12mo == 1 &
                                   artnet2$prep_hiv == 1))
num1824prepdenom <- length(which(artnet2$age.cat == "18-24" &
                                   artnet2$prep_part12mo == 1 &
                                   artnet2$prep_hiv == 1))
num2534prepdenom <- length(which(artnet2$age.cat == "25-34" &
                                   artnet2$prep_part12mo == 1 &
                                   artnet2$prep_hiv == 1))
num3544prepdenom <- length(which(artnet2$age.cat == "35-44" &
                                   artnet2$prep_part12mo == 1 &
                                   artnet2$prep_hiv == 1))
num4554prepdenom <- length(which(artnet2$age.cat == "45-54" &
                                   artnet2$prep_part12mo == 1 &
                                   artnet2$prep_hiv == 1))
num5565prepdenom <- length(which(artnet2$age.cat == "55-65" &
                                   artnet2$prep_part12mo == 1 &
                                   artnet2$prep_hiv == 1))

# Summary Table ---------------
# Column 1 - Overall, row %
respondents <- rbind(nrow(artnet2),
                     paste0(numwhite, " (",
                            round(100 * numwhite / num, 1), ")"),
                     paste0(numblack, " (",
                            round(100 * numblack / num, 1), ")"),
                     paste0(numhisp, " (",
                            round(100 * numhisp / num, 1), ")"),
                     paste0(numother, " (",
                            round(100 * numother / num, 1), ")"),
                     paste0(numne, " (",
                            round(100 * numne / num, 1), ")"),
                     paste0(nummw, " (",
                            round(100 * nummw / num, 1), ")"),
                     paste0(numsouth, " (",
                            round(100 * numsouth / num, 1), ")"),
                     paste0(numwest, " (",
                            round(100 * numwest / num, 1), ")"),
                     paste0(num1517, " (",
                            round(100 * num1517 / num, 1), ")"),
                     paste0(num1824, " (",
                            round(100 * num1824 / num, 1), ")"),
                     paste0(num2534, " (",
                            round(100 * num2534 / num, 1), ")"),
                     paste0(num3544, " (",
                            round(100 * num3544 / num, 1), ")"),
                     paste0(num4554, " (",
                            round(100 * num4554 / num, 1), ")"),
                     paste0(num5565, " (",
                            round(100 * num5565 / num, 1), ")"))

# Column 2 - Adult (Row %)
adultmen <- rbind(paste0(numadult, " (",
                         round(100 * numadult / num, 1), ")"),
                     paste0(numwhiteadult, " (",
                            round(100 * numwhiteadult / numwhite, 1), ")"),
                     paste0(numblackadult, " (",
                            round(100 * numblackadult / numblack, 1), ")"),
                     paste0(numhispadult, " (",
                            round(100 * numhispadult / numhisp, 1), ")"),
                     paste0(numotheradult, " (",
                            round(100 * numotheradult / numother, 1), ")"),
                     paste0(numneadult, " (",
                            round(100 * numneadult / numne, 1), ")"),
                     paste0(nummwadult, " (",
                            round(100 * nummwadult / nummw, 1), ")"),
                     paste0(numsouthadult, " (",
                            round(100 * numsouthadult / numsouth, 1), ")"),
                     paste0(numwestadult, " (",
                            round(100 * numwestadult / numwest, 1), ")"),
                     paste0(num1517adult, " (",
                            round(100 * num1517adult / num1517, 1), ")"),
                     paste0(num1824adult, " (",
                            round(100 * num1824adult / num1824, 1), ")"),
                     paste0(num2534adult, " (",
                            round(100 * num2534adult / num2534, 1), ")"),
                     paste0(num3544adult, " (",
                            round(100 * num3544adult / num3544, 1), ")"),
                     paste0(num4554adult, " (",
                            round(100 * num4554adult / num4554, 1), ")"),
                     paste0(num5565adult, " (",
                            round(100 * num5565adult / num5565, 1), ")"))

# Column 3 - Adult (Row %)
hivneg <- rbind(paste0(numhivneg, " (",
                         round(100 * numhivneg / num, 1), ")"),
                  paste0(numwhitehivneg, " (",
                         round(100 * numwhitehivneg / numwhite, 1), ")"),
                  paste0(numblackhivneg, " (",
                         round(100 * numblackhivneg / numblack, 1), ")"),
                  paste0(numhisphivneg, " (",
                         round(100 * numhisphivneg / numhisp, 1), ")"),
                  paste0(numotherhivneg, " (",
                         round(100 * numotherhivneg / numother, 1), ")"),
                  paste0(numnehivneg, " (",
                         round(100 * numnehivneg / numne, 1), ")"),
                  paste0(nummwhivneg, " (",
                         round(100 * nummwhivneg / nummw, 1), ")"),
                  paste0(numsouthhivneg, " (",
                         round(100 * numsouthhivneg / numsouth, 1), ")"),
                  paste0(numwesthivneg, " (",
                         round(100 * numwesthivneg / numwest, 1), ")"),
                  paste0(num1517hivneg, " (",
                         round(100 * num1517hivneg / num1517, 1), ")"),
                  paste0(num1824hivneg, " (",
                         round(100 * num1824hivneg / num1824, 1), ")"),
                  paste0(num2534hivneg, " (",
                         round(100 * num2534hivneg / num2534, 1), ")"),
                  paste0(num3544hivneg, " (",
                         round(100 * num3544hivneg / num3544, 1), ")"),
                  paste0(num4554hivneg, " (",
                         round(100 * num4554hivneg / num4554, 1), ")"),
                  paste0(num5565hivneg, " (",
                         round(100 * num5565hivneg / num5565, 1), ")"))

# Column 4 - Male partner in past 6 months
past6mos <- rbind(paste0(nump6mo, " (",
                       round(100 * nump6mo / num, 1), ")"),
                paste0(numwhitep6mo, " (",
                       round(100 * numwhitep6mo / numwhite, 1), ")"),
                paste0(numblackp6mo, " (",
                       round(100 * numblackp6mo / numblack, 1), ")"),
                paste0(numhispp6mo, " (",
                       round(100 * numhispp6mo / numhisp, 1), ")"),
                paste0(numotherp6mo, " (",
                       round(100 * numotherp6mo / numother, 1), ")"),
                paste0(numnep6mo, " (",
                       round(100 * numnep6mo / numne, 1), ")"),
                paste0(nummwp6mo, " (",
                       round(100 * nummwp6mo / nummw, 1), ")"),
                paste0(numsouthp6mo, " (",
                       round(100 * numsouthp6mo / numsouth, 1), ")"),
                paste0(numwestp6mo, " (",
                       round(100 * numwestp6mo / numwest, 1), ")"),
                paste0(num1517p6mo, " (",
                       round(100 * num1517p6mo / num1517, 1), ")"),
                paste0(num1824p6mo, " (",
                       round(100 * num1824p6mo / num1824, 1), ")"),
                paste0(num2534p6mo, " (",
                       round(100 * num2534p6mo / num2534, 1), ")"),
                paste0(num3544p6mo, " (",
                       round(100 * num3544p6mo / num3544, 1), ")"),
                paste0(num4554p6mo, " (",
                       round(100 * num4554p6mo / num4554, 1), ")"),
                paste0(num5565p6mo, " (",
                       round(100 * num5565p6mo / num5565, 1), ")"))


# Column 5 - Non-monogamous
nonmonog <- rbind(paste0(numnonmonog, " (",
                         round(100 * numnonmonog / num, 1), ")"),
                  paste0(numwhitenonmonog, " (",
                         round(100 * numwhitenonmonog / numwhite, 1), ")"),
                  paste0(numblacknonmonog, " (",
                         round(100 * numblacknonmonog / numblack, 1), ")"),
                  paste0(numhispnonmonog, " (",
                         round(100 * numhispnonmonog / numhisp, 1), ")"),
                  paste0(numothernonmonog, " (",
                         round(100 * numothernonmonog / numother, 1), ")"),
                  paste0(numnenonmonog, " (",
                         round(100 * numnenonmonog / numne, 1), ")"),
                  paste0(nummwnonmonog, " (",
                         round(100 * nummwnonmonog / nummw, 1), ")"),
                  paste0(numsouthnonmonog, " (",
                         round(100 * numsouthnonmonog / numsouth, 1), ")"),
                  paste0(numwestnonmonog, " (",
                         round(100 * numwestnonmonog / numwest, 1), ")"),
                  paste0(num1517nonmonog, " (",
                         round(100 * num1517nonmonog / num1517, 1), ")"),
                  paste0(num1824nonmonog, " (",
                         round(100 * num1824nonmonog / num1824, 1), ")"),
                  paste0(num2534nonmonog, " (",
                         round(100 * num2534nonmonog / num2534, 1), ")"),
                  paste0(num3544nonmonog, " (",
                         round(100 * num3544nonmonog / num3544, 1), ")"),
                  paste0(num4554nonmonog, " (",
                         round(100 * num4554nonmonog / num4554, 1), ")"),
                  paste0(num5565nonmonog, " (",
                         round(100 * num5565nonmonog / num5565, 1), ")"))

CAI <- rbind(paste0(numcai, " (",
                    round(100 * numcai / num, 1), ")"),
             paste0(numwhitecai, " (",
                    round(100 * numwhitecai / numwhite, 1), ")"),
             paste0(numblackcai, " (",
                    round(100 * numblackcai / numblack, 1), ")"),
             paste0(numhispcai, " (",
                    round(100 * numhispcai / numhisp, 1), ")"),
             paste0(numothercai, " (",
                    round(100 * numothercai / numother, 1), ")"),
             paste0(numnecai, " (",
                    round(100 * numnecai / numne, 1), ")"),
             paste0(nummwcai, " (",
                    round(100 * nummwcai / nummw, 1), ")"),
             paste0(numsouthcai, " (",
                    round(100 * numsouthcai / numsouth, 1), ")"),
             paste0(numwestcai, " (",
                    round(100 * numwestcai / numwest, 1), ")"),
             paste0(num1517cai, " (",
                    round(100 * num1517cai / num1517, 1), ")"),
             paste0(num1824cai, " (",
                    round(100 * num1824cai / num1824, 1), ")"),
             paste0(num2534cai, " (",
                    round(100 * num2534cai / num2534, 1), ")"),
             paste0(num3544cai, " (",
                    round(100 * num3544cai / num3544, 1), ")"),
             paste0(num4554cai, " (",
                    round(100 * num4554cai / num4554, 1), ")"),
             paste0(num5565cai, " (",
                    round(100 * num5565cai / num5565, 1), ")"))

sti <- rbind(paste0(numsti, " (",
                    round(100 * numsti / num, 1), ")"),
             paste0(numwhitesti, " (",
                    round(100 * numwhitesti / numwhite, 1), ")"),
             paste0(numblacksti, " (",
                    round(100 * numblacksti / numblack, 1), ")"),
             paste0(numhispsti, " (",
                    round(100 * numhispsti / numhisp, 1), ")"),
             paste0(numothersti, " (",
                    round(100 * numothersti / numother, 1), ")"),
             paste0(numnesti, " (",
                    round(100 * numnesti / numne, 1), ")"),
             paste0(nummwsti, " (",
                    round(100 * nummwsti / nummw, 1), ")"),
             paste0(numsouthsti, " (",
                    round(100 * numsouthsti / numsouth, 1), ")"),
             paste0(numweststi, " (",
                    round(100 * numweststi / numwest, 1), ")"),
             paste0(num1517sti, " (",
                    round(100 * num1517sti / num1517, 1), ")"),
             paste0(num1824sti, " (",
                    round(100 * num1824sti / num1824, 1), ")"),
             paste0(num2534sti, " (",
                    round(100 * num2534sti / num2534, 1), ")"),
             paste0(num3544sti, " (",
                    round(100 * num3544sti / num3544, 1), ")"),
             paste0(num4554sti, " (",
                    round(100 * num4554sti / num4554, 1), ")"),
             paste0(num5565sti, " (",
                    round(100 * num5565sti / num5565, 1), ")"))

sexacthivnegdenom <- rbind(paste0(numprepdenom, " (",
                                  round(100 * numprepdenom / num, 1), ")"),
                           paste0(numwhiteprepdenom, " (",
                                  round(100 * numwhiteprepdenom / numwhite, 1),
                                  ")"),
                           paste0(numblackprepdenom, " (",
                                  round(100 * numblackprepdenom / numblack, 1),
                                  ")"),
                           paste0(numhispprepdenom, " (",
                                  round(100 * numhispprepdenom / numhisp, 1),
                                  ")"),
                           paste0(numotherprepdenom, " (",
                                  round(100 * numotherprepdenom / numother, 1),
                                  ")"),
                           paste0(numneprepdenom, " (",
                                  round(100 * numneprepdenom / numne, 1),
                                  ")"),
                           paste0(nummwprepdenom, " (",
                                  round(100 * nummwprepdenom / nummw, 1),
                                  ")"),
                           paste0(numsouthprepdenom, " (",
                                  round(100 * numsouthprepdenom / numsouth, 1),
                                  ")"),
                           paste0(numwestprepdenom, " (",
                                  round(100 * numwestprepdenom / numwest, 1),
                                  ")"),
                           paste0(num1517prepdenom, " (",
                                  round(100 * num1517prepdenom / num1517, 1),
                                  ")"),
                           paste0(num1824prepdenom, " (",
                                  round(100 * num1824prepdenom / num1824, 1),
                                  ")"),
                           paste0(num2534prepdenom, " (",
                                  round(100 * num2534prepdenom / num2534, 1),
                                  ")"),
                           paste0(num3544prepdenom, " (",
                                  round(100 * num3544prepdenom / num3544, 1),
                                  ")"),
                           paste0(num4554prepdenom, " (",
                                  round(100 * num4554prepdenom / num4554, 1),
                                  ")"),
                           paste0(num5565prepdenom, " (",
                                  round(100 * num5565prepdenom / num5565, 1),
                                  ")"))

Table2 <- cbind(respondents, adultmen, hivneg, past6mos, nonmonog,
                     CAI, sti, sexacthivnegdenom)
colnames(Table2) <- c("Respondents", "1 - Adult Men", "2 - HIV-negative",
                           "3 - Male P6MO", "4 - Not monog w/ HIV-",
                           "5- CAI 6 months", "6 - Bacterial STI 12 months",
                           "sexually active, HIV-negative denom")
rownames(Table2) <- c("All", "White", "Black", "Hisp", "Other",
                           "Northeast", "Midwest", "South", "West",
                           "15-17", "18-24", "25-34", "35-44", "45-54", "55-65")

View(Table2)

# Output Table -----------------------------------------------------------------
write.csv(Table2, file = "Output/Table 2.csv")
