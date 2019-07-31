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

# Column 2 - HIV-neg
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

# Column 3 - Male partner in last 12 months
nump12mo <- length(which(artnet2$prep_part12mo == 1))
numwhitep12mo <- length(which(artnet2$race.cat == "white" &
                               artnet2$prep_part12mo == 1))
numblackp12mo <- length(which(artnet2$race.cat == "black" &
                               artnet2$prep_part12mo == 1))
numhispp12mo <- length(which(artnet2$race.cat == "hispanic" &
                              artnet2$prep_part12mo == 1))
numotherp12mo <- length(which(artnet2$race.cat == "other" &
                               artnet2$prep_part12mo == 1))
numnep12mo <- length(which(artnet2$region == "Northeast" &
                            artnet2$prep_part12mo == 1))
nummwp12mo <- length(which(artnet2$region == "Midwest" &
                            artnet2$prep_part12mo == 1))
numsouthp12mo <- length(which(artnet2$region == "South" &
                               artnet2$prep_part12mo == 1))
numwestp12mo <- length(which(artnet2$region == "West" &
                              artnet2$prep_part12mo == 1))
num1517p12mo <- length(which(artnet2$age.cat == "15-17" &
                              artnet2$prep_part12mo == 1))
num1824p12mo <- length(which(artnet2$age.cat == "18-24" &
                              artnet2$prep_part12mo == 1))
num2534p12mo <- length(which(artnet2$age.cat == "25-34" &
                              artnet2$prep_part12mo == 1))
num3544p12mo <- length(which(artnet2$age.cat == "35-44" &
                              artnet2$prep_part12mo == 1))
num4554p12mo <- length(which(artnet2$age.cat == "45-54" &
                              artnet2$prep_part12mo == 1))
num5565p12mo <- length(which(artnet2$age.cat == "55-65" &
                              artnet2$prep_part12mo == 1))

# Column 4 - Sexually active, HIV-negative denominator
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
respondents <- rbind(paste0(num, " (",
                            round(100 * num / num, 1), ")"),
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

# Column 2 - HIV neg
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

# Column 3 - Male partner in past 12 months
past12mos <- rbind(paste0(nump12mo, " (",
                       round(100 * nump12mo / num, 1), ")"),
                paste0(numwhitep12mo, " (",
                       round(100 * numwhitep12mo / numwhite, 1), ")"),
                paste0(numblackp12mo, " (",
                       round(100 * numblackp12mo / numblack, 1), ")"),
                paste0(numhispp12mo, " (",
                       round(100 * numhispp12mo / numhisp, 1), ")"),
                paste0(numotherp12mo, " (",
                       round(100 * numotherp12mo / numother, 1), ")"),
                paste0(numnep12mo, " (",
                       round(100 * numnep12mo / numne, 1), ")"),
                paste0(nummwp12mo, " (",
                       round(100 * nummwp12mo / nummw, 1), ")"),
                paste0(numsouthp12mo, " (",
                       round(100 * numsouthp12mo / numsouth, 1), ")"),
                paste0(numwestp12mo, " (",
                       round(100 * numwestp12mo / numwest, 1), ")"),
                paste0(num1517p12mo, " (",
                       round(100 * num1517p12mo / num1517, 1), ")"),
                paste0(num1824p12mo, " (",
                       round(100 * num1824p12mo / num1824, 1), ")"),
                paste0(num2534p12mo, " (",
                       round(100 * num2534p12mo / num2534, 1), ")"),
                paste0(num3544p12mo, " (",
                       round(100 * num3544p12mo / num3544, 1), ")"),
                paste0(num4554p12mo, " (",
                       round(100 * num4554p12mo / num4554, 1), ")"),
                paste0(num5565p12mo, " (",
                       round(100 * num5565p12mo / num5565, 1), ")"))

# Column 4 - PrEP Denominator
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

# Create Table
Table2 <- cbind(respondents, hivneg, past12mos, sexacthivnegdenom)
colnames(Table2) <- c("Respondents", "1 - HIV-negative",
                           "2 - Male P12MO",
                           "sexually active, HIV-negative denom")
rownames(Table2) <- c("All", "White", "Black", "Hisp", "Other",
                           "Northeast", "Midwest", "South", "West",
                           "15-17", "18-24", "25-34", "35-44", "45-54", "55-65")

View(Table2)

# Output Table -----------------------------------------------------------------
write.csv(Table2, file = "Output/Table 2.csv")
