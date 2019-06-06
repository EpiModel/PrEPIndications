## PrEP Indications   ##
## Table 3 Script     ##
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
# Column 1 - PrEP-eligible denominator (Male partner in past 12 months, HIV-negative)
num <- length(which(artnet2$prep_part12mo == 1 & artnet2$prep_hiv == 1))
numwhite <- length(which(artnet2$race.cat == "white" & artnet2$prep_part12mo == 1 & artnet2$prep_hiv == 1))
numblack <- length(which(artnet2$race.cat == "black" & artnet2$prep_part12mo == 1 & artnet2$prep_hiv == 1))
numhisp <- length(which(artnet2$race.cat == "hispanic" & artnet2$prep_part12mo == 1 & artnet2$prep_hiv == 1))
numother <- length(which(artnet2$race.cat == "other" & artnet2$prep_part12mo == 1 & artnet2$prep_hiv == 1))
numne <- length(which(artnet2$region == "Northeast" & artnet2$prep_part12mo == 1 & artnet2$prep_hiv == 1))
nummw <- length(which(artnet2$region == "Midwest" & artnet2$prep_part12mo == 1 & artnet2$prep_hiv == 1))
numsouth <- length(which(artnet2$region == "South" & artnet2$prep_part12mo == 1 & artnet2$prep_hiv == 1))
numwest <- length(which(artnet2$region == "West" & artnet2$prep_part12mo == 1 & artnet2$prep_hiv == 1))
num1517 <- length(which(artnet2$age.cat == "15-17" & artnet2$prep_part12mo == 1 & artnet2$prep_hiv == 1))
num1824 <- length(which(artnet2$age.cat == "18-24" & artnet2$prep_part12mo == 1 & artnet2$prep_hiv == 1))
num2534 <- length(which(artnet2$age.cat == "25-34" & artnet2$prep_part12mo == 1 & artnet2$prep_hiv == 1))
num3544 <- length(which(artnet2$age.cat == "35-44" & artnet2$prep_part12mo == 1 & artnet2$prep_hiv == 1))
num4554 <- length(which(artnet2$age.cat == "45-54" & artnet2$prep_part12mo == 1 & artnet2$prep_hiv == 1))
num5565 <- length(which(artnet2$age.cat == "55-65" & artnet2$prep_part12mo == 1 & artnet2$prep_hiv == 1))

# Column 2 - Meet 4 base USPHS Indications (leaving out adult for now - only affects 15-17 year olds)
numbase <- length(which(artnet2$prep_hiv == 1 & artnet2$prep_part6mo == 1 & artnet2$prep_nonmonog == 1))
numwhitebase <- length(which(artnet2$race.cat == "white" & artnet2$prep_hiv == 1 & artnet2$prep_part6mo == 1 & artnet2$prep_nonmonog == 1))
numblackbase <- length(which(artnet2$race.cat == "black" & artnet2$prep_hiv == 1 & artnet2$prep_part6mo == 1 & artnet2$prep_nonmonog == 1))
numhispbase <- length(which(artnet2$race.cat == "hispanic" & artnet2$prep_hiv == 1 & artnet2$prep_part6mo == 1 & artnet2$prep_nonmonog == 1))
numotherbase <- length(which(artnet2$race.cat == "other" & artnet2$prep_hiv == 1 & artnet2$prep_part6mo == 1 & artnet2$prep_nonmonog == 1))
numnebase <- length(which(artnet2$region == "Northeast" & artnet2$prep_hiv == 1 & artnet2$prep_part6mo == 1 & artnet2$prep_nonmonog == 1))
nummwbase <- length(which(artnet2$region == "Midwest" & artnet2$prep_hiv == 1 & artnet2$prep_part6mo == 1 & artnet2$prep_nonmonog == 1))
numsouthbase <- length(which(artnet2$region == "South" & artnet2$prep_hiv == 1 & artnet2$prep_part6mo == 1 & artnet2$prep_nonmonog == 1))
numwestbase <- length(which(artnet2$region == "West" & artnet2$prep_hiv == 1 & artnet2$prep_part6mo == 1 & artnet2$prep_nonmonog == 1))
num1517base <- length(which(artnet2$age.cat == "15-17" & artnet2$prep_hiv == 1 & artnet2$prep_part6mo == 1 & artnet2$prep_nonmonog == 1))
num1824base <- length(which(artnet2$age.cat == "18-24" & artnet2$prep_hiv == 1 & artnet2$prep_part6mo == 1 & artnet2$prep_nonmonog == 1))
num2534base <- length(which(artnet2$age.cat == "25-34" & artnet2$prep_hiv == 1 & artnet2$prep_part6mo == 1 & artnet2$prep_nonmonog == 1))
num3544base <- length(which(artnet2$age.cat == "35-44" & artnet2$prep_hiv == 1 & artnet2$prep_part6mo == 1 & artnet2$prep_nonmonog == 1))
num4554base <- length(which(artnet2$age.cat == "45-54" & artnet2$prep_hiv == 1 & artnet2$prep_part6mo == 1 & artnet2$prep_nonmonog == 1))
num5565base <- length(which(artnet2$age.cat == "55-65" & artnet2$prep_hiv == 1 & artnet2$prep_part6mo == 1 & artnet2$prep_nonmonog == 1))

# Column 3 - Base + CAI
numbasecai <- length(which(artnet2$prepind_uai == 1))
numwhitebasecai <- length(which(artnet2$race.cat == "white" & artnet2$prepind_uai == 1))
numblackbasecai <- length(which(artnet2$race.cat == "black" & artnet2$prepind_uai == 1))
numhispbasecai <- length(which(artnet2$race.cat == "hispanic" & artnet2$prepind_uai == 1))
numotherbasecai <- length(which(artnet2$race.cat == "other" & artnet2$prepind_uai == 1))
numnebasecai <- length(which(artnet2$region == "Northeast" & artnet2$prepind_uai == 1))
nummwbasecai <- length(which(artnet2$region == "Midwest" & artnet2$prepind_uai == 1))
numsouthbasecai <- length(which(artnet2$region == "South" & artnet2$prepind_uai == 1))
numwestbasecai <- length(which(artnet2$region == "West" & artnet2$prepind_uai == 1))
num1517basecai <- length(which(artnet2$age.cat == "15-17" & artnet2$prepind_uai == 1))
num1824basecai <- length(which(artnet2$age.cat == "18-24" & artnet2$prepind_uai == 1))
num2534basecai <- length(which(artnet2$age.cat == "25-34" & artnet2$prepind_uai == 1))
num3544basecai <- length(which(artnet2$age.cat == "35-44" & artnet2$prepind_uai == 1))
num4554basecai <- length(which(artnet2$age.cat == "45-54" & artnet2$prepind_uai == 1))
num5565basecai <- length(which(artnet2$age.cat == "55-65" & artnet2$prepind_uai == 1))

# Column 4 - Base + STI
numbasesti <- length(which(artnet2$prepind_sti == 1))
numwhitebasesti <- length(which(artnet2$race.cat == "white" & artnet2$prepind_sti == 1))
numblackbasesti <- length(which(artnet2$race.cat == "black" & artnet2$prepind_sti == 1))
numhispbasesti <- length(which(artnet2$race.cat == "hispanic" & artnet2$prepind_sti == 1))
numotherbasesti <- length(which(artnet2$race.cat == "other" & artnet2$prepind_sti == 1))
numnebasesti <- length(which(artnet2$region == "Northeast" & artnet2$prepind_sti == 1))
nummwbasesti <- length(which(artnet2$region == "Midwest" & artnet2$prepind_sti == 1))
numsouthbasesti <- length(which(artnet2$region == "South" & artnet2$prepind_sti == 1))
numwestbasesti <- length(which(artnet2$region == "West" & artnet2$prepind_sti == 1))
num1517basesti <- length(which(artnet2$age.cat == "15-17" & artnet2$prepind_sti == 1))
num1824basesti <- length(which(artnet2$age.cat == "18-24" & artnet2$prepind_sti == 1))
num2534basesti <- length(which(artnet2$age.cat == "25-34" & artnet2$prepind_sti == 1))
num3544basesti <- length(which(artnet2$age.cat == "35-44" & artnet2$prepind_sti == 1))
num4554basesti <- length(which(artnet2$age.cat == "45-54" & artnet2$prepind_sti == 1))
num5565basesti <- length(which(artnet2$age.cat == "55-65" & artnet2$prepind_sti == 1))

# Columns 5 and 6 - PrEP-eligible / PrEP denominator and PrEP-eligible / Overall denominator
numprepany <- length(which(artnet2$prepind_any == 1))
numwhiteprepany <- length(which(artnet2$race.cat == "white" & artnet2$prepind_any == 1))
numblackprepany <- length(which(artnet2$race.cat == "black" & artnet2$prepind_any == 1))
numhispprepany <- length(which(artnet2$race.cat == "hispanic" & artnet2$prepind_any == 1))
numotherprepany <- length(which(artnet2$race.cat == "other" & artnet2$prepind_any == 1))
numneprepany <- length(which(artnet2$region == "Northeast" & artnet2$prepind_any == 1))
nummwprepany <- length(which(artnet2$region == "Midwest" & artnet2$prepind_any == 1))
numsouthprepany <- length(which(artnet2$region == "South" & artnet2$prepind_any == 1))
numwestprepany <- length(which(artnet2$region == "West" & artnet2$prepind_any == 1))
num1517prepany <- length(which(artnet2$age.cat == "15-17" & artnet2$prepind_any == 1))
num1824prepany <- length(which(artnet2$age.cat == "18-24" & artnet2$prepind_any == 1))
num2534prepany <- length(which(artnet2$age.cat == "25-34" & artnet2$prepind_any == 1))
num3544prepany <- length(which(artnet2$age.cat == "35-44" & artnet2$prepind_any == 1))
num4554prepany <- length(which(artnet2$age.cat == "45-54" & artnet2$prepind_any == 1))
num5565prepany <- length(which(artnet2$age.cat == "55-65" & artnet2$prepind_any == 1))

overallnum <- nrow(artnet2)
overallnumwhite <- length(which(artnet2$race.cat == "white"))
overallnumblack <- length(which(artnet2$race.cat == "black"))
overallnumhisp <- length(which(artnet2$race.cat == "hispanic"))
overallnumother <- length(which(artnet2$race.cat == "other"))
overallnumne <- length(which(artnet2$region == "Northeast"))
overallnummw <- length(which(artnet2$region == "Midwest"))
overallnumsouth <- length(which(artnet2$region == "South"))
overallnumwest <- length(which(artnet2$region == "West"))
overallnum1517 <- length(which(artnet2$age.cat == "15-17"))
overallnum1824 <- length(which(artnet2$age.cat == "18-24"))
overallnum2534 <- length(which(artnet2$age.cat == "25-34"))
overallnum3544 <- length(which(artnet2$age.cat == "35-44"))
overallnum4554 <- length(which(artnet2$age.cat == "45-54"))
overallnum5565 <- length(which(artnet2$age.cat == "55-65"))

# Summary Table ---------------
# Column 1 - Overall, row %
prepdenom <- rbind(paste0(num, " (",
                            round(100 * num / num, 1), ")"),
                     paste0(numwhite, " (",
                            round(100 * numwhite / numwhite, 1), ")"),
                     paste0(numblack, " (",
                            round(100 * numblack / numblack, 1), ")"),
                     paste0(numhisp, " (",
                            round(100 * numhisp / numhisp, 1), ")"),
                     paste0(numother, " (",
                            round(100 * numother / numother, 1), ")"),
                     paste0(numne, " (",
                            round(100 * numne / numne, 1), ")"),
                     paste0(nummw, " (",
                            round(100 * nummw / nummw, 1), ")"),
                     paste0(numsouth, " (",
                            round(100 * numsouth / numsouth, 1), ")"),
                     paste0(numwest, " (",
                            round(100 * numwest / numwest, 1), ")"),
                     paste0(num1517, " (",
                            round(100 * num1517 / num1517, 1), ")"),
                     paste0(num1824, " (",
                            round(100 * num1824 / num1824, 1), ")"),
                     paste0(num2534, " (",
                            round(100 * num2534 / num2534, 1), ")"),
                     paste0(num3544, " (",
                            round(100 * num3544 / num3544, 1), ")"),
                     paste0(num4554, " (",
                            round(100 * num4554 / num4554, 1), ")"),
                     paste0(num5565, " (",
                            round(100 * num5565 / num5565, 1), ")"))

# Column 2 - Meet First 4 (Base Indications)
basemen <- rbind(paste0(numbase, " (",
                         round(100 * numbase / num, 1), ")"),
                  paste0(numwhitebase, " (",
                         round(100 * numwhitebase / numwhite, 1), ")"),
                  paste0(numblackbase, " (",
                         round(100 * numblackbase / numblack, 1), ")"),
                  paste0(numhispbase, " (",
                         round(100 * numhispbase / numhisp, 1), ")"),
                  paste0(numotherbase, " (",
                         round(100 * numotherbase / numother, 1), ")"),
                  paste0(numnebase, " (",
                         round(100 * numnebase / numne, 1), ")"),
                  paste0(nummwbase, " (",
                         round(100 * nummwbase / nummw, 1), ")"),
                  paste0(numsouthbase, " (",
                         round(100 * numsouthbase / numsouth, 1), ")"),
                  paste0(numwestbase, " (",
                         round(100 * numwestbase / numwest, 1), ")"),
                  paste0(num1517base, " (",
                         round(100 * num1517base / num1517, 1), ")"),
                  paste0(num1824base, " (",
                         round(100 * num1824base / num1824, 1), ")"),
                  paste0(num2534base, " (",
                         round(100 * num2534base / num2534, 1), ")"),
                  paste0(num3544base, " (",
                         round(100 * num3544base / num3544, 1), ")"),
                  paste0(num4554base, " (",
                         round(100 * num4554base / num4554, 1), ")"),
                  paste0(num5565base, " (",
                         round(100 * num5565base / num5565, 1), ")"))

# Column 3 - Base + CAI (Row %)
commoncai <- rbind(paste0(numbasecai, " (",
                       round(100 * numbasecai / num, 1), ")"),
                paste0(numwhitebasecai, " (",
                       round(100 * numwhitebasecai / numwhite, 1), ")"),
                paste0(numblackbasecai, " (",
                       round(100 * numblackbasecai / numblack, 1), ")"),
                paste0(numhispbasecai, " (",
                       round(100 * numhispbasecai / numhisp, 1), ")"),
                paste0(numotherbasecai, " (",
                       round(100 * numotherbasecai / numother, 1), ")"),
                paste0(numnebasecai, " (",
                       round(100 * numnebasecai / numne, 1), ")"),
                paste0(nummwbasecai, " (",
                       round(100 * nummwbasecai / nummw, 1), ")"),
                paste0(numsouthbasecai, " (",
                       round(100 * numsouthbasecai / numsouth, 1), ")"),
                paste0(numwestbasecai, " (",
                       round(100 * numwestbasecai / numwest, 1), ")"),
                paste0(num1517basecai, " (",
                       round(100 * num1517basecai / num1517, 1), ")"),
                paste0(num1824basecai, " (",
                       round(100 * num1824basecai / num1824, 1), ")"),
                paste0(num2534basecai, " (",
                       round(100 * num2534basecai / num2534, 1), ")"),
                paste0(num3544basecai, " (",
                       round(100 * num3544basecai / num3544, 1), ")"),
                paste0(num4554basecai, " (",
                       round(100 * num4554basecai / num4554, 1), ")"),
                paste0(num5565basecai, " (",
                       round(100 * num5565basecai / num5565, 1), ")"))

# Column 4 - Base + STI
commonsti <- rbind(paste0(numbasesti, " (",
                         round(100 * numbasesti / num, 1), ")"),
                  paste0(numwhitebasesti, " (",
                         round(100 * numwhitebasesti / numwhite, 1), ")"),
                  paste0(numblackbasesti, " (",
                         round(100 * numblackbasesti / numblack, 1), ")"),
                  paste0(numhispbasesti, " (",
                         round(100 * numhispbasesti / numhisp, 1), ")"),
                  paste0(numotherbasesti, " (",
                         round(100 * numotherbasesti / numother, 1), ")"),
                  paste0(numnebasesti, " (",
                         round(100 * numnebasesti / numne, 1), ")"),
                  paste0(nummwbasesti, " (",
                         round(100 * nummwbasesti / nummw, 1), ")"),
                  paste0(numsouthbasesti, " (",
                         round(100 * numsouthbasesti / numsouth, 1), ")"),
                  paste0(numwestbasesti, " (",
                         round(100 * numwestbasesti / numwest, 1), ")"),
                  paste0(num1517basesti, " (",
                         round(100 * num1517basesti / num1517, 1), ")"),
                  paste0(num1824basesti, " (",
                         round(100 * num1824basesti / num1824, 1), ")"),
                  paste0(num2534basesti, " (",
                         round(100 * num2534basesti / num2534, 1), ")"),
                  paste0(num3544basesti, " (",
                         round(100 * num3544basesti / num3544, 1), ")"),
                  paste0(num4554basesti, " (",
                         round(100 * num4554basesti / num4554, 1), ")"),
                  paste0(num5565basesti, " (",
                         round(100 * num5565basesti / num5565, 1), ")"))

# Column 5 - PrEP-eligible / PrEP denominator
commonany <- rbind(paste0(numprepany, " (",
                          round(100 * numprepany / num, 1), ")"),
                   paste0(numwhiteprepany, " (",
                          round(100 * numwhiteprepany / numwhite, 1), ")"),
                   paste0(numblackprepany, " (",
                          round(100 * numblackprepany / numblack, 1), ")"),
                   paste0(numhispprepany, " (",
                          round(100 * numhispprepany / numhisp, 1), ")"),
                   paste0(numotherprepany, " (",
                          round(100 * numotherprepany / numother, 1), ")"),
                   paste0(numneprepany, " (",
                          round(100 * numneprepany / numne, 1), ")"),
                   paste0(nummwprepany, " (",
                          round(100 * nummwprepany / nummw, 1), ")"),
                   paste0(numsouthprepany, " (",
                          round(100 * numsouthprepany / numsouth, 1), ")"),
                   paste0(numwestprepany, " (",
                          round(100 * numwestprepany / numwest, 1), ")"),
                   paste0(num1517prepany, " (",
                          round(100 * num1517prepany / num1517, 1), ")"),
                   paste0(num1824prepany, " (",
                          round(100 * num1824prepany / num1824, 1), ")"),
                   paste0(num2534prepany, " (",
                          round(100 * num2534prepany / num2534, 1), ")"),
                   paste0(num3544prepany, " (",
                          round(100 * num3544prepany / num3544, 1), ")"),
                   paste0(num4554prepany, " (",
                          round(100 * num4554prepany / num4554, 1), ")"),
                   paste0(num5565prepany, " (",
                          round(100 * num5565prepany / num5565, 1), ")"))

# Column 6 - PrEP-eligible / Overall Denominator
commonoveralldenom <- rbind(paste0(numprepany, " (",
                                   round(100 * numprepany / overallnum, 1), ")"),
                            paste0(numwhiteprepany, " (",
                                   round(100 * numwhiteprepany / overallnumwhite, 1), ")"),
                            paste0(numblackprepany, " (",
                                   round(100 * numblackprepany / overallnumblack, 1), ")"),
                            paste0(numhispprepany, " (",
                                   round(100 * numhispprepany / overallnumhisp, 1), ")"),
                            paste0(numotherprepany, " (",
                                   round(100 * numotherprepany / overallnumother, 1), ")"),
                            paste0(numneprepany, " (",
                                   round(100 * numneprepany / overallnumne, 1), ")"),
                            paste0(nummwprepany, " (",
                                   round(100 * nummwprepany / overallnummw, 1), ")"),
                            paste0(numsouthprepany, " (",
                                   round(100 * numsouthprepany / overallnumsouth, 1), ")"),
                            paste0(numwestprepany, " (",
                                   round(100 * numwestprepany / overallnumwest, 1), ")"),
                            paste0(num1517prepany, " (",
                                   round(100 * num1517prepany / overallnum1517, 1), ")"),
                            paste0(num1824prepany, " (",
                                   round(100 * num1824prepany / overallnum1824, 1), ")"),
                            paste0(num2534prepany, " (",
                                   round(100 * num2534prepany / overallnum2534, 1), ")"),
                            paste0(num3544prepany, " (",
                                   round(100 * num3544prepany / overallnum3544, 1), ")"),
                            paste0(num4554prepany, " (",
                                   round(100 * num4554prepany / overallnum4554, 1), ")"),
                            paste0(num5565prepany, " (",
                                   round(100 * num5565prepany / overallnum5565, 1), ")"))

Table3 <- cbind(prepdenom, basemen, commoncai, commonsti, commonany,
                     commonoveralldenom)
colnames(Table3) <- c("PrEP Denominator", "Base Indications", "Base + CAI",
                           "Base + STI", "% of PrEP Denom Indicated for PrEP",
                           "% of Overall Denom Indicated for PrEP")
rownames(Table3) <- c("All", "White", "Black", "Hisp", "Other",
                           "Northeast", "Midwest", "South", "West",
                           "15-17", "18-24", "25-34", "35-44", "45-54", "55-65")
View(Table3)

# Output Table -----------------------------------------------------------------
write.csv(Table3, file = "Output/Table 3.csv")
