## PrEP Indications   ##
## Figure 1 Script   ##
## 2019-06-06         ##

### Data and Packages
rm(list = ls())
library(dplyr)
library(tidyr)
library(readxl)
library(MASS)
library(haven)
library(tidyverse)
library(ggplot2)
library(gridExtra)

## Data Cleaning/Management Script
source("Analyses/Data Cleaning.R", echo = FALSE)

# Build table -----------------------------------------------------------------
# Column 1 - Race Description
race.cat <- cbind(c(rep("Total", 6),
                  rep("White", 6),
                  rep("Black", 6),
                  rep("Hispanic", 6),
                  rep("Other", 6)))

# Column 2 - Age Description
age.cat <- cbind(c(rep(c("15-17", "18-24", "25-34",
                          "35-44", "45-54", "55-65"),
                        5)))

# Column 3 - PrEP Denominator (Sexually Active, HIV-Negative)
prepdenom <- rbind(
  length(which(artnet2$age.cat == "15-17" & artnet2$prep_hiv == 1 & artnet2$prep_part12mo == 1)),
  length(which(artnet2$age.cat == "18-24" & artnet2$prep_hiv == 1 & artnet2$prep_part12mo == 1)),
  length(which(artnet2$age.cat == "25-34" & artnet2$prep_hiv == 1 & artnet2$prep_part12mo == 1)),
  length(which(artnet2$age.cat == "35-44" & artnet2$prep_hiv == 1 & artnet2$prep_part12mo == 1)),
  length(which(artnet2$age.cat == "45-54" & artnet2$prep_hiv == 1 & artnet2$prep_part12mo == 1)),
  length(which(artnet2$age.cat == "55-65" & artnet2$prep_hiv == 1 & artnet2$prep_part12mo == 1)),
  length(which(artnet2$age.cat == "15-17" & artnet2$race.cat == "white" &
                 artnet2$prep_hiv == 1 & artnet2$prep_part12mo == 1)),
  length(which(artnet2$age.cat == "18-24" & artnet2$race.cat == "white" &
                 artnet2$prep_hiv == 1 & artnet2$prep_part12mo == 1)),
  length(which(artnet2$age.cat == "25-34" & artnet2$race.cat == "white" &
                 artnet2$prep_hiv == 1 & artnet2$prep_part12mo == 1)),
  length(which(artnet2$age.cat == "35-44" & artnet2$race.cat == "white" &
                 artnet2$prep_hiv == 1 & artnet2$prep_part12mo == 1)),
  length(which(artnet2$age.cat == "45-54" & artnet2$race.cat == "white" &
                 artnet2$prep_hiv == 1 & artnet2$prep_part12mo == 1)),
  length(which(artnet2$age.cat == "55-65" & artnet2$race.cat == "white" &
                 artnet2$prep_hiv == 1 & artnet2$prep_part12mo == 1)),
  length(which(artnet2$age.cat == "15-17" & artnet2$race.cat == "black" &
                 artnet2$prep_hiv == 1 & artnet2$prep_part12mo == 1)),
  length(which(artnet2$age.cat == "18-24" & artnet2$race.cat == "black" &
                 artnet2$prep_hiv == 1 & artnet2$prep_part12mo == 1)),
  length(which(artnet2$age.cat == "25-34" & artnet2$race.cat == "black" &
                 artnet2$prep_hiv == 1 & artnet2$prep_part12mo == 1)),
  length(which(artnet2$age.cat == "35-44" & artnet2$race.cat == "black" &
                 artnet2$prep_hiv == 1 & artnet2$prep_part12mo == 1)),
  length(which(artnet2$age.cat == "45-54" & artnet2$race.cat == "black" &
                 artnet2$prep_hiv == 1 & artnet2$prep_part12mo == 1)),
  length(which(artnet2$age.cat == "55-65" & artnet2$race.cat == "black" &
                 artnet2$prep_hiv == 1 & artnet2$prep_part12mo == 1)),
  length(which(artnet2$age.cat == "15-17" & artnet2$race.cat == "hispanic" &
                 artnet2$prep_hiv == 1 & artnet2$prep_part12mo == 1)),
  length(which(artnet2$age.cat == "18-24" & artnet2$race.cat == "hispanic" &
                 artnet2$prep_hiv == 1 & artnet2$prep_part12mo == 1)),
  length(which(artnet2$age.cat == "25-34" & artnet2$race.cat == "hispanic" &
                 artnet2$prep_hiv == 1 & artnet2$prep_part12mo == 1)),
  length(which(artnet2$age.cat == "35-44" & artnet2$race.cat == "hispanic" &
                 artnet2$prep_hiv == 1 & artnet2$prep_part12mo == 1)),
  length(which(artnet2$age.cat == "45-54" & artnet2$race.cat == "hispanic" &
                 artnet2$prep_hiv == 1 & artnet2$prep_part12mo == 1)),
  length(which(artnet2$age.cat == "55-65" & artnet2$race.cat == "hispanic" &
                 artnet2$prep_hiv == 1 & artnet2$prep_part12mo == 1)),
  length(which(artnet2$age.cat == "15-17" & artnet2$race.cat == "other" &
                 artnet2$prep_hiv == 1 & artnet2$prep_part12mo == 1)),
  length(which(artnet2$age.cat == "18-24" & artnet2$race.cat == "other" &
                 artnet2$prep_hiv == 1 & artnet2$prep_part12mo == 1)),
  length(which(artnet2$age.cat == "25-34" & artnet2$race.cat == "other" &
                 artnet2$prep_hiv == 1 & artnet2$prep_part12mo == 1)),
  length(which(artnet2$age.cat == "35-44" & artnet2$race.cat == "other" &
                 artnet2$prep_hiv == 1 & artnet2$prep_part12mo == 1)),
  length(which(artnet2$age.cat == "45-54" & artnet2$race.cat == "other" &
                 artnet2$prep_hiv == 1 & artnet2$prep_part12mo == 1)),
  length(which(artnet2$age.cat == "55-65" & artnet2$race.cat == "other" &
                 artnet2$prep_hiv == 1 & artnet2$prep_part12mo == 1)))

# Column 4 - MSM with indications for PrEP
prepind <- rbind(
  length(which(artnet2$age.cat == "15-17" & artnet2$prepind_any == 1)),
  length(which(artnet2$age.cat == "18-24" & artnet2$prepind_any == 1)),
  length(which(artnet2$age.cat == "25-34" & artnet2$prepind_any == 1)),
  length(which(artnet2$age.cat == "35-44" & artnet2$prepind_any == 1)),
  length(which(artnet2$age.cat == "45-54" & artnet2$prepind_any == 1)),
  length(which(artnet2$age.cat == "55-65" & artnet2$prepind_any == 1)),
  length(which(artnet2$age.cat == "15-17" & artnet2$race.cat == "white" &
                 artnet2$prepind_any == 1)),
  length(which(artnet2$age.cat == "18-24" & artnet2$race.cat == "white" &
                 artnet2$prepind_any == 1)),
  length(which(artnet2$age.cat == "25-34" & artnet2$race.cat == "white" &
                 artnet2$prepind_any == 1)),
  length(which(artnet2$age.cat == "35-44" & artnet2$race.cat == "white" &
                 artnet2$prepind_any == 1)),
  length(which(artnet2$age.cat == "45-54" & artnet2$race.cat == "white" &
                 artnet2$prepind_any == 1)),
  length(which(artnet2$age.cat == "55-65" & artnet2$race.cat == "white" &
                 artnet2$prepind_any == 1)),
  length(which(artnet2$age.cat == "15-17" & artnet2$race.cat == "black" &
                 artnet2$prepind_any == 1)),
  length(which(artnet2$age.cat == "18-24" & artnet2$race.cat == "black" &
                 artnet2$prepind_any == 1)),
  length(which(artnet2$age.cat == "25-34" & artnet2$race.cat == "black" &
                 artnet2$prepind_any == 1)),
  length(which(artnet2$age.cat == "35-44" & artnet2$race.cat == "black" &
                 artnet2$prepind_any == 1)),
  length(which(artnet2$age.cat == "45-54" & artnet2$race.cat == "black" &
                 artnet2$prepind_any == 1)),
  length(which(artnet2$age.cat == "55-65" & artnet2$race.cat == "black" &
                 artnet2$prepind_any == 1)),
  length(which(artnet2$age.cat == "15-17" & artnet2$race.cat == "hispanic" &
                 artnet2$prepind_any == 1)),
  length(which(artnet2$age.cat == "18-24" & artnet2$race.cat == "hispanic" &
                 artnet2$prepind_any == 1)),
  length(which(artnet2$age.cat == "25-34" & artnet2$race.cat == "hispanic" &
                 artnet2$prepind_any == 1)),
  length(which(artnet2$age.cat == "35-44" & artnet2$race.cat == "hispanic" &
                 artnet2$prepind_any == 1)),
  length(which(artnet2$age.cat == "45-54" & artnet2$race.cat == "hispanic" &
                 artnet2$prepind_any == 1)),
  length(which(artnet2$age.cat == "55-65" & artnet2$race.cat == "hispanic" &
                 artnet2$prepind_any == 1)),
  length(which(artnet2$age.cat == "15-17" & artnet2$race.cat == "other" &
                 artnet2$prepind_any == 1)),
  length(which(artnet2$age.cat == "18-24" & artnet2$race.cat == "other" &
                 artnet2$prepind_any == 1)),
  length(which(artnet2$age.cat == "25-34" & artnet2$race.cat == "other" &
                 artnet2$prepind_any == 1)),
  length(which(artnet2$age.cat == "35-44" & artnet2$race.cat == "other" &
                 artnet2$prepind_any == 1)),
  length(which(artnet2$age.cat == "45-54" & artnet2$race.cat == "other" &
                 artnet2$prepind_any == 1)),
  length(which(artnet2$age.cat == "55-65" & artnet2$race.cat == "other" &
                 artnet2$prepind_any == 1)))

# Column 5 - Percentage
indpct <- round(100 * prepind/prepdenom, 1)

# Build Summary Table
prep <- cbind(race.cat, age.cat, prepdenom, prepind, indpct)
# colnames(prep) <- c("Race Category", "Age Category", "PrEP Denom", "PrEP Indicated",
#                     "Percent Indicated")
prep <- as.data.frame(prep)

# Reclassifying variables
prep$Race.Cat <- as.character(prep$V1)
prep$Age.Cat <- as.character(prep$V2)
prep$PrEPDenom <- as.numeric(as.character(prep$V3))
prep$Indicated <- as.numeric(as.character(prep$V4))
prep$indpct <- as.numeric(as.character(prep$V5))

# Drop columns
prep <- prep[, 6:ncol(prep)]

# Add confidence limits
# phat +/- z* sqrt((phat)(1 - phat))/n)
prep$pctLCI  <- prep$indpct - 1.96 * sqrt((prep$indpct) * (100 - prep$indpct) / prep$PrEPDenom)
prep$pctUCI  <- prep$indpct + 1.96 * sqrt((prep$indpct) * (100 - prep$indpct) / prep$PrEPDenom)

# Figure 1
figure1 <- ggplot(data = prep, aes(x = Age.Cat, y = indpct, fill = Race.Cat)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = pctLCI, ymax = pctUCI), width = .2,
                position = position_dodge(.9)) +
  ylab("Percent Indicated for PrEP") +
  xlab("Age Category") +
  #labs(title = ("Percent of Sexually Active, HIV-Negative MSM Eligible for PrEP \n by Race/Ethnicity and Age")) +
  ylim(0, 100) +
  geom_hline(yintercept = 24.7, linetype = "solid",
             color = "red", size = 1) +
  geom_text(aes(label = indpct),
            position = position_dodge(width = 0.9),
            size = 3,
            vjust = -3.0) +
  theme_bw()
