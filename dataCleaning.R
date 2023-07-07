setwd("/Users/rifat/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/PhD CS USASK/Research/Suicide ABM CSTEM/suicidal ideation ethica/")

library(remotes)
# remotes::install_github('cdriveraus/ctsem', INSTALL_opts = "--no-multiarch", dependencies = c("Depends", "Imports"))
library(Rcpp)
library(ctsem)
library(plyr)
library(dplyr)
library(ICC)
library(rmcorr)
library(Rcpp)
library(data.table)
library(tidyverse)
library(splitstackshape)
library(ggplot2)
library(gridExtra)

df <- read.csv("ScalaASuicidalityData_19Feb2019.csv", header = TRUE) # 1315 obs

# see the labels for survey status
unique(df$dur_1) # [1] "answered" "exp_canc"

# delete expired surveys
df <- subset(df, dur_1 == "answered") # 838 obs
df <- na.omit(df)

indata$ID <- as.factor (indata$ActUserID)
indata$record_time <- str_remove(indata$record_time, "-06:00")


# Convert recorded_date and start_date columns to datetime objects
indata$record_time <- as.POSIXct(indata$record_time, format = "%Y-%m-%dT%H:%M:%OS")
indata$startDate <- as.POSIXct(indata$startDate, format = "%Y-%m-%d %H:%M")

indata <- indata %>%
  group_by(ID) %>%
  mutate(timeInStudy_hr = difftime(record_time, startDate, units = "hours"))

indata$timeInStudy_hr <- as.numeric(gsub(" hours", "", indata$timeInStudy_hr))
indata$timeInStudy_hr <- round(indata$timeInStudy_hr, 2)

# Calculate the Z-score within each ID group to identify outliers >= 3 or <= -3
indata <- indata %>%
  group_by(ID) %>%
  mutate(z_score_suicid = scale(res3_Suicid))

# remove outliers based on z-score
indata <- subset(indata, z_score_suicid > -3.0 & z_score_suicid < 3.0)

# check ID's with less than or equals to 5 observations
## remove only less than two samples IDs
id_counts <- table(indata$ID)
ids_less_than_5 <- names(id_counts[id_counts < 5]) # "11" "13" "21" 26" "38" "42"

# remove ID's with less than or equals to 5 observations
indata <- indata %>%
  filter(!ID %in% ids_less_than_5)

# after the creation of violing plot remove ID's without variability
unwanted_ids <- c("39", "48", "49", "50")
indata <- indata %>%
  filter(!ID %in% unwanted_ids)


indata1 <- subset(indata, ID %in% c(4, 5, 6, 7, 8))

violin1 <- ggplot(indata1, aes(x = ID, y = res3_Suicid, fill = ID)) +
  geom_violin() +
  geom_boxplot(width = 0.2, fill = "white", color = "black") +
  labs(x = "Individual ID", y = "Suicidality") +
  ggtitle("Individual Level Variability") +
  theme_minimal()
violin1 # ID 4, 6, 8 have extreme values, remove these from indata 

# Remove rows for the target ID where the measurement is above the threshold
indata <- indata[!(indata$ID == "4" & indata$res3_Suicid >= 60), ] # from visual inspection
indata <- indata[!(indata$ID == "6" & indata$res3_Suicid >= 20), ]
indata <- indata[!(indata$ID == "8" & indata$res3_Suicid >= 15), ]

indata2 <- subset(indata, ID %in% c(15, 16, 17, 36))

violin2 <- ggplot(indata2, aes(x = ID, y = res3_Suicid, fill = ID)) +
  geom_violin() +
  geom_boxplot(width = 0.2, fill = "white", color = "black") +
  labs(x = "Individual ID", y = "Suicidality") +
  ggtitle("Individual Level Variability") +
  theme_minimal()
violin2 # ID 16 has extreme values, remove these from indata 

# Remove rows for the target ID where the measurement is above the threshold
indata <- indata[!(indata$ID == "16" & indata$res3_Suicid >= 15), ]

indata3 <- subset(indata, ID %in% c(18, 19, 20, 23))
violin3 <- ggplot(indata3, aes(x = ID, y = res3_Suicid, fill = ID)) +
  geom_violin() +
  geom_boxplot(width = 0.2, fill = "white", color = "black") +
  labs(x = "Individual ID", y = "Suicidality") +
  ggtitle("Individual Level Variability") +
  theme_minimal()
violin3 # ID 18, 19, 23 have extreme values, remove these from indata 

# Remove rows for the target ID where the measurement is above and below the threshold
indata <- indata[!(indata$ID == "18" & indata$res3_Suicid >= 90), ]
indata <- indata[!(indata$ID == "19" & indata$res3_Suicid >= 65), ]
indata <- indata[!(indata$ID == "23" & indata$res3_Suicid >= 85), ]
indata <- indata[!(indata$ID == "23" & indata$res3_Suicid <= 25), ]

indata4 <- subset(indata, ID %in% c(27, 28, 29, 30))

violin4 <- ggplot(indata4, aes(x = ID, y = res3_Suicid, fill = ID)) +
  geom_violin() +
  geom_boxplot(width = 0.2, fill = "white", color = "black") +
  labs(x = "Individual ID", y = "Suicidality") +
  ggtitle("Individual Level Variability") +
  theme_minimal()
violin4 # ID 27 and 30 have extreme values, ignore at this moment

indata5 <- subset(indata, ID %in% c(31, 32, 33, 34))

violin5 <- ggplot(indata5, aes(x = ID, y = res3_Suicid, fill = ID)) +
  geom_violin() +
  geom_boxplot(width = 0.2, fill = "white", color = "black") +
  labs(x = "Individual ID", y = "Suicidality") +
  ggtitle("Individual Level Variability") +
  theme_minimal()
violin5 # ID 31, 32, 33, 34 have extreme values, remove these from indata 

# Remove rows for the target ID where the measurement is above and below the threshold
indata <- indata[!(indata$ID == "31" & indata$res3_Suicid >= 50), ]
indata <- indata[!(indata$ID == "31" & indata$res3_Suicid <= 5), ]
indata <- indata[!(indata$ID == "32" & indata$res3_Suicid >= 95), ]
indata <- indata[!(indata$ID == "34" & indata$res3_Suicid >= 27), ]


indata6 <- subset(indata, ID %in% c(37, 40, 41))

violin6 <- ggplot(indata6, aes(x = ID, y = res3_Suicid, fill = ID)) +
  geom_violin() +
  geom_boxplot(width = 0.2, fill = "white", color = "black") +
  labs(x = "Individual ID", y = "Suicidality") +
  ggtitle("Individual Level Variability") +
  theme_minimal()
violin6 # ID 41 has extreme values, perhaps ignore. Also ID 39 does not have any variablity. remove them entirely from the data set 


indata7 <- subset(indata, ID %in% c(43, 44, 45))

violin7 <- ggplot(indata7, aes(x = ID, y = res3_Suicid, fill = ID)) +
  geom_violin() +
  geom_boxplot(width = 0.2, fill = "white", color = "black") +
  labs(x = "Individual ID", y = "Suicidality") +
  ggtitle("Individual Level Variability") +
  theme_minimal()
violin7 # ID 44, 45, 46 have extreme values, ignore 46

# Remove rows for the target ID where the measurement is above and below the threshold
indata <- indata[!(indata$ID == "44" & indata$res3_Suicid <= 10), ]
indata <- indata[!(indata$ID == "45" & indata$res3_Suicid >= 55), ]


indata8 <- subset(indata, ID %in% c(46, 47, 51))

violin8 <- ggplot(indata8, aes(x = ID, y = res3_Suicid, fill = ID)) +
  geom_violin() +
  geom_boxplot(width = 0.2, fill = "white", color = "black") +
  labs(x = "Individual ID", y = "Suicidality") +
  ggtitle("Individual Level Variability") +
  theme_minimal()
violin8 # ID 48, 49, 50 remove for lack of variability

write.csv(indata, file = "indata.csv")

# Arrange plots side by side
grid.arrange(violin1, violin2, violin3, violin4, ncol = 2)
grid.arrange(violin5, violin6, violin7, violin8, ncol = 2)

