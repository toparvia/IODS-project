# title: "Chapter 5. Dimensionality reduction techniques"
# author: "Tuure Parviainen"
# date: "November 30, 2017"



require("easypackages") # for loading and installing many packages at one time
packages_to_load <- c("broom", "dplyr", "tidyverse", "corrplot", "ggplot2", "GGally", "devtools", "ggthemes", "stringr")
packages(packages_to_load, prompt = TRUE) # lock'n'load install/load

human <- read.table("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human1.txt", sep  =",", header = T)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # to set directory to source file

# remove the commas from GNI and replace with a numeric version of it
human$GNI <- str_replace(human$GNI, pattern=",", replace ="") %>% as.numeric()

# columns to keep
keep <- c("Country", "Edu2.FM", "Labo.FM", "Life.Exp", "Edu.Exp", "GNI", "Mat.Mor", "Ado.Birth", "Parli.F")

# select the 'keep' columns
human <- select(human, one_of(keep))

# print out the data along with a completeness indicator as the last column
test <- data.frame(human[-1], comp = complete.cases(human))

# filter out all rows with NA values
human_ <- filter(human, complete.cases(human) == TRUE)

# remowing are rows
tail(human_, 7)
chosen <- nrow(human_)-7
humaf <- human_[1:chosen,]

# Adding rownames
rownames(humaf) <- humaf$Country

# Dropping country
humaf <- select(humaf, -Country)

#Writing the 'humaf' .csv
write.csv(humaf, file = "data/humaf.csv")


