# Test

# @Tuure Parviainen
# 11/18/2017
#working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #works in Rstudio only

library(dplyr)

#read data
url <- "http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets"
# web address for math class data
url_math <- paste(url, "student-mat.csv", sep = "/")

# read the math class questionaire data into memory
math <- read.table(url_math, sep = ";" , header=TRUE)
# web address for portuguese class data
url_por <- paste(url, "student-por.csv", sep ="/")
# read the portuguese class questionaire data into memory
por <- read.table(url_por, sep = ";", header = TRUE)

# common columns to use as identifiers
join_by <- c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet")

# join the two datasets by the selected identifiers
math_por <- inner_join(math, por, by = join_by)

alc <- select(math_por, one_of(join_by))

# columns that were not used for joining the data
notjoined_columns <- colnames(math)[!colnames(math) %in% join_by]

# print out the columns not used for joining
#notjoined_columns

# for every column name not used for joining...
for(column_name in notjoined_columns) {
  # select two columns from 'math_por' with the same original name
  two_columns <- select(math_por, starts_with(column_name))
  # select the first column vector of those two columns
  first_column <- select(two_columns, 1)[[1]]
  
  # if that first column  vector is numeric...
  if(is.numeric(first_column)) {
    # take a rounded average of each row of the two columns and
    # add the resulting vector to the alc data frame
    alc[column_name] <- round(rowMeans(two_columns))
  } else { # else if it's not numeric...
    # add the first column vector to the alc data frame
    alc[column_name] <- first_column
  }
}

# adding the alcohol use
alc <- mutate(alc, alc_use = (Dalc + Walc) / 2)
# adding the high usage / low usage as TRUE / FALSE, respectively.
alc <- mutate(alc, high_use = alc_use > 2)

# Glimpse at the data
dim(alc)
glimpse(alc)
# Writing the csv to the data folder 
write.csv(alc, file= "data/ex3alc.csv")

# Data Set Information:
#   
#   This data approach student achievement in secondary education of two Portuguese schools. The data attributes include student grades, demographic, social and school related features) and it was collected by using school reports and questionnaires. Two datasets are provided regarding the performance in two distinct subjects: Mathematics (mat) and Portuguese language (por). In [Cortez and Silva, 2008], the two datasets were modeled under binary/five-level classification and regression tasks. Important note: the target attribute G3 has a strong correlation with attributes G2 and G1. This occurs because G3 is the final year grade (issued at the 3rd period), while G1 and G2 correspond to the 1st and 2nd period grades. It is more difficult to predict G3 without G2 and G1, but such prediction is much more useful (see paper source for more details).
# 
# 
# Attribute Information:
#   
#   # Attributes for both student-mat.csv (Math course) and student-por.csv (Portuguese language course) datasets: 
# 1 school - student's school (binary: 'GP' - Gabriel Pereira or 'MS' - Mousinho da Silveira) 
# 2 sex - student's sex (binary: 'F' - female or 'M' - male) 
# 3 age - student's age (numeric: from 15 to 22) 
# 4 address - student's home address type (binary: 'U' - urban or 'R' - rural) 
# 5 famsize - family size (binary: 'LE3' - less or equal to 3 or 'GT3' - greater than 3) 
# 6 Pstatus - parent's cohabitation status (binary: 'T' - living together or 'A' - apart) 
# 7 Medu - mother's education (numeric: 0 - none, 1 - primary education (4th grade), 2 â???" 5th to 9th grade, 3 â???" secondary education or 4 â???" higher education) 
# 8 Fedu - father's education (numeric: 0 - none, 1 - primary education (4th grade), 2 â???" 5th to 9th grade, 3 â???" secondary education or 4 â???" higher education) 
# 9 Mjob - mother's job (nominal: 'teacher', 'health' care related, civil 'services' (e.g. administrative or police), 'at_home' or 'other') 
# 10 Fjob - father's job (nominal: 'teacher', 'health' care related, civil 'services' (e.g. administrative or police), 'at_home' or 'other') 
# 11 reason - reason to choose this school (nominal: close to 'home', school 'reputation', 'course' preference or 'other') 
# 12 guardian - student's guardian (nominal: 'mother', 'father' or 'other') 
# 13 traveltime - home to school travel time (numeric: 1 - <15 min., 2 - 15 to 30 min., 3 - 30 min. to 1 hour, or 4 - >1 hour) 
# 14 studytime - weekly study time (numeric: 1 - <2 hours, 2 - 2 to 5 hours, 3 - 5 to 10 hours, or 4 - >10 hours) 
# 15 failures - number of past class failures (numeric: n if 1<=n<3, else 4) 
# 16 schoolsup - extra educational support (binary: yes or no) 
# 17 famsup - family educational support (binary: yes or no) 
# 18 paid - extra paid classes within the course subject (Math or Portuguese) (binary: yes or no) 
# 19 activities - extra-curricular activities (binary: yes or no) 
# 20 nursery - attended nursery school (binary: yes or no) 
# 21 higher - wants to take higher education (binary: yes or no) 
# 22 internet - Internet access at home (binary: yes or no) 
# 23 romantic - with a romantic relationship (binary: yes or no) 
# 24 famrel - quality of family relationships (numeric: from 1 - very bad to 5 - excellent) 
# 25 freetime - free time after school (numeric: from 1 - very low to 5 - very high) 
# 26 goout - going out with friends (numeric: from 1 - very low to 5 - very high) 
# 27 Dalc - workday alcohol consumption (numeric: from 1 - very low to 5 - very high) 
# 28 Walc - weekend alcohol consumption (numeric: from 1 - very low to 5 - very high) 
# 29 health - current health status (numeric: from 1 - very bad to 5 - very good) 
# 30 absences - number of school absences (numeric: from 0 to 93) 
# 
# # these grades are related with the course subject, Math or Portuguese: 
# 31 G1 - first period grade (numeric: from 0 to 20) 
# 31 G2 - second period grade (numeric: from 0 to 20) 
# 32 G3 - final grade (numeric: from 0 to 20, output target)
