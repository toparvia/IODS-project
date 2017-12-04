# to introduce Principal Components Analysis (PCA). The data originates from the United Nations Development Programme. See their data page for more information. For a nice overview see also the calculating the human development indices pdf.
#
# Most of the variable names have been shortened and two new variables have been computed. See the meta file for the modified data here for descriptions.

# Link 1. http://hdr.undp.org/en/content/human-development-index-hdi
# Link 2. http://hdr.undp.org/sites/default/files/hdr2015_technical_notes.pdf
# Link 3. https://raw.githubusercontent.com/TuomoNieminen/Helsinki-Open-Data-Science/master/datasets/human_meta.txt

packages_to_load c("dyplr", "corrplot", "factominer", "tidyr", "ggplot2")

# read the human data
human <- read.table("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human1.txt", sep  =",", header = T)

# look at the (column) names of human
names(human)

# look at the structure of human
str(human)

# print out summaries of the variables
summary(human)

# access the stringr package
library(stringr)

# look at the structure of the GNI column in 'human'
str(human$GNI)

# remove the commas from GNI and print out a numeric version of it
str_replace(human$GNI, pattern=",", replace ="") %>% as.numeric()

# columns to keep
keep <- c("Country", "Edu2.FM", "Labo.FM", "Life.Exp", "Edu.Exp", "GNI", "Mat.Mor", "Ado.Birth", "Parli.F")

# select the 'keep' columns
human <- select(human, one_of(keep))

# print out a completeness indicator of the 'human' data
complete.cases(human)

# print out the data along with a completeness indicator as the last column
test <- data.frame(human[-1], comp = complete.cases(human))
test
# filter out all rows with NA values
human_ <- filter(human, complete.cases(human) == TRUE)

# look at the last 10 observations of human
tail(human, 10)

# define the last indice we want to keep
last <- nrow(human) - 7

# choose everything until the last 7 observations
human_ <- human[1:last, ]

# add countries as rownames
rownames(human_) <- human_$Country

# remove the Country variable
human_ <- select(human, -Country)

# Access GGally
library(GGally)

# visualize the 'human_' variables
ggpairs(human_)

# compute the correlation matrix and visualize it with corrplot
cor(human_) %>% corrplot()

# PCA with R
#
# Principal Component Analysis (PCA) can be performed by two sightly different matrix decomposition methods from linear algebra: the Eigenvalue Decomposition and the Singular Value Decomposition (SVD).
#
# There are two functions in the default package distribution of R that can be used to perform PCA: princomp() and prcomp(). The prcomp() function uses the SVD and is the preferred, more numerically accurate method.
#
# Both methods quite literally decompose a data matrix into a product of smaller matrices, which let's us extract the underlying principal components. This makes it possible to approximate a lower dimensional representation of the data by choosing only a few principal components.

# standardize the variables
human_std <- scale(human)

# print out summaries of the standardized variables
summary(human_std)

# perform principal component analysis (with the SVD method)
pca_human <- prcomp(human_std)

# draw a biplot of the principal component representation and the original variables
biplot(pca_human, choices = 1:2, cex = c(0.8, 1), col = c("grey40", "deeppink2"))

# create and print out a summary of pca_human
s <- summary(pca_human)


# rounded percetanges of variance captured by each PC
pca_pr <- round(1*s$importance[2, ]*100, digits = 1)

# print out the percentages of variance
pca_pr

# create object pc_lab to be used as axis labels
pc_lab <- paste0(names(pca_pr), " (", pca_pr, "%)")

# draw a biplot
biplot(pca_human, cex = c(0.8, 1), col = c("grey40", "deeppink2"), xlab = pc_lab[1], ylab = pc_lab[2])

# column names to keep in the dataset
keep_columns <- c("Tea", "How", "how", "sugar", "where", "lunch")

# select the 'keep_columns' to create a new dataset
tea_time <- select(tea, one_of(keep_columns))

# look at the summaries and structure of the data
tea_time %>% summary()
str(tea_time)

# visualize the dataset
gather(tea_time) %>% ggplot(aes(value)) + facet_wrap("key", scales = "free") + geom_bar() + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

# Multiple Correspondence Analysis (MCA) is a method to analyze qualitative data and it is an extension of Correspondence analysis (CA). MCA can be used to detect patterns or structure in the data as well as in dimension reduction.

# tea_time is available

# multiple correspondence analysis
mca <- MCA(tea_time, graph = FALSE)

# summary of the model
summary(mca)

# visualize MCA
plot(mca, invisible=c("ind"), habillage = "quali")
