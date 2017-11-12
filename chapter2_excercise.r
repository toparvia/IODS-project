#install.packages(c("ggplot2", "GGally", "readr", "ggcorrplot", "caret"))
#libraries used
library(readr)
library(ggplot2)
library(GGally)
library(ggcorrplot)
library(caret)
#read data
PKY <- read_delim("http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt", "\t", escape_double = FALSE, trim_ws = TRUE)

#working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #works in Rstudio only
#Some functions for data exploration, courtesy of Highland statistics
source("HighstatLibV10.R")

dim(PKY)
str(PKY)
summary(PKY)

#data is quite big, so quick summaries aren't very informative, looking first at factor correlation

#Factor correlation visually
MyVars <- PKY[c(1:10,59)]
Mypairs(MyVars)
Mydotplot(MyVars)

library(caret) #findCorrelation algorithm
PKY2 <- PKY[-60]
correlations <- abs(cor(PKY2, use="pairwise.complete.obs"))
#correlation of all parameters with positive numbers
correlations.selected <- findCorrelation(correlations, cutoff = .7, exact = TRUE)
#colums with to select by cutoff 0.7
corr <- cor(PKY2[c(correlations.selected,59)])
#correlations of narrow selection and points variable

# Ploting correaltions
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of PKY", 
           ggtheme=theme_bw)


#Now plotting screened parameters for closer look

my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) +
    geom_point() +
    geom_smooth(method=loess, fill="red", color="red", ...) +
    geom_smooth(method=lm, fill="blue", color="blue", ...)
  p
}

#In continous scale for visual purposes, discrete would be correct
g = ggpairs(PKY,columns = c(correlations.selected,59), lower = list(continuous = my_fn))
g

#generate the learning2014 dataset
# dataset variables: gender, age, attitude, deep, stra, surf and points by combining questions in the learning2014 data

# gender      <-  PKY$gender
# age         <-  PKY$age
# attitude    <-  Da+Db+Dc+Dd+De+Df+Dg+Dh+Di+Dj
# deep        <-  d_sm+d_ri+d_ue
# surf        <-  su_lp+su_um+su_sb
# stra        <-  st_os+st_tm

# d_sm     Seeking Meaning           D03+D11+D19+D27
# d_ri     Relating Ideas            D07+D14+D22+D30
# d_ue     Use of Evidence           D06+D15+D23+D31

# su_lp    Lack of Purpose           SU02+SU10+SU18+SU26
# su_um    Unrelated Memorising      SU05+SU13+SU21+SU29
# su_sb    Syllabus-boundness        SU08+SU16+SU24+SU32

# st_os    Organized Studying        ST01+ST09+ST17+ST25
# st_tm    Time Management           ST04+ST12+ST20+ST28

#install.packages("data.table")
library(data.table)

gender      <- data.table(PKY$gender)
age         <- data.table(PKY$Age)
attitude    <- data.table(PKY)[, .(Da+Db+Dc+Dd+De+Df+Dg+Dh+Di+Dj)]
attitude    <- attitude/10
deep        <- data.table(PKY)[, .(D03+D11+D19+D27+D07+D14+D22+D30+D06+D15+D23+D31)]
deep        <- deep/12
surf        <- data.table(PKY)[, .(SU02+SU10+SU18+SU26+SU05+SU13+SU21+SU29+SU08+SU16+SU24+SU32)]
surf        <- surf/12
stra        <- data.table(PKY)[, .(ST01+ST09+ST17+ST25+ST04+ST12+ST20+ST28)]
stra        <- stra/8
points      <- data.table(PKY$Points)
learn2014   <- data.table(cbind(gender, age, attitude, deep, stra, surf, points))
colnames(learn2014) <- c("gender", "age", "attitude", "deep", "stra", "surf", "points")

learn2014   <- learn2014[points >0]
# deep[, mean(V1)] #to calculate number out of data.table

summary(learn2014)
#Correlations
p <-ggpairs(learn2014, mapping = aes(col = gender, alpha = 0.3), lower = list(combo = wrap("facethist", bins = 20)))
p

g = ggpairs(learn2014,columns = c(2:7), lower = list(continuous = my_fn))
g

m1 <- lm(points ~ attitude*deep*stra*surf+age, data=learn2014)
step(m1)
m2 <- lm(points ~ attitude + stra + surf + age + attitude:stra + 
           attitude:surf + stra:surf + attitude:stra:surf, data = learn2014)
m3 <- lm(points ~ attitude+deep+stra+surf+age, data=learn2014)
m4 <- lm(points ~ attitude + stra + age, data = learn2014)
m5 <- lm(points ~ attitude + stra, data = learn2014)
m6 <- lm(points ~ attitude + stra + gender, data = learn2014)
AIC(m1,m2,m3,m4,m5,m6)
#By Akaike's information criteria, the model with the best fit is m5, since it is also most parsimonius.
par(mfrow=c(2,2))
plot(m5)
#Model looks good. Residuals have quite even distribution on both sides of zero and there are only few outliers.
summary(m5)
# Attitude is highly significant and stra is border case with p > 0.09. Model explains only 20 % of variation of the data.
