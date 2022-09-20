# Get the working directory
getwd()
# Change the working directory
setwd("/Users/Tonkos/OneDrive/Documents/ECON124/Homework 1A")
# Question 1
# Install packages
install.packages("readstata13")
# reading the packages and loading data fakenews.dta and survey.dta
library(readstata13)
fndat <- read.dta13("fakenews.dta")
sdat <- read.dta13("survey.dta")

# Question 2
# Omitting NA
na.omit(fndat$fb_share)
# Creating varaible x to determine the amount of pro-trump post
x <- sum(na.omit(fndat$fb_share[fndat$pro == "Trump"]))* 1000
y <- sum(na.omit(fndat$fb_share[fndat$pro == "Clinton"]))* 1000
cat( x, "pro-Trump articles were shared on Facebook, and", y, "pro-Clinton articles were shared, in this dataset", sep = " ")

# Question 3
# Database comprehensive 
cat(sum(fndat$buzzfeed), "is the total number of Buzzfeed articles it contians and percent of its articles don't appear in others is", sum((fndat$buzzfeed[!fndat$snopes & !fndat$politifact])/sum(fndat$buzzfeed))*100, sep = " ", "\n")
cat(sum(fndat$snopes), "is the total number of Snopes articles it contians and percent of its articles don't appear in others is", sum((fndat$snopes[!fndat$buzzfeed & !fndat$politifact])/sum(fndat$snopes))*100, sep = " ", "\n")
cat(sum(fndat$politifact), "is the total number of Politifacts articles it contians and percent of its articles don't appear in others is", sum((fndat$politifact & !fndat$buzzfeed & !fndat$snopes)/sum(fndat$politifact))*100, sep = " ", "\n")

# Question 4
# Using cat to produce an answer to the question 3
cat(round(mean(sdat$MediaMinutesPerDay), digits = 0), "is the average minutes per day spent on media while", round((mean(sdat$SocialMediaMinutesPerDay[sdat$MediaMinutesPerDay > 0])/mean(sdat$MediaMinutesPerDay[sdat$MediaMinutesPerDay > 0]))*100, digits = 0), "percent was spent on social media out of all media", "\n")

# Question 5
# Bar plot
source <- tapply(rep(1, nrow(sdat)), sdat$MostImportantSource, sum)
barplot(sort(source, decreasing = FALSE), main = "Most Important Source of News", ylab = "Observation on what was most important source of news", col = rainbow(7), las=2)

# Question 6
#logistic regression
logreg <- glm(sdat$UseSocialMedia ~ sdat$Voted_Trump + sdat$Income + sdat$Educ_Years + sdat$Age, data = sdat, family = 'binomial')
summary(logreg)
# B
rss <- logreg$deviance / logreg$null.deviance
rss
# C
(coef(logreg)["sdat$Voted_Trump"])*100

# Question 7
df <- sdat[sdat$Heard == "Yes",]
source <- tapply(rep(1, nrow(df)), df$BarLabel, sum)
source2 <- tapply(rep(1, nrow(df)), df[,c("ThoughtTrue", "BarLabel")], sum)
thoughtcol <- c("red", "yellow", "green")
labelcol <- c("green", "red", "orange", "blue")
barplot(source, main = "Most Important Source of News", ylab = "Article", col =  labelcol) 
legend("topright", c("Big True", "Fake", "Placebo", "Small True"), fill= labelcol)
barplot(source2, main = "Most Important Source of News", ylab = "Article", col =  thoughtcol) 
legend("topright", c("Yes", "Not Sure", "No"), fill= thoughtcol)

# Question 8
# A
pf <- sdat[sdat$BarLabel == "Fake" | sdat$BarLabel == "Placebo",]
pf$BarLabelbi <- ifelse(pf$BarLabel == "Fake", 1, 0)
pf$Heardbi <- ifelse(pf$Heard == "Yes", 1, 0)
# B
reg <-  glm(pf$BarLabelbi ~ pf$Heardbi, data =  pf)
summary(reg)

# Question 9
# A
K <- ifelse(sdat$BarLabel != "Fake" | sdat$BarLabel != "Placebo" & sdat$ThoughtTrue == "Yes", 1, ifelse(sdat$ThoughtTrue == "Not sure", 0.5, 0))
# B
Party <- ifelse(sdat$Republican == 1, "Rep", "Dem")
# C
reg <- glm(K~Party, data = sdat)
summary(reg)
# D
reg <- glm(K~Party + sdat$Income + sdat$Educ_Years + sdat$Age, data = sdat)
summary(reg)

# Question 10
# A
B <- ifelse(sdat$ThoughtTrue == "Yes", 1, ifelse(sdat$ThoughtTrue == "Not sure", 0.5, 0))
# B
reg <- glm(B~Party, data = sdat)
summary(reg)
# C
regint <- glm(B~Party + sdat$Democrat*sdat$ArticleProClinton + sdat$Republican*sdat$ArticleProTrump, data = sdat)
summary(regint)
