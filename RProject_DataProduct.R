---
  title: 'Cyber Threats and Sources from AgConnections 2019/2020'
  author: 'Ty Weglarz'
  date: '4-7-2020'
  output:
    word_document: default
    pdf_document: default 
---
   
getwd()
## Load data set into R
attackInfo <- read.csv("~/Documents/GitHub/R_CyberAlert_Project/20200204_1025_repot_job_256.csv",
           stringsAsFactors = FALSE)

## Verify data set is clean and looks as expected

head(attackInfo)
tail(attackInfo)

## Install Packages and dependencies

install.packages("dplyr")
install.packages("plyr")
install.packages("shiny")
install.packages("rmarkdown")
install.packages("lubridate")
install.packages("yaml")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("jtools")
install.packages("rsconnect")


##Isolate Date to day of week for regressions by using dplyr and regex to
##Exctract first three characters only
newAttackInfo <- attackInfo %>% 
  dplyr::mutate(dayOfWeek.Received = str_extract(Day.Received, "^.{0,3}"))

rsconnect::setAccountInfo(name='tweglarz', token='52BA42B99189ED4349D4B027CD234423', secret='jNCQ1iZ86sTj0xIc0RTCqRTVXEsRxB0zIdpKFxtx')
##Predictors
vars <- "Source.Country1 + dayOfWeek.Received1"
vars1 <- "Threat.Category1 + dayOfWeek.Received1"
vars2 <-"dayOfWeek.Received1"

##Data for regression. Seeing if Source Country and day of week can predict the threat category
datReg3 <- data.frame(matrix(rnorm(1164), ncol=3))
names(datReg3) <- c("Threat.Category", paste("Source.Country", 1:2, sep=""))
names(datReg3) <- c("Threat.Category", "Source.Country1", paste("dayOfWeek.Received", 1:1, sep=""))
##Creat a formula and paste response
forM <- as.formula(paste("Threat.Category ~", vars, sep =""))
forM
#Fit the model
mod <- lm(forM, data = datReg3)
summ(mod)

##Data for regression. Seeing if day of week recieved and threat category
##can predict source country
datReg4 <- data.frame(matrix(rnorm(1164), ncol=3))
names(datReg4) <- c("Source.Country", paste("Threat.Category", 1:2, sep=""))
names(datReg4) <- c("Source.Country", "Threat.Category1", paste("dayOfWeek.Received", 1:1, sep=""))
##Creat a formula and paste response
forM2 <- as.formula(paste("Source.Country ~", vars1, sep=""))
forM2
#Fit the model
mod1 <- lm(forM2, data = datReg4)
summ(mod1)

##Data for regression. Seeing if Threat Category can predict the Source Country
datReg5 <-data.frame(matrix(rnorm(1164), ncol=2))
names(datReg5) <- c("Source.Country", paste("Threat.Category", 1:1, sep=""))
##Creat a formula and paste response
forM3 <- as.formula(paste("Source.Country ~", vars))
#Fit the model
mod2 <- lm(forM3, data = datReg5)
summ(mod2)



##Data for regression. Seeing if Source Country can predict the Threat Category
datReg6 <-data.frame(matrix(rnorm(1164), ncol=2))
names(datReg6) <- c("Threat.Category", paste("Source.Country", 1:1, sep=""))
##Creat a formula and paste response
forM4 <- as.formula(paste("Threat.Category ~", vars))
#Fit the model
mod3 <- lm(forM4, data = datReg6)
summ(mod3)

##Conclusion of Regression
####With a coefficient of determination; 
####or R-squared value of less than 0.02 it is evident 
####that the threat category is not a good predictor 
####of the source country. The vice-verse is also true, 
####as source country of a threat is not a good predictor 
####of the type of threat. In conculstion, the location of
####a threats origin and the type of threat thrown at AgConnections
####do not correlate. 

getwd()
#Plotting and Sorting Data

####Calculating frequencies
tab <- table(attackInfo$Source.Country)
####Sorting
tab_s <- sort(tab)
####Extract Top 10 Countries
top10 <- tail(names(tab_s), 10)
####Subsetting dataframe
d_s <- subset(attackInfo, Source.Country %in% top10)
####Ordering factor by level 
d_s$Source.Country <- factor(d_s$Source.Country, levels = rev(top10))

##Plotting
ggplot(d_s, aes(x = Source.Country, fill=I("green"), col=I("black"))) +
    geom_bar() +
    theme_classic() +
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
    ggtitle("Total Threats by Country") +
    geom_text(stat='count',aes(label=..count..),vjust=-1) +
    xlab("Country") +
    ylab("Total")


####Calculating frequencies
tab1 <- table(attackInfo$Threat.Category)
####Sorting
tab_s1 <- sort(tab1)
####Extracting Top 10 Threat Categories
top5 <- tail(names(tab_s1), 5)
####Subsetting dataframe
d_s1 <- subset(attackInfo, Threat.Category %in% top5)
####Ordering factor by level
d_s1$Threat.Category <- factor(d_s1$Threat.Category, levels = rev(top5))

##Plotting
ggplot(d_s1, aes(x = Threat.Category, fill=I("green"), col=I("black"))) +
  geom_bar() +
  theme_classic() +
  ggtitle("Total Cyber Threats by Type") +
  xlab("Type of Threat") +
  geom_text(stat='count',aes(label=..count..),vjust=-1) +
  ylab("Total")
