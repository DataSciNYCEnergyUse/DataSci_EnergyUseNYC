# Import energydata
EnergyData <- read.csv("G:/MPH4thsemester/Managing Data Sci/FW__Joining_group_6_%5bManaging_data_science%5d/CleanedEnergy.csv", header=T, na.strings="", stringsAsFactors=F)
view(EnergyData)
dim(EnergyData) #dimensions of the dataframe(number of rows and columns)
#[1] 31796    18
nrow(EnergyData) 
ncol(EnergyData)
head(EnergyData) # first 6 observation of the dataframe
tail(EnergyData) # last 6 observations of the dataframe
names(EnergyData) #column headers
str(EnergyData) #includes the data type for each column
#selecting only necessary variables
EnergyDatav1 <- EnergyData[c(1,2,5,7,8,12,15:18)] #10 variables
names(EnergyDatav1)[names(EnergyDatav1)=="Weather.Normalized.Source.EUI"] <- "weather" # rename column names
names(EnergyDatav1)[names(EnergyDatav1)=="Total.GHG.Emissions"]<- "GHG"
names(EnergyDatav1)[names(EnergyDatav1)=="Primary.Property.Type"]<- "PropertyType"
#checking for missing data
colSums(is.na(EnergyDatav1)) # number of missing per column/variables
str(EnergyDatav1)
class(EnergyDatav1$Borough)
apply(EnergyDatav1, 2, function(x) sum(is.na(x)))
#change variable borough to a factor (categorical variable)
#first change all levels to numbers such that manhattan=1, bk=2, bx=4, sa=5, qn=3
EnergyDatav1$Borough[EnergyDatav1$Borough=="manhattan"]="1"
EnergyDatav1$Borough[EnergyDatav1$Borough=="brooklyn"]="2"
EnergyDatav1$Borough[EnergyDatav1$Borough=="queens"]="3"
EnergyDatav1$Borough[EnergyDatav1$Borough=="bronx"]="4"
EnergyDatav1$Borough[EnergyDatav1$Borough=="staten island"]="5"
#now use factor() for nominal data
EnergyDatav1$Borough=factor(EnergyDatav1$Borough, levels= c(1,2,3,4,5), labels = c("manhattan", "brooklyn", "queens", "bronx", "staten island"))
str(EnergyDatav1$Borough)

#change value "2012 to NA  --> DID NOT WORK
EnergyDatav1[EnergyDatav1$Primary.Property.Type=="2012"]<-NA
str(EnergyDatav1$Primary.Property.Type)

colSums(is.na(EnergyDatav2)) # number of missing per column/variables

#change total ghg emissions, energy star score, property type and proerty floor area to a numeric data type
EnergyDatav2<- transform(EnergyDatav1, GHG = as.numeric(GHG), ENERGY.STAR.Score = as.numeric(ENERGY.STAR.Score), Property.Floor.Area = as.numeric(Property.Floor.Area), BBL = as.numeric(BBL) )
str(EnergyDatav1)
head(EnergyDatav1)
#change Year from a numeric variable to a factor variable
EnergyDatav2$Year <- factor(EnergyDatav2$Year)
#change Primary Property Type from numeric to factor # might be better to leav as numeric
#EnergyDatav1$Primary.Property.Type <- factor(EnergyDatav1$Primary.Property.Type)
str(EnergyDatav1$Primary.Property.Type)
#FIND VALUE 2012 in the variables 
which(grepl("2012", EnergyDatav1$Primary.Property.Type))
#[1]  4654  9491  9523 10989  there are four rows with this as property type
EnergyDatav1$Zip.code <- factor(EnergyDatav1$Zip.code)

print(idx <- sapply(EnergyDatav1, is.numeric))

str(InputMeans)
str(EnergyDatav2)
table(EnergyDatav2$PropertyType)
colSums(is.na(EnergyDatav2)) # number of missing per column/variables
names(EnergyDatav2)
GHG <- EnergyDatav2[c(7)] # test if for loop works- it does
InputMeans <- EnergyDatav2[c(3,7,5,6,8)] # to create dataset with only continuous variables
#remove rows with values of 0
names(EnergyDatav2)
EnergyDatav3= EnergyDatav2[!(EnergyDatav2$Property.Floor.Area == "0"), ]
EnergyDatav3= EnergyDatav2[!(EnergyDatav2$ENERGY.STAR.Score == "0"), ]
EnergyDatav3= EnergyDatav2[!(EnergyDatav2$Site.EUI == "0"), ]
EnergyDatav3= EnergyDatav2[!(EnergyDatav2$weather == "0"), ]
EnergyDatav3= EnergyDatav2[!(EnergyDatav2$GHG == "0"), ]

colSums(is.na(EnergyDatav3)) # number of missing per column/variables


#treat missing values with the mean of the column variable!!!!!
for(i in ncol(EnergyDatav3)){
  EnergyDatav3[is.na(EnergyDatav3[,i]), i] <- mean(EnergyDatav3[,i], na.rm = TRUE)
}
colSums(is.na(InputMeans)) # number of missing per column/variables
i
library(DMwr)


#Descriptives summaries
install.packages("pastecs")
library(pastecs)
stat.desc(EnergyDatav1)

summary(EnergyDatav3$Site.EUI, na.rm=T)# basic statistics of continuous variables
summary(GHG)
sd(EnergyDatav3$Site.EUI, na.rm=TRUE)
plot(InputMeans$GHG)
plot(GHG)
fivenum(EnergyDatav1, na.rm=T, sd) #same as above
mean(EnergyDatav1$Total.GHG.Emissions, na.rm=T)# to exclude missing values  (NAs)
mean(EnergyDatav1$ENERGY.STAR.Score, na.rm=T)
max(table(EnergyDatav1$ENERGY.STAR.Score)) # to get mode by frequencies 
#descriptive statistics by group using --aggregate---
aggregate(EnergyDatav1[c("BBL")], EnergyDatav1["Borough"], summary, na.rm=true)
#descriptive statistics by group using --tapply---
mean <- tapply(EnergyDatav1$Total.GHG.Emissions, EnergyDatav1$Year, na.rm=true)
mean
str(EnergyDatav1$Year)

Frequency= c(14832,5769,5589,5019,579)
Borough= c("Manhattan", "Brooklyn", "Queens", "Bronx", "Staten Island")
barplot(Frequency, names.arg = c("Manhattan", "Brooklyn", "Queens", "Bronx", "Staten Island"), xlab = "Borough", ylab = "Frequency", ylim = c(0,15500), main = "Number of Benchmarking Submission by Borough", col = c("navy", "maroon", "forest green", "dark orange", "yellow"), density = NULL)
table(EnergyDatav1$Borough, EnergyDatav1$Year)
library("sqldf", lib.loc="~/R/win-library/3.2")
table1=sqldf("select Borough, avg(GHG) as AverageGHG from EnergyDatav1 group by Borough")
table(EnergyDatav1$GHG, EnergyDatav1$Borough)
ZZZ <- EnergyDatav1[c(3,5,6,7,8)]
str(ZZZ)
print(idx <- sapply(ZZZ, is.numeric))
boxplot(ZZZ)
boxplot(apply(ZZZ,2,scale))
hist(ZZZ$Total.GHG.Emissions)
#tables
table(EnergyDatav1$Borough, useNA=c("ifany") )
    #manhattan      brooklyn        queens         bronx staten island 
    #14832          5769          5589          5019           579 
    #<NA> 
    #  8 
names(EnergyDatav1)
ftable(EnergyDatav1, row.vars=2,9)
EnergyDatav1$Borough=factor(EnergyDatav1$Borough, levels = c(1:5), labels = c())
EnergyData$Index <- as.numeric(as.character(EnergyData$Index))
EnergyDatav2=na.omit(EnergyDatav1)
write.csv(EnergyDatav2, file = "G:/EnergyDatav2.csv")

EnergyDatav2=complete.cases(EnergyDatav1$Weather.Normalized.Source.EUI)
EnergyData[complete.cases(EnergyData$Index),]
EnergyDatav2= EnergyData[!is.na(EnergyDatav1$Weather.Normalized.Source.EUI),]  # to try to remove rows with null values in Index column in the dataframe
subset(DF, !is.na(y))
sum(!is.na(EnergyData$Index))
levels(EnergyData$variable) # to get all the categories of a categorical variable
summary(EnergyData) # basic statistics of continuous variables
fivenum(EnergyData) #same as above
mean(Energydata$continuousvariable, na.rm=T)# to remove missing values  (NAs)
var(Energydata$continuousvariable, na.rm=T)
sd(Energydata$continuousvariable, na.rm=T)
print(idx <- sapply(Energydata, is.numeric)) #  to check variables are continuous
energydata1 = na.omit(EnergyData) #create new dataset without missing data

str(EnergyData$Index)
#many columns are not numerical when they should be
EnergyData$Index <- as.numeric(as.character(EnergyData$Index))

# test for outliers, remove them!!!!!



#Linear Regression --Acessing relationship between outcome (Total Greenhouse Gas Emissions) and one predictor (Weather Energy Use Intensity)

EnergyDatav2=na.omit(EnergyDatav1) #omitted all missing values
EnergyDatav1a= EnergyDatav1[!is.na(EnergyDatav1$weather),]  # to try to remove rows with null values in weather and total gas only
EnergyDatav1a= EnergyDatav1[!is.na(EnergyDatav1$GHG),] 

class(EnergyDatav2$GHG)
class(EnergyDatav2$weather)
plot(EnergyDatav3$weather, EnergyDatav3$GHG, main = "ScatterPlot between GHG Emissions and Weather EUI", ylab ="Total GreenHouse Gas Emissions", xlab= "Weather Energy Use Intensity")
#there appears to be one outliers # will need to test for them and probably remove them
#Pearsons correlation
cor(EnergyDatav3$weather, EnergyDatav3$GHG, use = "complete") #to run correlation between variables excluding missing data
#0.9619737 all missing omitted
cor(EnergyDatav1a$weather, EnergyDatav1a$GHG, use = "complete")
#0.933687 just GHG and weather missing ommitted 
?lm
mod1a<-lm(EnergyDatav1a$GHG ~ EnergyDatav1a$weather) #linear model with only GHG and weather NA omiited only

mod1<-lm(EnergyDatav2$GHG ~ EnergyDatav2$weather) #linear model with all NA omitted

summary(mod1a)
summary(mod1)