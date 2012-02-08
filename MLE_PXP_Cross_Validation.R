###################################
# This file uses the NFL PxP Data #
# as an example for POLSCI 303 ####
################################

rm(list = ls())
setwd("F:/Documents/Teaching_Assistance/POLSCI_303/Lab")
#set.seed(94654321)
options(scipen = 10)

### Packages ###

install.packages("stringr")
library(stringr)
install.packages("ggplot2")
library(ggplot2)
theme_set(theme_bw(base_size = 7))
install.packages("MASS")
library(MASS)

### Functions ###

MakeTable <- function(vector, toregex){  do.call(rbind, strsplit(vector, toregex))  }
MyPalette <- colorRampPalette(c(hsv(7/12, 7/12, 7/12), hsv(0, 7/12, 7/12)))

### Load data ###

PXP <- read.csv("2011_nfl_pbp_data_reg_season.csv", as.is = T)
PXP <- PXP[nchar(PXP$off) > 0 & nchar(PXP$def) > 0, ]
head(PXP)

### Define some new variables

# A regular expression matcher to find the home team:
PXP$HomeTeam <- gsub("@", "", str_sub(PXP$gameid, start = -3, end = -1))
PXP$OffenseisHome <- (PXP$HomeTeam == PXP$off)

# A regular expression matcher to make a "Sacked" indicator variable:
PXP$Sack <- str_detect(PXP$description, "sacked")

# A more complicated regular expression matcher using the MakeTable() function defined above
# to count the number of yards gained on a play:
PXP$Gain <- as.numeric(MakeTable(MakeTable(PXP$description, " for ")[, 2], " yard")[, 1])

# Pass play or not?
PXP$Pass <- regexpr(" pass ", PXP$description) != -1
PXP$Punt <- regexpr(" punt", PXP$description) != -1

####################
# Cross-validation #
####################

head(PXP)
PXP$TeamGame <- paste(PXP$off, PXP$gameid)
NewFrame <- data.frame(Game = sort(unique(PXP$TeamGame)))
NewFrame$MeanGain <- by(PXP$Gain[PXP$Punt == 0], PXP$TeamGame[PXP$Punt == 0], mean, na.rm = T)[as.character(NewFrame$Game)]
NewFrame$PassProp <- by(PXP$Pass[PXP$Punt == 0], PXP$TeamGame[PXP$Punt == 0], mean, na.rm = T)[as.character(NewFrame$Game)]
NewFrame$KExclude <- sample(LETTERS[1:4], nrow(NewFrame), replace = T)
NewFrame <- NewFrame[NewFrame$PassProp > 0, ]

table(NewFrame$KExclude)

head(NewFrame, 10)
plot(NewFrame$PassProp, NewFrame$MeanGain)

DataCollection <- NULL
NSims <- 100
NStochSims <- 100
SimFrame <- c()

for(kk in LETTERS[1:4]){
Model <- lm(MeanGain ~ PassProp, data = NewFrame[NewFrame$KExclude != kk, ])
Intercept <- Model$coef[1]
Slope <- Model$coef[2]
SE <- summary(Model)$coef[2, 2]

OOSYhat <- predict(Model, newdata = NewFrame[NewFrame$KExclude == kk, ])
OOSY <- NewFrame[NewFrame$KExclude == kk, "MeanGain"]
plot(OOSYhat, OOSY)
RMSE <- sqrt(mean((OOSYhat - OOSY) ^ 2))

NewData <- c(kk, Intercept, Slope, SE, RMSE)
DataCollection <- rbind(DataCollection, NewData)

### Simulation

PassPropLevels <- quantile(NewFrame$PassProp, 1:3/4)
Scenarios <- cbind(1, PassPropLevels)
rownames(Scenarios) <- c("Low", "Mid", "Hi")
Coefficients <- coef(Model)
VarCovar <- vcov(Model)
MVRNormDraws <- mvrnorm(NSims, Coefficients, VarCovar)
SimYhats <- MVRNormDraws %*% t(Scenarios)


RSE <- summary(Model)$sigma
StochSimYhats <- apply(SimYhats, 2, function(cc){
 rnorm(length(cc) * NStochSims, rep(cc, NStochSims), RSE)})
head(StochSimYhats)
dim(StochSimYhats)

NewSims <- cbind(kk, melt(StochSimYhats)[, -1])

SimFrame <- rbind(SimFrame, NewSims)
}

dim(SimFrame)

DataCollection <- data.frame(DataCollection)
colnames(DataCollection) <- c("KExclude", "Intercept", "Slope", "SE", "RMSE")
DataCollection[, -1] <- apply(DataCollection[, -1], 2,
 function(x){as.numeric(as.character(x))})

SimFrame <- data.frame(SimFrame)
colnames(SimFrame) <- c("KExclude", "Scenario", "Value")

ZP1 <- ggplot(NewFrame,
 aes(x = PassProp, y = MeanGain, colour = KExclude))
ZP1 <- ZP1 + geom_point()
ZP1 <- ZP1 + scale_colour_brewer(palette = "Set1")
print(ZP1)

ZP1 <- ZP1 + geom_abline(aes(intercept = Intercept,
 slope = Slope, colour = KExclude), data = DataCollection)
print(ZP1)

head(SimFrame)
ZP2 <- ggplot(SimFrame,
 aes(x = Value, colour = Scenario))
ZP2 <- ZP2 + geom_density()
ZP2 <- ZP2 + facet_grid(KExclude ~ .)
print(ZP2)

head(SimFrame)
ZP2 <- ggplot(SimFrame,
 aes(x = Value, colour = KExclude ))
ZP2 <- ZP2 + geom_density()
ZP2 <- ZP2 + facet_grid(Scenario ~ .)
print(ZP2)

### lapply ###

KFolds <- LETTERS[1:4]

MyList <- c(7, 5, 4, 12, 9000, 2.3)
lapply(MyList, function(vv){
sqrt(vv * 10)
})

sqrt(MyList * 10)

SetofModelObjects <- lapply(LETTERS[1:4], function(kk){
  lm(MeanGain ~ PassProp, data = NewFrame[NewFrame$KExclude != kk, ])
  })

SetofModelObjects

SetofModelObjects[[4]]$coef

VarCovar <- lapply(SetofModelObjects, function(mm){vcov(mm)})

lapply(VarCovar, function(x){prod(x)})


MyFancyFunction <- function(aa, bb){
aa / bb
}

MyFancyFunction(100, 5)


