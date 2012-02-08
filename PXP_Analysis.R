###################################
# This file uses the NFL PxP Data #
# as an example for POLSCI 303 ####
################################

rm(list = ls())
setwd("F:/Documents/Teaching_Assistance/POLSCI_303/Lab")
set.seed(1337)
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

SmoothCoefficientPlot <- function(models, modelnames = "", removeintercept = FALSE){
  # models must be a list()

  Alphas <- seq(1, 99, 2) / 100

  Multiplier <- qnorm(1 - Alphas / 2)
  zzTransparency <<- 1/(length(Multiplier)/4)
  CoefficientTables <- lapply(models, function(x){summary(x)$coef})
  TableRows <- unlist(lapply(CoefficientTables, nrow))

  if(modelnames[1] == ""){
    ModelNameLabels <- rep(paste("Model", 1:length(TableRows)), TableRows)
    } else {
    ModelNameLabels <- rep(modelnames, TableRows)
    }

  MatrixofModels <- cbind(do.call(rbind, CoefficientTables), ModelNameLabels)
  if(removeintercept == TRUE){
    MatrixofModels <- MatrixofModels[!rownames(MatrixofModels) == "(Intercept)", ]
    }
  MatrixofModels <- data.frame(cbind(rownames(MatrixofModels), MatrixofModels))

  MatrixofModels <- data.frame(cbind(MatrixofModels, rep(Multiplier, each = nrow(MatrixofModels))))

  colnames(MatrixofModels) <- c("IV", "Estimate", "StandardError", "TValue", "PValue", "ModelName", "Scalar")
  MatrixofModels$IV <- factor(MatrixofModels$IV, levels = MatrixofModels$IV)
  MatrixofModels[, -c(1, 6)] <- apply(MatrixofModels[, -c(1, 6)], 2, function(x){as.numeric(as.character(x))})
  MatrixofModels$Emphasis <- by(1 - seq(0, 1, length = length(Multiplier) + 1)[-1], as.character(round(Multiplier, 5)), mean)[as.character(round(MatrixofModels$Scalar, 5))]

  OutputPlot <- qplot(data = MatrixofModels, x = IV, y = Estimate,
   ymin = Estimate - Scalar * StandardError, ymax = Estimate + Scalar * StandardError,
   ylab = NULL, xlab = NULL, alpha = I(zzTransparency), colour = I(gray(0)), geom = "blank")
  OutputPlot <- OutputPlot + geom_hline(yintercept = 0, lwd = I(7/12), colour = I(hsv(0/12, 7/12, 7/12)), alpha = I(5/12))
  OutputPlot <- OutputPlot + geom_linerange(data = MatrixofModels, aes(size = 1/Emphasis), alpha = I(zzTransparency), colour = I(gray(0)))
  OutputPlot <- OutputPlot + scale_size_continuous(legend = FALSE)
  OutputPlot <- OutputPlot + facet_grid(~ ModelName) + coord_flip() + geom_point(aes(x = IV, y = Estimate), colour = I(gray(0))) + theme_bw()
  return(OutputPlot)
  }

MyPalette <- colorRampPalette(c(hsv(7/12, 7/12, 7/12), hsv(0, 7/12, 7/12)))

### Load data ###
PXP <- read.csv("2011_nfl_pbp_data_reg_season.csv", as.is = T)
PXP <- PXP[-nrow(PXP), ]

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

###############
# Models, etc.#
###############

head(PXP)

ExpectedGainModel <- lm(Gain ~ ydline + factor(down) + togo + scorediff + factor(qtr) + OffenseisHome + Pass, data = PXP[PXP$Punt == 0, ])
summary(ExpectedGainModel)

SmoothCoefficientPlot(list(ExpectedGainModel))















###########################
# Scenarios / simulations #
###########################

Coefficients <- coef(ExpectedGainModel)
VarCovar <- vcov(ExpectedGainModel)

# Scenario one: End of game, down 6, pass play
Scenario1 <- c(1, 50, 0, 0, 1, 10, -6, 0, 0, 1, 0, 1, 1)

# Scenario two: End of game, down 6, run play
Scenario2 <- c(1, 50, 0, 0, 1, 10, -6, 0, 0, 1, 0, 1, 0)

names(Scenario1) <- names(Scenario2) <- colnames(VarCovar)

NSystematicSimulations <- 100

MVRNormDraws <- mvrnorm(NSystematicSimulations, Coefficients, VarCovar)
head(MVRNormDraws)

Simulation1Yhat <- MVRNormDraws %*% Scenario1
Simulation2Yhat <- MVRNormDraws %*% Scenario2

### Plot ###

SimulationFrame <- data.frame(Yhat = c(Simulation1Yhat, Simulation2Yhat))
SimulationFrame$SimulationName <- rep(c("Pass", "Run"), each = NSystematicSimulations)
SimulationFrame$ErrorType <- "Systematic"
head(SimulationFrame)

ZP1 <- ggplot(data = SimulationFrame,
 aes(x = Yhat, fill = SimulationName), alpha = I(1/2))
ZP1 <- ZP1 + geom_density()
ZP1 <- ZP1 + scale_colour_manual(values = MyPalette(2))
ZP1 <- ZP1 + scale_y_continuous(expand = c(0, 0))
print(ZP1)

ggsave(plot = ZP1, "Simulation_Systematic_Error.pdf", h = 4.5, w = 8.5)

### Adding Stochastic error ### 

sqrt(sum(ExpectedGainModel$resid ^ 2) / (length(ExpectedGainModel$resid) - 2))
sd(ExpectedGainModel$resid)
summary(ExpectedGainModel)$sigma
RSE <- summary(ExpectedGainModel)$sigma

NStochasticSimulations <- 100
StochasticSimulations <- data.frame(Yhat = rep(SimulationFrame$Yhat,
 each = NStochasticSimulations))
StochasticSimulations$Yhat <- rnorm(length(StochasticSimulations$Yhat),
 StochasticSimulations$Yhat, RSE)
StochasticSimulations$SimulationName <- rep(c("Pass", "Run"), each = NStochasticSimulations * NSystematicSimulations)
StochasticSimulations$ErrorType <- "Stochastic"

SimulationFrame <- data.frame(rbind(SimulationFrame, StochasticSimulations))
head(SimulationFrame)

### Plot ###

ZP2 <- ggplot(data = SimulationFrame,
 aes(x = Yhat, colour = SimulationName, lty = ErrorType),
 alpha = I(1/2))
ZP2 <- ZP2 + geom_density()
ZP2 <- ZP2 + scale_colour_manual(values = MyPalette(2))
ZP2 <- ZP2 + scale_y_continuous(expand = c(0, 0))
print(ZP2)

ggsave(plot = ZP2, "Simulation_Stochastic_Error.pdf", h = 4.5, w = 8.5)

ZP2 <- ZP2 + facet_grid(ErrorType ~ .)
print(ZP2)










##############
# Punt model #
##############

PuntModel <- lm(Punt ~ togo + ydline + scorediff + factor(qtr), data = PXP[PXP$down == 4, ])
summary(PuntModel)
SmoothCoefficientPlot(list(PuntModel), removeintercept = TRUE)

Coefficients <- coef(PuntModel)
VarCovar <- vcov(PuntModel)
RSE <- sd(PuntModel$residuals)

head(model.matrix(PuntModel))

# Scenario one: First quarter, down by 7
Scenario1 <- c(1, 2, 50, -7, 0, 0, 0, 0)

# Scenario one: Fourth quarter, down by 7
Scenario2 <- c(1, 2, 50, -7, 0, 0, 1, 0)

names(Scenario1) <- names(Scenario2) <- colnames(VarCovar)

NSystematicSimulations <- 100

MVRNormDraws <- mvrnorm(NSystematicSimulations, Coefficients, VarCovar)
head(MVRNormDraws)

Simulation1Yhat <- MVRNormDraws %*% Scenario1
Simulation2Yhat <- MVRNormDraws %*% Scenario2

SimulationFrame <- data.frame(Yhat = c(Simulation1Yhat, Simulation2Yhat))
SimulationFrame$SimulationName <- rep(c("First Q", "Fourth Q"), each = NSystematicSimulations)
SimulationFrame$ErrorType <- "Systematic"

# Adding Stochastic error #

NStochasticSimulations <- 100
StochasticSimulations <- data.frame(Yhat = rep(SimulationFrame$Yhat,
 each = NStochasticSimulations))
StochasticSimulations$Yhat <- rnorm(length(StochasticSimulations$Yhat),
 StochasticSimulations$Yhat, RSE)
StochasticSimulations$SimulationName <- rep(c("First Q", "Fourth Q"), each = NStochasticSimulations * NSystematicSimulations)
StochasticSimulations$ErrorType <- "Stochastic"

SimulationFrame <- data.frame(rbind(SimulationFrame, StochasticSimulations))
head(SimulationFrame)

### Plot ###

ZP3 <- ggplot(data = SimulationFrame,
 aes(x = Yhat, colour = SimulationName, lty = ErrorType),
 alpha = I(1/2))
ZP3 <- ZP3 + geom_density()
ZP3 <- ZP3 + scale_colour_manual(values = MyPalette(2))
ZP3 <- ZP3 + scale_y_continuous(expand = c(0, 0))
print(ZP3)
