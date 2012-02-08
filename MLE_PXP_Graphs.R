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

MyPalette <- colorRampPalette(c(hsv(7/12, 7/12, 7/12), hsv(0, 7/12, 7/12)))

### Load data ###
PXP <- read.csv("2011_nfl_pbp_data_reg_season.csv", as.is = T)
PXP <- PXP[nchar(PXP$off) > 0, ]
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
# Graphs, etc.#
###############

PXP <- PXP[nchar(PXP$off) > 0 & nchar(PXP$def) > 0, ]
SummaryFrame <- data.frame(Team = sort(unique(PXP$off)))
SummaryFrame$Off <- by(PXP$Gain, PXP$off, mean, na.rm = T)[SummaryFrame$Team]
SummaryFrame$Def <- by(PXP$Gain, PXP$def, mean, na.rm = T)[SummaryFrame$Team]
LongSummary <- melt(SummaryFrame)

ZRadar <- ggplot(LongSummary,
 aes(x = as.numeric(as.factor(Team)),
 y = value, colour = variable))
ZRadar <- ZRadar + geom_path()
ZRadar <- ZRadar + coord_polar()
ZRadar <- ZRadar + scale_x_continuous(
 breaks = unique(as.numeric(as.factor(LongSummary$Team))),
 labels = unique(LongSummary$Team))
print(ZRadar)

ZTextScatter <- ggplot(SummaryFrame,
 aes(x = Off, y = Def, label = Team))
ZTextScatter <- ZTextScatter + geom_text()
print(ZTextScatter)

PXP <- PXP[PXP$gameid == "20110908_NO@GB", ]
PXP$TimeElapsed <- 60*60 - PXP$min * 60 - as.numeric(PXP$sec)

ZHistogram <- ggplot(PXP,
 aes(x = Gain))
ZHistogram <- ZHistogram +
 geom_histogram()
print(ZHistogram)

ZDotplot <- ggplot(PXP,
 aes(x = Gain, y = off))
ZDotplot <- ZDotplot +
 geom_point()
print(ZDotplot)

ZDensity <- ggplot(PXP,
 aes(x = Gain, colour = off))
ZDensity <- ZDensity +
 geom_density()
print(ZDensity)

ZDensity <- ZDensity + geom_rug()
print(ZDensity)

ZBoxplot <- ggplot(PXP,
 aes(x = off, y = Gain))
ZBoxplot <- ZBoxplot +
 geom_boxplot()
print(ZBoxplot)

ZBarplot <- ggplot(PXP,
 aes(x = off))
ZBarplot <- ZBarplot +
 geom_bar()
print(ZBarplot)

SummaryFrame <- data.frame(off = sort(unique(PXP$off)))
SummaryFrame$Mean <- by(PXP$Gain, PXP$off, mean, na.rm = T)[SummaryFrame$off]
SummaryFrame$SD <- by(PXP$Gain, PXP$off, sd, na.rm = T)[SummaryFrame$off]
SummaryFrame$SE <- SummaryFrame$SD / sqrt(by(!is.na(PXP$Gain), PXP$off, sum, na.rm = T)[SummaryFrame$off])

ZPointrange <- ggplot(SummaryFrame,
 aes(x = off, y = Mean,
 ymin = Mean - SE, ymax = Mean + SE))
ZPointrange <- ZPointrange +
 geom_pointrange()
print(ZPointrange)

PXP <- PXP[PXP$Gain < 100, ]
PXP <- PXP[!is.na(PXP$off) & !is.na(PXP$Pass), ]

ZFacet <- ggplot(PXP,
 aes(x = ydline, y = Gain))
ZFacet <- ZFacet + geom_point()
ZFacet <- ZFacet + facet_grid(
 off ~ Pass)
print(ZFacet)


head(PXP)

### Save ###
HH <- 4.5
WW <- 4.25

ggsave(plot = ZHistogram, "Plot_Examples/Histogram.pdf", h = HH, w = WW)
ggsave(plot = ZDotplot, "Plot_Examples/Dotplot.pdf", h = HH, w = WW)
ggsave(plot = ZDensity, "Plot_Examples/Density.pdf", h = HH, w = WW)
ggsave(plot = ZBoxplot, "Plot_Examples/Boxplot.pdf", h = HH, w = WW)
ggsave(plot = ZBarplot, "Plot_Examples/Barplot.pdf", h = HH, w = WW)
ggsave(plot = ZPointrange, "Plot_Examples/Pointrange.pdf", h = HH, w = WW)
ggsave(plot = ZRadar, "Plot_Examples/Radar.pdf", h = HH, w = WW)
ggsave(plot = ZTextScatter, "Plot_Examples/TextScatter.pdf", h = HH, w = WW)
ggsave(plot = ZFacet, "Plot_Examples/Facet.pdf", h = HH, w = WW)






