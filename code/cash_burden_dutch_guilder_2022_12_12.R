### Paper on cash burden: Appendix B: Comaprison with BF (1989) paper
#
# The following packages are used:
#library(formattable)# has percent function
#library(plotrix)# weighted histograms
#library(dplyr)
library(xtable)# for LaTeX tables
#library(writexl)# export to Excel 
#library(ggplot2)
#library(spatstat) # for weighted.median
#library(mfx)
#library(texreg)# exports regression result to LaTeX (just like stargazer) or HTML (can be read by Excel or Word)
#library(regclass) # for confusion_matrix
#library(nnet) # for multinomial logit
#library(AER) # for p-values of nnet multinom logit coeftest(regression)
#library(rpart)
#library(rpart.plot)
#library(partykit)# modifies rpart tree plot
#library(performanceEstimation)# for SMOTE (balancing data by generating synthetic data classification tree) => hard to balance. Not used, bad results even when balanced.
#library(ROSE)# balancing data w.r.t. minority class (similar to SMOTE) => generates negative income. Do not use!
#library(caret)
#library("randomForest")

#setwd("C:/Oz_local_workspace_2")
#dir()

### Define Dutch guilder currency
(guilder_coins.vec = c(0.05, 0.10, 0.25, 1.00, 2.50))
#
(guilder_notes.vec = c(5, 10, 25, 50, 100, 250, 1000))
#
(guilder.vec = c(guilder_coins.vec, guilder_notes.vec))
#
# Dutch guilder in 1986 US dollar
(guilder_coins_1986_usd.vec = guilder_coins.vec/2.565)
#
(guilder_notes_1986_usd.vec = guilder_notes.vec/2.565)
#
(guilder_1986_usd.vec = guilder.vec/2.565)
#
# Dutch guilder in 2019 US dollar
(guilder_coins_2019_usd.vec = guilder_coins_1986_usd.vec*2.34)
#
(guilder_notes_2019_usd.vec = guilder_notes_1986_usd.vec*2.34)
#
(guilder_2019_usd.vec = guilder_1986_usd.vec*2.34)
#
# Dutch guilder in 2022 US dollar
(guilder_coins_2022_usd.vec = guilder_coins_1986_usd.vec*2.64)
#
(guilder_notes_2022_usd.vec = guilder_notes_1986_usd.vec*2.64)
#
(guilder_2022_usd.vec = guilder_1986_usd.vec*2.64)

# making it a data frame
(guider_var.vec = c("Guilder coin denomination", "Guilder coin in 1986 USD", "Guilder coin in 2019 USD", "Guilder note denomination", "Guilder note in 1986 USD", "Guilder note in 2019 USD"))
#
length(guilder_coins.vec)
length(guilder_notes.vec)
#
(col1.vec = c(guilder_coins.vec[1], guilder_coins_1986_usd.vec[1], guilder_coins_2019_usd.vec[1], guilder_notes.vec[1], guilder_notes_1986_usd.vec[1], guilder_notes_2019_usd.vec[1]))
#
(col2.vec = c(guilder_coins.vec[2], guilder_coins_1986_usd.vec[2], guilder_coins_2019_usd.vec[2], guilder_notes.vec[2], guilder_notes_1986_usd.vec[2], guilder_notes_2019_usd.vec[2]))
#
(col3.vec = c(guilder_coins.vec[3], guilder_coins_1986_usd.vec[3], guilder_coins_2019_usd.vec[3], guilder_notes.vec[3], guilder_notes_1986_usd.vec[3], guilder_notes_2019_usd.vec[3]))
#
(col4.vec = c(guilder_coins.vec[4], guilder_coins_1986_usd.vec[4], guilder_coins_2019_usd.vec[4], guilder_notes.vec[4], guilder_notes_1986_usd.vec[4], guilder_notes_2019_usd.vec[4]))
#
(col5.vec = c(guilder_coins.vec[5], guilder_coins_1986_usd.vec[5], guilder_coins_2019_usd.vec[5], guilder_notes.vec[5], guilder_notes_1986_usd.vec[5], guilder_notes_2019_usd.vec[5]))
#
(col6.vec = c(NA, NA, NA, guilder_notes.vec[6], guilder_notes_1986_usd.vec[6], guilder_notes_2019_usd.vec[6]))
#
(col7.vec = c(NA, NA, NA, guilder_notes.vec[7], guilder_notes_1986_usd.vec[7], guilder_notes_2019_usd.vec[7]))

(guilder.df = data.frame(guider_var.vec, col1.vec, col2.vec, col3.vec, col4.vec, col5.vec, col6.vec, col7.vec))

print(xtable(guilder.df, digits = 2), include.rownames=F, include.colnames=F , hline.after=c(0,0,1,3,3,4))

