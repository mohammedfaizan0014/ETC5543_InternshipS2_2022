library(sdcMicro)
#Set working Directory change for you
#setwd("C:/Users/poshaugh/OneDrive - University of Wollongong/UOW-CSS/R/disclosure-old")
source(here::here("analysis/DisclosureRisk.R"))
source(here::here("analysis/DRisk_update.R"))


##################
##all continuous
###################

#require(sdcMicro)
library(sdcMicro)
CASC_sample <- CASCrefmicrodata[,c(2,3,4,6)]
CASC_protected <- addNoise(CASC_sample,noise = 100)$xm #Additive Noise protected

DRisk_NN <- drscore(
  Sample = CASC_sample, #Original Sample
  Protected = CASC_protected,
  delta = 0.05,
  kdistinct = 0.05, #k distinct threshold if integer then
                 # probability threshold is k/SS (SS = sample size)
  ldeniable = 5, # l undeniable threshold if integer then
                         # probability threshold is l/SS (SS = sample size)
  neighbourhood = 1,
  #Possible 'neighbourhood' types
  # 1 = Mahalanobis (Based on Mahalanobis Distance)
  # 2 = DSTAR   (Based on Density Based Distance)
  # 3 = StdEuclid (Based on Standardised (by std dev) Euclidean Distance)
  # 4 = RelEuclid (Relative Euclidean Distance sum_k ((Xk-Yk)/Xk)^2)
  neigh_type = 'prob',
  #Possible 'neigh_type' types
  #constant = fixed threshold on distance
  #prob = Nearest Neighbour Probability Neighbourhood used (Worst Case Scenario 1)
  #estprob = = Nearest Neighbour Probability Neighbourhood used based on protected density (Worst Case Scenario 2)
  numeric.vars = 1:4, #Which Variables are continuous?
  outlier.par = list(centre = median,
                     scale = var,
                     thresh = 0.01)
  #Parameters to adjust how MV outliers are determined.
  #Default is that lie 99% (based on Chi-Square n-1 dist) away from median after scale by variance.
)

#Update neighbourhood to fixed threshold definition
DRisk_Fxd <- update(DRisk_NN,neigh_type = 'constant',
                          delta = 1)

########################
## mixed dataset
########################

load("data/wage4.rda"); load("data/wage4_protected.rda")
nn <- drscore(Sample = wage4, Protected = wage4_protected, numeric.vars = c("age","wage"))
nn <- drscore(Sample = wage4, Protected = wage4_protected, numeric.vars = c(1,4))
nn <- drscore(Sample = wage4, Protected = wage4_protected) #does not work without numeric.vars
xd <- update(nn,delta = 0.06)

###############################
# MBD Example
###############################
source('MBDensity3.4.R')
source('MBD.predict.R')
source('MBD.predict.R')
source('MBD.ks2.R')
source('MBD.cdf.R')
source('MBD.polynomial.R')
source('MBD.conditional.R')
source('MBD.gibbs.poly.R')

Fit <- MBDensity(CASC_sample, bounds = sapply(1:4,function(k) range(CASC_sample[,k])*c(0.95,1.05)))
plot(sapply(1:16,function(k) Fit$MPO$normval[k,k,k,k]))
Synth <- MBD.gibbs.poly(Fit, K= 8, variables = 1:4)

DRiskMBD_NN <- DisclosureRisk(Sample = CASC_sample,
                              Protected= Synth,
                              delta= 0.05,
                              kdistinct = 0.05,
                              ldeniable = 5,
                              neighbourhood = 1,
                              neigh_type = 'prob',
                              numeric.vars = 1:4)

# ##################
# ##all continuous###################
#
# library(svMisc)
# library(dress)
# library(sdcMicro)
# CASC_sample <- CASCrefmicrodata[,c(2,3,4,6)]
# CASC_protected <- addNoise(CASC_sample,noise = 100)$xm #Additive Noise protected
#
# DRisk_NN <- drscore(
#   Sample = CASC_sample, #Original Sample
#   Protected = CASC_protected,
#   delta = 0.05,
#   kdistinct = 0.05,
#   ldeniable = 5,
#   neighbourhood = 1,
#   neigh_type = 'prob',
#   numeric.vars = 1:4, #Which Variables are continuous?
#   outlier.par = list(centre = median,
#                      scale = var,
#                      thresh = 0.01)
# )
#
