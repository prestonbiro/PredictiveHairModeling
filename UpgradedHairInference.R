#Upgraded Inference

#Sample some generated hair data
source('UpgradedHairDataGeneration.R')
#What we want to figure out:
print(treatInts)
print(treatDecays)
#The data we will use to learn it
print(generatedHairSample)
print(treatAssign)
print(treatTimes)

#Hair Model Inference Practice

library(tidyverse)
library(truncnorm)

metHastInt <- function(curZ,muZ,sigZ,yObs_i,timeAdj_i,tauSD){
  #Metropolis Hastings Sampler for the main treatment intercept Z_j
  
  #Propose new z from truncNorm(curZ,.01^2)
  propSD = .01
  propZ = rtruncnorm(1,mean = curZ,sd = propSD,a = 0,b = 1)
  
  #Likelihood portion
  likePart = sum(log(dtruncnorm(yObs_i,mean = propZ * timeAdj_i,sd = tauSD,a = 0,b = 1)) -
                       log(dtruncnorm(yObs_i,mean = curZ * timeAdj_i,sd = tauSD,a = 0,b = 1)))
  priorPart = dtruncnorm(propZ,mean = muZ,sd = sigZ,a = 0,b = 1)/
    dtruncnorm(curZ,mean = muZ,sd = sigZ,a = 0,b = 1)
  hastingsPart = dtruncnorm(curZ,mean = propZ,sd = propSD,a = 0,b = 1)/
    dtruncnorm(propZ,mean = curZ,sd = propSD,a = 0,b = 1)
  
  acceptRatio = exp(likePart) * priorPart * hastingsPart
  if(acceptRatio > 1) {
    return(propZ)
  }
  else {
    if(runif(1) < acceptRatio){
      return(propZ)
    }
    else{
      return(curZ)
    }
  }
}

metHastGamma <- function(curGamma,gammaA,gammaB,curZ,yObs_i,
                         timeDiffNegSq_i,alpha,tauSD){
  #Metropolis Hastings Sampler for the decay of treatment gamma_j
  
  #Propose new gamma from truncNorm(curZ,.01^2)
  propSD = .05
  propGamma = rtruncnorm(1,mean = curGamma,sd = propSD,a = 0,b = 5)
  
  #Likelihood portion
  likePart = sum(log(dtruncnorm(yObs_i,mean = curZ * exp(timeDiffNegSq_i/(alpha * propGamma)^2),sd = tauSD,a = 0,b = 1)) -
                   log(dtruncnorm(yObs_i,mean = curZ * exp(timeDiffNegSq_i/(alpha * curGamma)^2),sd = tauSD,a = 0,b = 1)))
  priorPart = dgamma(propGamma,gammaA,gammaB,log = T) - dgamma(curGamma,gammaA,gammaB,log = T)
  hastingsPart = dtruncnorm(curGamma,mean = propGamma,sd = propSD,a = 0,b = 5)/
    dtruncnorm(propGamma,mean = curGamma,sd = propSD,a = 0,b = 5)
  
  acceptRatio = exp(likePart + priorPart) * hastingsPart
  if(acceptRatio > 1) {
    return(propGamma)
  }
  else {
    if(runif(1) < acceptRatio){
      return(propGamma)
    }
    else{
      return(curGamma)
    }
  }
}

metHastTau <- function(curTau,tauScale,yObs_i,predYMean_i){
  #Metropolis Hastings Sampler for the decay of treatment gamma_j
  
  #Propose new gamma from cauchy+(curZ,.1^2)
  propSD = .05
  propTau = rtruncnorm(1,mean = curTau,sd = propSD,a = 0,b = 1)
  
  #Likelihood portion
  likePart = sum(log(dtruncnorm(yObs_i,mean = predYMean_i,sd = propTau,a = 0,b = 1)) -
                   log(dtruncnorm(yObs_i,mean = predYMean_i,sd = curTau,a = 0,b = 1)))
  priorPart = dcauchy(propTau,0,scale = tauScale,log = T) -
    dcauchy(curTau,0,scale = tauScale,log = T)
  hastingsPart = dtruncnorm(curTau,mean = propTau,sd = propSD,a = 0,b = 1)/
    dtruncnorm(propTau,mean = curTau,sd = propSD,a = 0,b = 1)
  
  acceptRatio = exp(likePart + priorPart) * hastingsPart
  if(acceptRatio > 1) {
    return(propTau)
  }
  else {
    if(runif(1) < acceptRatio){
      return(propTau)
    }
    else{
      return(curTau)
    }
  }
}

#' Gibbs Sampler Hair Model - Basic
#' 
#' @desc Gibbs sampler for hair model, trying to perform inference to determine
#' intercept and decay effects of hair quality. This assumes a very basic 
#' structure, with the only controlling factor being the treatment.
#' 
#' @param yHair A n_o-vector of hair quality measurements
#' @param tSample A n_o-vector of time points where hair quality was measured
#' @param tTreat A n_z-vector of time points where hair was treated
#' @param zTreat A n_z vector denoting what treatment was assigned on each day, 
#' with treatments denoted as 1,...,J
#' 
#' @returns A list containing the treatment intercepts and treatment decay rates
#' for each treatment
#' 
#' @details Treats the hair quality measurements as the output variable, giving
#' them a normal likelihood with a mean centered at the treatment intercept 
#' (peak) scaled by a time adjustment. The time adjustment is a squared 
#' exponential kernel with a scaling parameter of alpha = 48 multiplied by a 
#' corresponding decay rate for the specific treatment. The likelihood is given
#' an unknown variance parameter we will call tau^2. The prior for the treatment
#' intercept is a truncated normal with mean .8 and variance .05^2, truncated at
#' 0 and 1 to force quality parameters to stay within the quality measurement 
#' scale. The treatment decay rate parameters are given a gamma prior with shape
#' and rate parameters of 5 and 5, attempting to give a relatively informative
#' prior centered around 1, but allowing for reasonable deviation as determined
#' by the data. The measurement variance parameter tau^2 is given a horshoe 
#' prior with scale 1/10 to force the measurement variation to be assumed small.
GibbsHairModelUpgraded <- function(yHair,tSample,tTreat,zTreat,
                                Burn = 10000,Store = 10000,StoreRatio = 5){
  #Set constants
  n_o = length(yHair)
  n_z = length(zTreat)
  J = max(zTreat)
  
  #Set hyperparameters
  alpha = 48
  muZ = .8
  sigZ = .5
  gammaA = 10
  gammaB = 10
  tauScale = 1/10
  
  #Initialize parameters
  intTreat = rep(.9,J)
  # intTreat = treatInts
  gammaTreat = rep(1,J)
  # gammaTreat = treatDecays
  tauSD = .1
  
  #Structure things in a helpful way
  timeSinceTreatment = lapply(tSample,function(x) difftime(x,max(tTreat[tTreat < x]),units = 'hours')) %>% unlist
  timeSinceNegSquared = -(timeSinceTreatment^2)
  # print(timeSinceNegSquared)
  lastTreatment = lapply(tSample,function(x) zTreat[which(tTreat == max(tTreat[tTreat < x]))]) %>% unlist
  
  #Burn cycle
  print('Burn Cycle')
  for(i in 1:Burn){
    if(i %% 1000 == 0) print(i)
    #Initial latent assignments
    timeAdjustment = exp(timeSinceNegSquared/(gammaTreat[lastTreatment] * alpha)^2)
    m_i = intTreat[lastTreatment] * timeAdjustment
    
    for(j in 1:J){
      #If we wanted to speed this up, we could make individual lastTreatIx vars
      #for each j, and then we don't have to look it up each time
      lastTreatIx = lastTreatment == j
      
      #Sample intercepts
      intTreat[j] = metHastInt(intTreat[j],muZ,sigZ,yHair[lastTreatIx],
                               timeAdjustment[lastTreatIx],tauSD)
      #Sample decay rates
      gammaTreat[j] = metHastGamma(gammaTreat[j],gammaA,gammaB,intTreat[j],yHair[lastTreatIx],
                                   timeSinceNegSquared[lastTreatIx],alpha,tauSD)
    }
    #Sample measurement variance
    tauSD = metHastTau(tauSD,tauScale,yHair,m_i)
  }
  
  #Setup Storage
  intStore = matrix(NA,nrow = Store,ncol = J)
  decayStore = matrix(NA,nrow = Store,ncol = J)
  tauStore = rep(NA,Store)
  storeCycle = Store * StoreRatio
  ix = 0
  
  #Store cycle
  print('Store Cycle')
  for(i in 1:storeCycle){
    if(i %% 1000 == 0) print(i)
    #Initial latent assignments
    timeAdjustment = exp(timeSinceNegSquared/(gammaTreat[lastTreatment] * alpha)^2)
    m_i = intTreat[lastTreatment] * timeAdjustment
    
    #Sample intercepts
    for(j in 1:J){
      lastTreatIx = lastTreatment == j
      intTreat[j] = metHastInt(intTreat[j],muZ,sigZ,yHair[lastTreatIx],
                               timeAdjustment[lastTreatIx],tauSD)
      gammaTreat[j] = metHastGamma(gammaTreat[j],gammaA,gammaB,intTreat[j],yHair[lastTreatIx],
                                   timeSinceNegSquared[lastTreatIx],alpha,tauSD)
    }
    tauSD = metHastTau(tauSD,tauScale,yHair,m_i)
    
    if(i %% StoreRatio == 0){
      ix = ix + 1
      intStore[ix,] = intTreat
      decayStore[ix,] = gammaTreat
      tauStore[ix] = tauSD
    }
  }
  # m <<- m_i
  return(list(TreatmentIntercepts = intStore,
              TreatmentDecays = decayStore,
              QualityVariance = tauStore))
}

inferParameters = GibbsHairModelUpgraded(yHair = generatedHairSample$Quality,
                       tSample = generatedHairSample$Time,
                       tTreat = treatTimes,
                       zTreat = treatAssign)
hist(inferParameters$TreatmentIntercepts[,1],breaks = 100)
abline(v = treatInts[1],col = 'red')
hist(inferParameters$TreatmentIntercepts[,2],breaks = 100)
abline(v = treatInts[2],col = 'red')
hist(inferParameters$TreatmentIntercepts[,3],breaks = 100)
abline(v = treatInts[3],col = 'red')
hist(inferParameters$TreatmentIntercepts[,4],breaks = 100)
abline(v = treatInts[4],col = 'red')
hist(inferParameters$TreatmentDecays[,1],breaks = 100)
abline(v = treatDecays[1],col = 'red')
hist(inferParameters$TreatmentDecays[,2],breaks = 100)
abline(v = treatDecays[2],col = 'red')
hist(inferParameters$TreatmentDecays[,3],breaks = 100)
abline(v = treatDecays[3],col = 'red')
hist(inferParameters$TreatmentDecays[,4],breaks = 100)
abline(v = treatDecays[4],col = 'red')
hist(inferParameters$QualityVariance,breaks = 100)
plot(inferParameters$TreatmentIntercepts[,1])
plot(inferParameters$TreatmentIntercepts[,2])
plot(inferParameters$TreatmentIntercepts[,3])
plot(inferParameters$TreatmentIntercepts[,4])
plot(inferParameters$TreatmentDecays[,1])
plot(inferParameters$TreatmentDecays[,2])
plot(inferParameters$TreatmentDecays[,3])
plot(inferParameters$TreatmentDecays[,4])
plot(inferParameters$QualityVariance)

#Right now the order of the parameters are pretty far off, and although they're
#similar in order, there seem to be some identifiability issues 
predInts <- colMeans(inferParameters$TreatmentIntercepts)
predInts
treatInts
predDecays <- colMeans(inferParameters$TreatmentDecays)
predDecays
treatDecays

extPredInts <- predInts[treatAssign]
extPredDecays <- predDecays[treatAssign]

predictedDataCurve <- 
  data.frame(Time = seq.POSIXt(from = min(treatTimes),
                               to = max(treatTimes) + 24*3600,
                               by = '30 mins')) %>% 
  mutate(
    Quality = sampleHairQuality(Time,
                                treatTimes,treatAssign,
                                extPredInts,
                                extPredDecays)) %>% 
  mutate(Treatment = lapply(Time,function(x) 
    treatAssign[which(treatTimes == max(treatTimes[treatTimes <= x]))]) %>% unlist)


ggplot(predictedDataCurve,aes(x = Time,y = Quality,color = Treatment)) + 
  # geom_point() + 
  geom_line() +
  coord_cartesian(ylim = c(0,1)) + 
  # geom_vline(xintercept = treatTimes,lty = 2) +
  scale_color_identity() +
  geom_point(data = generatedHairSample,size = 3) + 
  geom_hline(data = data.frame(Quality = treatInts,
                               Treatment = 1:4),
             aes(yintercept = Quality,color = Treatment),lty = 2)

