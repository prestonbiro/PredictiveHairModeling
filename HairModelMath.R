#Hair Research model checking

library(tidyverse)

treatEffects = 1
timeDelays = c(2,5,12,24)
alpha = 24
treatRate = c(.9,1,1.1,1.05)
lambdaTreat = treatEffects * exp(-(timeDelays^2)/(treatRate * alpha^2))
lambdaTreat
curve(exp(-x^2/(24^2)),xlim = c(0,72))

lambdaTreat = rexp(length(timeDelays),timeDelays)
lambdaTreat
curve(dexp(x,rate = timeDelays[4]/24),xlim = c(0,10))


#Let's try something
alpha = 48
treatInts = c(8,9,7,6)
treatDelayRates = c(.9,1,1.2,.7)

treatCurve <- function(timeDelay,treatNum){
  return(treatInts[treatNum] * exp(-timeDelay^2 / 
                                     ((treatDelayRates[treatNum] * alpha)^2)))
}

curve(treatCurve(x,1),xlim = c(0,72),ylim = c(0,10))
curve(treatCurve(x,2),xlim = c(0,72),col = rgb(1,0,0),add = T)
curve(treatCurve(x,3),xlim = c(0,72),col = rgb(0,1,0),add = T)
curve(treatCurve(x,4),xlim = c(0,72),col = rgb(0,0,1),add = T)

#These curves make sense, but let's keep playing around
#Let's start thinking of the treatments as boosts, rather than intercepts
#and try to make full data generation options

#Assume hair quality between 0 and 1
baseHairQuality = .5
#Assume treatment times are listed at the following (as hours since start)
treatTimes = (c(1.25,2.25,4.25,7.25,9.25,10.25,12.25,14.25) - 1) * 24
#List which treatment used
treatAssign = c(1,1,2,2,1,1,2,2)
#Treat Spikes
treatSpikes = c(.2,.4) #Second treatment twice as good as first
treatInts = c(.8,.9) #Kinda the same, but different view
#Treat decays
treatDecays = c(.9,1.1) #First treatment should decay faster
#Now when a treatment is assigned, we will add the spike of the effect
#Or should it be more of a reset to the quality peak? I think that makes 
#more sense, but I'll check with experts
#Lets start with the peaks
generateHairQualityCurve <- function(t,z){
  #Given the treatment intercepts and decay rates set above and that this
  #model is how hair quality operates, this is the true curves we'd expect
  #t - time of treatment
  #z - assigned treatment
  timePoints = 1:(24*14)
  N = length(timePoints)
  qualityAssign = rep(NA,N)
  qualityAssign[t] = treatInts[z]
  lastTreatTime = -24
  lastTreatAssign = 1
  extendZ = rep(NA,N)
  for(i in 1:N){
    if(i %in% t) {
      lastTreatTime = i
      lastTreatAssign = z[t == i]
    }
    else{
      qualityAssign[i] = treatInts[lastTreatAssign] * 
        exp(-(lastTreatTime - i)^2 / (alpha * treatDecays[lastTreatAssign]) ^ 2)
    }
    extendZ[i] = lastTreatAssign
  }
  return(data.frame(Time = min(t):N,
                    Quality = qualityAssign[min(t):N],
                    Treatment = extendZ[min(t):N]))
  # return(data.frame(Time = timePoints,
  #                   Quality = qualityAssign,
  #                   Treatment = extendZ))
}
exCurve = generateHairQualityCurve(treatTimes,treatAssign)
ggplot(exCurve,aes(x = Time,y = Quality,color = Treatment)) + 
  geom_point() + 
  geom_line() +
  geom_vline(xintercept = treatTimes) + 
  scale_color_identity()

#Great, this makes sense
#Now lets make it Bayesian
generateBayesHairQualityCurve <- function(t,z){
  #Given the treatment intercepts and decay rates set above and that this
  #model is how hair quality operates, this is the true curves we'd expect
  #t - time of treatment
  #z - assigned treatment
  bayesTreatInts = rnorm(length(z),treatInts[z],sd = .025)
  bayesTreatDecays = rnorm(length(z),treatDecays[z],sd = .1)
  timePoints = 1:(24*14)
  N = length(timePoints)
  qualityAssign = rep(NA,N)
  qualityAssign[t] = bayesTreatInts
  lastTreatTime = -24
  lastTreatAssign = 1
  lastInt = bayesTreatInts[1]
  lastDecay = bayesTreatDecays[1]
  extendZ = rep(NA,N)
  for(i in 1:N){
    if(i %in% t) {
      lastTreatTime = i
      lastTreatAssign = z[t == i]
      lastInt = bayesTreatInts[t == i]
      lastDecay = bayesTreatDecays[t == i]
    }
    else{
      qualityAssign[i] = lastInt * 
        exp(-(lastTreatTime - i)^2 / (alpha * lastDecay) ^ 2)
    }
    extendZ[i] = lastTreatAssign
  }
  return(data.frame(Time = min(t):N,
                    Quality = qualityAssign[min(t):N],
                    Treatment = extendZ[min(t):N]))
  # return(data.frame(Time = timePoints,
  #                   Quality = qualityAssign,
  #                   Treatment = extendZ))
}
bayesCurve = generateBayesHairQualityCurve(treatTimes,treatAssign)
ggplot(bayesCurve,aes(x = Time,y = Quality,color = Treatment)) + 
  geom_point() + 
  geom_line() +
  geom_vline(xintercept = treatTimes) + 
  scale_color_identity()

#Okay so we got a bayesian version and it looks pretty good, now lets 
#try to see if we can do any inference

sampleTimes = sort(c(9 + 1:14 * 24,12 + 1:14 * 24,21 + 1:14 * 24)) - 24
ggplot(bayesCurve,aes(x = Time,y = Quality,color = Treatment)) + 
  geom_point() + 
  geom_line() +
  geom_vline(xintercept = treatTimes) + 
  scale_color_identity() + 
  geom_vline(xintercept = sampleTimes,lty = 2,alpha = .5)

sampleQualityDF = bayesCurve %>% filter(Time %in% sampleTimes)
ggplot(sampleQualityDF,aes(x = Time,y = Quality,color = Treatment)) + 
  geom_point(size = 2) + 
  geom_vline(xintercept = treatTimes) + 
  scale_color_identity()  

ggplot(bayesCurve,aes(x = Time,y = Quality,color = Treatment)) + 
  geom_point() + 
  geom_line() +
  geom_vline(xintercept = treatTimes) + 
  scale_color_identity() + 
  geom_point(data = sampleQualityDF,size = 3)

#Inference straight from this

sampleQualityDF
