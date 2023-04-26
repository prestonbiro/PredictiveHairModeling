#Upgraded Hair Data Generation
library(tidyverse)

#Read in schedule
realData = read.csv('HairResearchData.csv') %>% 
  mutate(ExpectedTreatTime = as.POSIXct(paste(Date,'7:30'),format = '%m/%d/%Y %H:%M'),
         TreatTimeStamp = as.POSIXct(paste(Date,Treatment.Time),format = '%m/%d/%Y %H:%M'),
         MorningTimeStamp = as.POSIXct(paste(Date,Morning.Picture.Time),format = '%m/%d/%Y %H:%M'),
         MiddayTimeStamp = as.POSIXct(paste(Date,Midday.Picture.Time),format = '%m/%d/%Y %H:%M'),
         EveningTimeStamp = as.POSIXct(paste(Date,Evening.Picture.Time),format = '%m/%d/%Y %H:%M'))
treatTimes = realData %>% 
  # filter(Actual.Treatment != 'NOTHING') %>% 
  filter(Treatment.Plan != 'NOTHING') %>% 
  # mutate(UsedTreatTime = as.POSIXct(ifelse(is.na(TreatTimeStamp),
  #                                          ExpectedTreatTime,TreatTimeStamp),
  #                                   origin = '1970-01-01 UTC')) %>% 
  pull(ExpectedTreatTime) 
treatName = realData %>% 
  filter(Treatment.Plan != 'NOTHING') %>% 
  pull(Treatment.Variable) 
treatOpts = unique(treatName)
treatAssign = lapply(treatName,function(x) which(treatOpts == x)) %>% unlist
sampleTimes = realData %>% 
  select(MorningTimeStamp,MiddayTimeStamp,EveningTimeStamp) %>% 
  pivot_longer(everything()) %>% 
  pull(value) %>% 
  na.omit()
expectedSampleTimes = c(
  seq.POSIXt(from = as.POSIXct('2023-02-11 09:30:00 CST'),
             to = as.POSIXct('2023-04-08 09:30:00 CDT'),by = 'day'),
  seq.POSIXt(from = as.POSIXct('2023-02-11 13:30:00 CST'),
             to = as.POSIXct('2023-04-08 13:30:00 CDT'),by = 'day'),
  seq.POSIXt(from = as.POSIXct('2023-02-11 21:30:00 CST'),
             to = as.POSIXct('2023-04-08 21:30:00 CDT'),by = 'day')) %>% 
  sort()

treatInts = c(.8,.9,.75,.6) #True Intercepts of treatments
treatDecays = c(.9,1.1,1,1.3) #First treatment should decay faster

# generateBayesHairQualityCurveTime <- function(t,z){
#   #Given the treatment intercepts and decay rates set above and that this
#   #model is how hair quality operates, this is the true curves we'd expect
#   #t - time of treatment
#   #z - assigned treatment
#   bayesTreatInts = rnorm(length(z),treatInts[z],sd = .025)
#   bayesTreatDecays = rnorm(length(z),treatDecays[z],sd = .1)
#   timePoints = seq.POSIXt(min(t),max(t) + 24*3600,by = 'hour')
#   alpha = 48 * 3600
#   N = length(timePoints)
#   qualityDF = data.frame(Time = timePoints,Quality = NA,Treatment = NA)
#   lastTreatTime = '2023-02-10 08:30:00 CST'
#   lastTreatAssign = 1
#   lastInt = bayesTreatInts[1]
#   lastDecay = bayesTreatDecays[1]
#   extendZ = rep(NA,N)
#   ix = 0
#   for(i in timePoints){
#     ix = ix + 1
#     if(i %in% t) {
#       lastTreatTime = i
#       lastTreatAssign = z[t == i]
#       lastInt = bayesTreatInts[t == i]
#       lastDecay = bayesTreatDecays[t == i]
#       qualityDF[ix,'Quality'] = lastInt
#     }
#     else{
#       qualityDF[ix,'Quality'] = lastInt * 
#         exp(-(lastTreatTime - i)^2 / (alpha * lastDecay) ^ 2)
#     }
#     qualityDF[ix,'Treatment'] = lastTreatAssign
#   }
#   return(qualityDF)
#   # return(data.frame(Time = timePoints,
#   #                   Quality = qualityAssign,
#   #                   Treatment = extendZ))
# }
# #Takes a while (~100 seconds)
# bayesCurveTime = generateBayesHairQualityCurveTime(treatTimes,treatAssign)
# sampleQualityDF = bayesCurveTime %>% filter(Time %in% sampleTimes)

qualityFunction <- function(tDiff,yInt,decayRate,alphaFactor = 48){
  return(yInt * exp(-tDiff^2/(decayRate * alphaFactor)^2))
}

generateBayesHairParameters <- function(z,trueInts,trueDecays){
  #Given a certain treatment schedule and the true parameters, generates
  #bayesian draws from the true parameters
  bayesTreatInts = rnorm(length(z),trueInts[z],sd = .025)
  bayesTreatDecays = rnorm(length(z),trueDecays[z],sd = .1)
  return(list(SampledTreatInts = bayesTreatInts,
              SampledTreatDecays = bayesTreatDecays))
}

sampleHairQuality <- function(x,t,z,bayesInt,bayesDecay){
  #Samples hair quality given parameters and known treatment times t and 
  #assignment z at sample times x
  lapply(x,function(y) 
    qualityFunction(
      as.double(difftime(y,t[max(which(t <= y))],units = 'hours')),
      bayesInt[max(which(t <= y))],bayesDecay[max(which(t <= y))])) %>% 
    unlist %>% 
    return()
}

bayesParams = generateBayesHairParameters(treatAssign,treatInts,treatDecays)
yObs = sampleHairQuality(expectedSampleTimes,treatTimes,treatAssign,
                     bayesParams$SampledTreatInts,bayesParams$SampledTreatDecays)
# plot(expectedSampleTimes,yObs)

# a = curve(sampleHairQuality(x,treatTimes,treatAssign,
#                         bayesParams$SampledTreatInts,bayesParams$SampledTreatDecays),
#           xlim = c(min(treatTimes),max(treatTimes) + 24*3600),ylim = c(0,1),n = 100000)
# points(expectedSampleTimes,yObs)

fakeDataCurve = data.frame(Time = seq.POSIXt(from = min(treatTimes),
                                             to = max(treatTimes) + 24*3600,
                                             by = '30 mins')) %>% 
  mutate(
    Quality = sampleHairQuality(Time,
                                treatTimes,treatAssign,
                                bayesParams$SampledTreatInts,
                                bayesParams$SampledTreatDecays)) %>% 
  mutate(Treatment = lapply(Time,function(x) 
    treatAssign[which(treatTimes == max(treatTimes[treatTimes <= x]))]) %>% unlist)
generatedHairSample = data.frame(Time = expectedSampleTimes,
                        Quality = yObs) %>% 
  mutate(Treatment = lapply(Time,function(x) 
    treatAssign[which(treatTimes == max(treatTimes[treatTimes <= x]))]) %>% unlist)

ggplot(fakeDataCurve,aes(x = Time,y = Quality,color = Treatment)) + 
  geom_point() + 
  geom_line() +
  coord_cartesian(ylim = c(0,1)) + 
  # geom_vline(xintercept = treatTimes,lty = 2) +
  scale_color_identity() +
  geom_point(data = generatedHairSample,size = 3) +
  geom_hline(data = data.frame(Quality = treatInts,
                               Treatment = 1:4),
             aes(yintercept = Quality,color = Treatment),lty = 2)

