
# load the libraries
library(RPostgreSQL)
library(dplyr)
library(tidyr)
library(reshape2)
library (ca)
library (plotrix)
library(ggplot2)
library(viridis)

#### Read in CSV of ware data

wareTypeData<- read.csv(file = '87Church_PastPerfectRecords_CleanedFinal.csv', fileEncoding = 'UTF-8-BOM', stringsAsFactors = FALSE)

#### Eliminate variables we will not be using

wareTypeDataStream <- wareTypeData %>%
  select(COUNT, SITE.COMPONENT, UNIT, LEVEL, FEATURE, DAACS.CONTEXT, DAACS.WARE, DAACS.CEW)

#Replace blanks in CEW with "Not Applicable" - note to SEP streamline and clean later!

#wareTypeDataStream[wareTypeDataStream == ""] <- NA 

#### Filter out just Colonoware data

ColonowareA <- wareTypeDataStream %>% filter(DAACS.WARE == 'Colonoware')
ColonowareB <- wareTypeDataStream %>% filter(DAACS.WARE == 'Native American')

ColonowareC <- rbind(ColonowareA, ColonowareB)

#### Remove kitchen features 7 and 7L, which we will not be using

Colonoware<-subset(ColonowareC, DAACS.CONTEXT!="F007" & DAACS.CONTEXT!="F07L")
             
#### Filter out workyard data

ColonowareGroupedWorkyard <- Colonoware %>% filter(SITE.COMPONENT == 'Workyard')

#### Create dataframe with just needed Workyard components

ColonowareLevel3 <- ColonowareGroupedWorkyard %>% filter(LEVEL == "3")
ColonowareLevel3a <- ColonowareGroupedWorkyard %>% filter(LEVEL == "3a")
ColonowareLevel3b <- ColonowareGroupedWorkyard %>% filter(LEVEL == "3b")
ColonowareLevel4 <- ColonowareGroupedWorkyard %>% filter(LEVEL == "4")
ColonowareLevel5 <- ColonowareGroupedWorkyard %>% filter(LEVEL == "5")
ColonowareLevel6 <- ColonowareGroupedWorkyard %>% filter(LEVEL == "6")
ColonowareLevel7 <- ColonowareGroupedWorkyard %>% filter(LEVEL == "7")
ColonowareLevel8 <- ColonowareGroupedWorkyard %>% filter(LEVEL == "8")

ColonowareWorkyardLevels <- rbind(ColonowareLevel3, ColonowareLevel3a, ColonowareLevel3b, ColonowareLevel4,
                                  ColonowareLevel5, ColonowareLevel6, ColonowareLevel7, ColonowareLevel8)

#Eliminate feature contexts

ColonowareWorkyardLevelsFin <- ColonowareWorkyardLevels %>% filter(FEATURE != "45")
ColonowareWorkyardLevelsFin <- ColonowareWorkyardLevelsFin %>% filter(FEATURE != "26")

#### Eliminate Unit data and group by levels

ColonoWorkyardLevelsTypeComp <- ColonowareWorkyardLevelsFin %>%
  select(COUNT, LEVEL, DAACS.WARE, DAACS.CEW)
                                                         
ColonoWorkyardLevelsTypeCompFin <-ColonoWorkyardLevelsTypeComp %>% 
  group_by(LEVEL, DAACS.WARE, DAACS.CEW) %>%
  summarise(Count = sum(COUNT))

#Change variable name level to context to compile with feature data

ColonoWorkyardLevelsTypeCompFinB <- rename(ColonoWorkyardLevelsTypeCompFin, CONTEXT = LEVEL)

####Now grab needed feature data from Workyard to add to final table

ColonowareFea65 <- ColonowareGroupedWorkyard %>% filter(FEATURE == '65')
ColonowareFea65X <- ColonowareGroupedWorkyard %>% filter(FEATURE == '65x')
ColonowareFea65D <- ColonowareGroupedWorkyard %>% filter(FEATURE == '65d')
ColonowareFea65C <- ColonowareGroupedWorkyard %>% filter(FEATURE == '65c')
ColonowareFea65B <- ColonowareGroupedWorkyard %>% filter(FEATURE == '65b')
ColonowareFea65A <- ColonowareGroupedWorkyard %>% filter(FEATURE == '65a')
ColonowareFea166 <- ColonowareGroupedWorkyard %>% filter(FEATURE == '166')
ColonowareFea166a <- ColonowareGroupedWorkyard %>% filter(FEATURE == '166a')
ColonowareFea166b <- ColonowareGroupedWorkyard %>% filter(FEATURE == '166b')

ColonowareWorkyardFea <- rbind(ColonowareFea65, ColonowareFea65X, ColonowareFea65D, ColonowareFea65C, ColonowareFea65B,
                               ColonowareFea65A, ColonowareFea166, ColonowareFea166a, ColonowareFea166b)

#### Eliminate Unit data and group by feature

ColonowareWorkyardFeaFin <- ColonowareWorkyardFea %>%
  select(COUNT, FEATURE, DAACS.WARE, DAACS.CEW)

ColonowareWorkyardCompFeaFin  <-ColonowareWorkyardFeaFin  %>% 
  group_by(FEATURE, DAACS.WARE, DAACS.CEW) %>%
  summarise(Count = sum(COUNT))

#Change variable name level to context to compile with level data

ColonowareWorkyardCompFeaFinB <- rename(ColonowareWorkyardCompFeaFin, CONTEXT = FEATURE)

#### Combine with final level data

CITYCOLONOHWFIN <- rbind(ColonowareWorkyardCompFeaFinB, ColonoWorkyardLevelsTypeCompFinB)

#### Repeat entire process with Kitchen and Privy

#Kitchen
ColonowareKitchen <- Colonoware %>% filter(SITE.COMPONENT == 'Kitchen')
ColonowareKitchenB <- ColonowareKitchen %>%
  select(COUNT, SITE.COMPONENT, DAACS.WARE, DAACS.CEW)
KITCHENFIN  <-ColonowareKitchenB  %>% 
  group_by(SITE.COMPONENT, DAACS.WARE, DAACS.CEW) %>%
  summarise(Count = sum(COUNT))

KITCHENFINB<- rename(KITCHENFIN, CONTEXT = SITE.COMPONENT)

#Privy
ColonowarePrivy <- Colonoware %>% filter(SITE.COMPONENT == 'Privy')
ColonowarePrivyB <- ColonowarePrivy %>%
  select(COUNT, SITE.COMPONENT, DAACS.WARE, DAACS.CEW)
PRIVYFIN  <-ColonowarePrivyB  %>% 
  group_by(SITE.COMPONENT, DAACS.WARE, DAACS.CEW) %>%
  summarise(Count = sum(COUNT))

PRIVYFINB<- rename(PRIVYFIN, CONTEXT = SITE.COMPONENT)

#### Compile!

CITYCOLONOHWFINZ <- rbind(KITCHENFINB, PRIVYFINB, CITYCOLONOHWFIN)

#### CSV!

#write.csv(CITYCOLONOHWFINZ,"CityColonoHWNum.csv", row.names = FALSE)

#### Eliminate Native American individual types

CITYCOLONOHWFINZ$DAACS.CEW <- with(CITYCOLONOHWFINZ, ifelse(DAACS.WARE=='Native American', 'Native American',
                                                            DAACS.CEW))

#Re-compile
CITYCOLONOHWFINZB <- CITYCOLONOHWFINZ %>%
  select(Count, CONTEXT, DAACS.CEW)
CITYCOLONOHWFINZC  <-CITYCOLONOHWFINZB  %>% 
  group_by(CONTEXT, DAACS.CEW) %>%
  summarise(Count = sum(Count))

#### Replace "Not Applicable" with Unidentified
CITYCOLONOHWFINZC$DAACS.CEW <- with(CITYCOLONOHWFINZC, ifelse(DAACS.CEW=='Not Applicable', 'Unidentified',
                                                            DAACS.CEW))


#### Compile all of Fea. 65 and Fea. 166 into a single context
CITYCOLONOHWFINZC$CONTEXT <- with(CITYCOLONOHWFINZC, ifelse(CONTEXT=='166a', '166',
                                                            CONTEXT))
CITYCOLONOHWFINZC$CONTEXT <- with(CITYCOLONOHWFINZC, ifelse(CONTEXT=='166b', '166',
                                                            CONTEXT))
CITYCOLONOHWFINZC$CONTEXT <- with(CITYCOLONOHWFINZC, ifelse(CONTEXT=='65a', '65',
                                                            CONTEXT))
CITYCOLONOHWFINZC$CONTEXT <- with(CITYCOLONOHWFINZC, ifelse(CONTEXT=='65b', '65',
                                                            CONTEXT))
CITYCOLONOHWFINZC$CONTEXT <- with(CITYCOLONOHWFINZC, ifelse(CONTEXT=='65c', '65',
                                                            CONTEXT))
CITYCOLONOHWFINZC$CONTEXT <- with(CITYCOLONOHWFINZC, ifelse(CONTEXT=='65d', '65',
                                                            CONTEXT))
CITYCOLONOHWFINZC$CONTEXT <- with(CITYCOLONOHWFINZC, ifelse(CONTEXT=='65x', '65',
                                                             CONTEXT))

#Compile 3a and 3b into 3

CITYCOLONOHWFINZC$CONTEXT <- with(CITYCOLONOHWFINZC, ifelse(CONTEXT=='3a', '3',
                                                            CONTEXT))
CITYCOLONOHWFINZC$CONTEXT <- with(CITYCOLONOHWFINZC, ifelse(CONTEXT=='3b', '3',
                                                            CONTEXT))

#Re-compile

CITYCOLONOHWFINALTABLE  <-CITYCOLONOHWFINZC  %>% 
  group_by(CONTEXT, DAACS.CEW) %>%
  summarise(Count = sum(Count))

#### Relative Frequencies Calculation

#Transpose the data set
CITYCOLONOHWFINALTABLET<- CITYCOLONOHWFINALTABLE %>% group_by(CONTEXT,DAACS.CEW) %>%
  summarise(Count=sum(Count)) %>%
  spread(DAACS.CEW,
         value=Count ,
         fill=0 )

#Rename Native American and River Burnished
CITYCOLONOHWFINALTABLET <- rename(CITYCOLONOHWFINALTABLET, NativeAmerican = 'Native American')
CITYCOLONOHWFINALTABLET <- rename(CITYCOLONOHWFINALTABLET, RiverBurnished = 'River-Burnished')

#Calculate sums of ware types
CITYCOLONOHWFINALTABLETS <- CITYCOLONOHWFINALTABLET %>% mutate(sumrow= Lesesne + NativeAmerican +
                                                                 RiverBurnished + Stobo + Unidentified +
                                                                 Yaughan)

#### Write CSV to transfer to excel for final calculations

write.csv(CITYCOLONOHWFINALTABLETS,"CityColonoHWTypeRowSums.csv", row.names = FALSE)

####Calculate relative frequencies 

RelativeFreqs <-CITYCOLONOHWFINALTABLETS %>% mutate(LesesneRF=Lesesne/sumrow,
                                                    NativeAmericanRF=NativeAmerican/sumrow,
                                                    RiverBurnishedRF=RiverBurnished/sumrow,
                                                    StoboRF=Stobo/sumrow,
                                                    UnidentifiedRF=Unidentified/sumrow,
                                                    YaughanRF=Yaughan/sumrow)
