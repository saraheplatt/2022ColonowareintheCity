# load the libraries
library(RPostgreSQL)
library(dplyr)
library(tidyr)
library(reshape2)
library (ca)
library (plotrix)
library(ggplot2)
library(viridis)

#### Read in CSV of phased ware data

wareTypeData<- read.csv(file = '87Church_PhasedData_PlattDiss.csv', fileEncoding = 'UTF-8-BOM', stringsAsFactors = FALSE)

#### Filter out variables we won't be using
wareTypeDataStream <- wareTypeData %>%
  select(WARE, CEW, Phase, COUNT)

#### Filter out just colonoware data

ColonowareA <- wareTypeDataStream %>% filter(WARE == 'Colonoware')
ColonowareB <- wareTypeDataStream %>% filter(WARE == 'Native American')

Colonoware <- rbind(ColonowareA, ColonowareB)

#### Group colonoware by phase
PhasedColonoware  <-Colonoware  %>% 
  group_by(WARE, CEW, Phase) %>%
  summarise(Count = sum(COUNT))

#### Rename and compile NA ceramics

PhasedColonoware$CEW <- with(PhasedColonoware, ifelse(WARE=='Native American', 'Native American',
                                                      CEW))

#### Transpose data
PHASEDCOLONOFINAL <- PhasedColonoware %>% group_by(CEW, Phase) %>%
  summarise(Count=sum(Count)) %>%
  spread(CEW,
         value=Count ,
         fill=0 )

#### Sum columns
PhasedColonoRelFreq <- PHASEDCOLONOFINAL %>% mutate(sumrow= `Lesesne` + 
                                                      `Native American` +
                                                      `Not Applicable` +
                                                      `River Burnished` +
                                                      `Stobo` +
                                                      `Yaughan`)

#### Calculate Relative Frequencies

PhasedColonoRelFreqB <- PhasedColonoRelFreq %>% mutate(LesesneRF=`Lesesne`/sumrow,
                                            NativeAmericanRF=`Native American`/sumrow,
                                            NotApplicableRF=`Not Applicable`/sumrow,
                                            RiverBurnishedRF=`River Burnished`/sumrow,
                                            StoboRF=`Stobo`/sumrow,
                                            YaughanRF=`Yaughan`/sumrow)

