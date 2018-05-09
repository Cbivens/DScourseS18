# RTE CEREAL DATA ANALYSIS: Market Share and Pricing Determinants
# Christin Bivens
# Spring 2018 

#-------------------------------------------------------------------
####################### TABLE OF CONTENTS ##########################
#-------------------------------------------------------------------

# 1. Packages
# 2. Data
# 3. Creating New Variables
# 4. Making Tibbles of the Parent Companies
# 5. Exploring the Data Using the 4 Biggest Parent Companies
#    5.1 Data Frames and GG Plots
#    5.2 Summary Statistics
#    5.3 Correlation Tests
#    5.4 Simple Linear Regressions
# 6. Making a Tibble of 4 Biggest Parent Companies Combined
# 7. Creating the Predictive Models

#-------------------------------------------------------------------
########################## 1. PACKAGES #############################
#-------------------------------------------------------------------

library(stringi)
library(dplyr)
library(tidyverse)
library(ggthemes)
library(knitr)
library(haven)
library(covr)
library(Rcpp)
library(forcats)
library(hms)
library(testthat)
library(tibble)
library(readr)
library(ggplot2)
library(stargazer)

#-------------------------------------------------------------------
#################### 2. LOADING/VIEWING DATA #######################
#-------------------------------------------------------------------

cereal<-read_dta("/Users/christin/Downloads/Cereal_PD_03052016.dta",
                 encoding = 'latin1')
# converting STATA file to csv
head(cereal, n=2)
# viewing header labels

head(cereal$AttrParentLevel_ID)
head(cereal$AttrParentLevel)
# viewing parent company info general mills = 1

tail(cereal$Year)
head(cereal$Year)
# spans 2001 - 2003

# Variables for price and parent company
#     AttrParentLevel_ID (1-12)
#     AttrParentLevel (company names)

# Price variables
#     AvgPriceperUnit

Cereal <-as_tibble(cereal)
# converting .csv to tibble format

Cereal[is.na(Cereal)]<-0
# making nan values equal to 0

#-------------------------------------------------------------------
################# 3. MAKING NEW VARIABLES ##########################
#-------------------------------------------------------------------

Cereal$Pricing<-(Cereal$DollarSales/Cereal$UnitSales)
# making a solo column of prices

Cereal$PricePerOZ<-(Cereal$Pricing/Cereal$char4_boxsize)
# making a column of price per ounce

#-------------------------------------------------------------------
# MAKING POPULATION COLUMN WITH PER WEEK POPULATION
#-------------------------------------------------------------------

Cereal$pop<-NA
week1<-filter(Cereal, Cereal$Week=="1")
Cereal$pop[Cereal$Week==1]<-sum(week1$UnitSales)
week2<-filter(Cereal, Cereal$Week=="2")
Cereal$pop[Cereal$Week==2]<-sum(week2$UnitSales)
week3<-filter(Cereal, Cereal$Week=="3")
Cereal$pop[Cereal$Week==3]<-sum(week3$UnitSales)
week4<-filter(Cereal, Cereal$Week=="4")
Cereal$pop[Cereal$Week==4]<-sum(week4$UnitSales)
week5<-filter(Cereal, Cereal$Week=="5")
Cereal$pop[Cereal$Week==5]<-sum(week5$UnitSales)
week6<-filter(Cereal, Cereal$Week=="6")
Cereal$pop[Cereal$Week==6]<-sum(week6$UnitSales)
week7<-filter(Cereal, Cereal$Week=="7")
Cereal$pop[Cereal$Week==7]<-sum(week7$UnitSales)
week8<-filter(Cereal, Cereal$Week=="8")
Cereal$pop[Cereal$Week==8]<-sum(week8$UnitSales)
week9<-filter(Cereal, Cereal$Week=="9")
Cereal$pop[Cereal$Week==9]<-sum(week9$UnitSales)
week10<-filter(Cereal, Cereal$Week=="10")
Cereal$pop[Cereal$Week==10]<-sum(week10$UnitSales)
week11<-filter(Cereal, Cereal$Week=="11")
Cereal$pop[Cereal$Week==11]<-sum(week11$UnitSales)
week12<-filter(Cereal, Cereal$Week=="12")
Cereal$pop[Cereal$Week==12]<-sum(week12$UnitSales)
week13<-filter(Cereal, Cereal$Week=="13")
Cereal$pop[Cereal$Week==13]<-sum(week13$UnitSales)
week14<-filter(Cereal, Cereal$Week=="14")
Cereal$pop[Cereal$Week==14]<-sum(week14$UnitSales)
week15<-filter(Cereal, Cereal$Week=="15")
Cereal$pop[Cereal$Week==15]<-sum(week15$UnitSales)
week16<-filter(Cereal, Cereal$Week=="16")
Cereal$pop[Cereal$Week==16]<-sum(week16$UnitSales)
week17<-filter(Cereal, Cereal$Week=="17")
Cereal$pop[Cereal$Week==17]<-sum(week17$UnitSales)
week18<-filter(Cereal, Cereal$Week=="18")
Cereal$pop[Cereal$Week==18]<-sum(week18$UnitSales)
week19<-filter(Cereal, Cereal$Week=="19")
Cereal$pop[Cereal$Week==19]<-sum(week19$UnitSales)
week20<-filter(Cereal, Cereal$Week=="20")
Cereal$pop[Cereal$Week==20]<-sum(week20$UnitSales)
week21<-filter(Cereal, Cereal$Week=="21")
Cereal$pop[Cereal$Week==21]<-sum(week21$UnitSales)
week22<-filter(Cereal, Cereal$Week=="22")
Cereal$pop[Cereal$Week==22]<-sum(week22$UnitSales)
week23<-filter(Cereal, Cereal$Week=="23")
Cereal$pop[Cereal$Week==23]<-sum(week23$UnitSales)
week24<-filter(Cereal, Cereal$Week=="24")
Cereal$pop[Cereal$Week==24]<-sum(week24$UnitSales)
week25<-filter(Cereal, Cereal$Week=="25")
Cereal$pop[Cereal$Week==25]<-sum(week25$UnitSales)
week26<-filter(Cereal, Cereal$Week=="26")
Cereal$pop[Cereal$Week==26]<-sum(week26$UnitSales)
week27<-filter(Cereal, Cereal$Week=="27")
Cereal$pop[Cereal$Week==27]<-sum(week27$UnitSales)
week28<-filter(Cereal, Cereal$Week=="28")
Cereal$pop[Cereal$Week==28]<-sum(week28$UnitSales)
week29<-filter(Cereal, Cereal$Week=="29")
Cereal$pop[Cereal$Week==29]<-sum(week29$UnitSales)
week30<-filter(Cereal, Cereal$Week=="30")
Cereal$pop[Cereal$Week==30]<-sum(week30$UnitSales)
week31<-filter(Cereal, Cereal$Week=="31")
Cereal$pop[Cereal$Week==31]<-sum(week31$UnitSales)
week32<-filter(Cereal, Cereal$Week=="32")
Cereal$pop[Cereal$Week==32]<-sum(week32$UnitSales)
week33<-filter(Cereal, Cereal$Week=="33")
Cereal$pop[Cereal$Week==33]<-sum(week33$UnitSales)
week34<-filter(Cereal, Cereal$Week=="34")
Cereal$pop[Cereal$Week==34]<-sum(week34$UnitSales)
week35<-filter(Cereal, Cereal$Week=="35")
Cereal$pop[Cereal$Week==35]<-sum(week35$UnitSales)
week36<-filter(Cereal, Cereal$Week=="36")
Cereal$pop[Cereal$Week==36]<-sum(week36$UnitSales)
week37<-filter(Cereal, Cereal$Week=="37")
Cereal$pop[Cereal$Week==37]<-sum(week37$UnitSales)
week38<-filter(Cereal, Cereal$Week=="38")
Cereal$pop[Cereal$Week==38]<-sum(week38$UnitSales)
week39<-filter(Cereal, Cereal$Week=="39")
Cereal$pop[Cereal$Week==39]<-sum(week39$UnitSales)
week40<-filter(Cereal, Cereal$Week=="40")
Cereal$pop[Cereal$Week==40]<-sum(week40$UnitSales)
week41<-filter(Cereal, Cereal$Week=="41")
Cereal$pop[Cereal$Week==41]<-sum(week41$UnitSales)
week42<-filter(Cereal, Cereal$Week=="42")
Cereal$pop[Cereal$Week==42]<-sum(week42$UnitSales)
week43<-filter(Cereal, Cereal$Week=="43")
Cereal$pop[Cereal$Week==43]<-sum(week43$UnitSales)
week44<-filter(Cereal, Cereal$Week=="44")
Cereal$pop[Cereal$Week==44]<-sum(week44$UnitSales)
week45<-filter(Cereal, Cereal$Week=="45")
Cereal$pop[Cereal$Week==45]<-sum(week45$UnitSales)
week46<-filter(Cereal, Cereal$Week=="46")
Cereal$pop[Cereal$Week==46]<-sum(week46$UnitSales)
week47<-filter(Cereal, Cereal$Week=="47")
Cereal$pop[Cereal$Week==47]<-sum(week47$UnitSales)
week48<-filter(Cereal, Cereal$Week=="48")
Cereal$pop[Cereal$Week==48]<-sum(week48$UnitSales)
week49<-filter(Cereal, Cereal$Week=="49")
Cereal$pop[Cereal$Week==49]<-sum(week49$UnitSales)
week50<-filter(Cereal, Cereal$Week=="50")
Cereal$pop[Cereal$Week==50]<-sum(week50$UnitSales)
week51<-filter(Cereal, Cereal$Week=="51")
Cereal$pop[Cereal$Week==51]<-sum(week51$UnitSales)
week52<-filter(Cereal, Cereal$Week=="52")
Cereal$pop[Cereal$Week==52]<-sum(week52$UnitSales)
week53<-filter(Cereal, Cereal$Week=="53")
Cereal$pop[Cereal$Week==53]<-sum(week53$UnitSales)
week54<-filter(Cereal, Cereal$Week=="54")
Cereal$pop[Cereal$Week==54]<-sum(week54$UnitSales)
week55<-filter(Cereal, Cereal$Week=="55")
Cereal$pop[Cereal$Week==55]<-sum(week55$UnitSales)
week56<-filter(Cereal, Cereal$Week=="56")
Cereal$pop[Cereal$Week==56]<-sum(week56$UnitSales)
week57<-filter(Cereal, Cereal$Week=="57")
Cereal$pop[Cereal$Week==57]<-sum(week57$UnitSales)
week58<-filter(Cereal, Cereal$Week=="58")
Cereal$pop[Cereal$Week==58]<-sum(week58$UnitSales)
week59<-filter(Cereal, Cereal$Week=="59")
Cereal$pop[Cereal$Week==59]<-sum(week59$UnitSales)
week60<-filter(Cereal, Cereal$Week=="60")
Cereal$pop[Cereal$Week==60]<-sum(week60$UnitSales)
week61<-filter(Cereal, Cereal$Week=="61")
Cereal$pop[Cereal$Week==61]<-sum(week61$UnitSales)
week62<-filter(Cereal, Cereal$Week=="62")
Cereal$pop[Cereal$Week==62]<-sum(week62$UnitSales)
week63<-filter(Cereal, Cereal$Week=="63")
Cereal$pop[Cereal$Week==63]<-sum(week63$UnitSales)
week64<-filter(Cereal, Cereal$Week=="64")
Cereal$pop[Cereal$Week==64]<-sum(week64$UnitSales)
week65<-filter(Cereal, Cereal$Week=="65")
Cereal$pop[Cereal$Week==65]<-sum(week65$UnitSales)
week66<-filter(Cereal, Cereal$Week=="66")
Cereal$pop[Cereal$Week==66]<-sum(week66$UnitSales)
week67<-filter(Cereal, Cereal$Week=="67")
Cereal$pop[Cereal$Week==67]<-sum(week67$UnitSales)
week68<-filter(Cereal, Cereal$Week=="68")
Cereal$pop[Cereal$Week==68]<-sum(week68$UnitSales)
week69<-filter(Cereal, Cereal$Week=="69")
Cereal$pop[Cereal$Week==69]<-sum(week69$UnitSales)
week70<-filter(Cereal, Cereal$Week=="70")
Cereal$pop[Cereal$Week==70]<-sum(week70$UnitSales)
week71<-filter(Cereal, Cereal$Week=="71")
Cereal$pop[Cereal$Week==71]<-sum(week71$UnitSales)
week72<-filter(Cereal, Cereal$Week=="72")
Cereal$pop[Cereal$Week==72]<-sum(week72$UnitSales)
week73<-filter(Cereal, Cereal$Week=="73")
Cereal$pop[Cereal$Week==73]<-sum(week73$UnitSales)
week74<-filter(Cereal, Cereal$Week=="74")
Cereal$pop[Cereal$Week==74]<-sum(week74$UnitSales)
week75<-filter(Cereal, Cereal$Week=="75")
Cereal$pop[Cereal$Week==75]<-sum(week75$UnitSales)
week76<-filter(Cereal, Cereal$Week=="76")
Cereal$pop[Cereal$Week==76]<-sum(week76$UnitSales)
week77<-filter(Cereal, Cereal$Week=="77")
Cereal$pop[Cereal$Week==77]<-sum(week77$UnitSales)
week78<-filter(Cereal, Cereal$Week=="78")
Cereal$pop[Cereal$Week==78]<-sum(week78$UnitSales)
week79<-filter(Cereal, Cereal$Week=="79")
Cereal$pop[Cereal$Week==79]<-sum(week79$UnitSales)
week80<-filter(Cereal, Cereal$Week=="80")
Cereal$pop[Cereal$Week==80]<-sum(week80$UnitSales)
week81<-filter(Cereal, Cereal$Week=="81")
Cereal$pop[Cereal$Week==81]<-sum(week81$UnitSales)
week82<-filter(Cereal, Cereal$Week=="82")
Cereal$pop[Cereal$Week==82]<-sum(week82$UnitSales)
week83<-filter(Cereal, Cereal$Week=="83")
Cereal$pop[Cereal$Week==83]<-sum(week83$UnitSales)
week84<-filter(Cereal, Cereal$Week=="84")
Cereal$pop[Cereal$Week==84]<-sum(week84$UnitSales)
week85<-filter(Cereal, Cereal$Week=="85")
Cereal$pop[Cereal$Week==85]<-sum(week85$UnitSales)
week86<-filter(Cereal, Cereal$Week=="86")
Cereal$pop[Cereal$Week==86]<-sum(week86$UnitSales)
week87<-filter(Cereal, Cereal$Week=="87")
Cereal$pop[Cereal$Week==87]<-sum(week87$UnitSales)
week88<-filter(Cereal, Cereal$Week=="88")
Cereal$pop[Cereal$Week==88]<-sum(week88$UnitSales)
week89<-filter(Cereal, Cereal$Week=="89")
Cereal$pop[Cereal$Week==89]<-sum(week89$UnitSales)
week90<-filter(Cereal, Cereal$Week=="90")
Cereal$pop[Cereal$Week==90]<-sum(week90$UnitSales)
week91<-filter(Cereal, Cereal$Week=="91")
Cereal$pop[Cereal$Week==91]<-sum(week91$UnitSales)
week92<-filter(Cereal, Cereal$Week=="92")
Cereal$pop[Cereal$Week==92]<-sum(week92$UnitSales)
week93<-filter(Cereal, Cereal$Week=="93")
Cereal$pop[Cereal$Week==93]<-sum(week93$UnitSales)
week94<-filter(Cereal, Cereal$Week=="94")
Cereal$pop[Cereal$Week==94]<-sum(week94$UnitSales)
week95<-filter(Cereal, Cereal$Week=="95")
Cereal$pop[Cereal$Week==95]<-sum(week95$UnitSales)

#-------------------------------------------------------------------
# CONSTRUCTING A Y VARIABLE
#-------------------------------------------------------------------

# Yit  = MARKET SHARE
#      = QUANTITY SOLD PER PRODUCT DESCRIPTION PER WEEK/ 
#        POPULATION PER WEEK
#      = log(MSit) - log(MSot)
#      = (log(ProductDescription$Unitsales/Cereal$pop)) - log((pop-ProductDescription$UnitSales)/pop))

# MSot = outside goods at time t/ population at time t
#      = (population - total quantity)/population

Cereal$Yit<-((log(Cereal$UnitSales/Cereal$pop)) - log((Cereal$pop-Cereal$UnitSales)/Cereal$pop))
Cereal[Cereal=="-Inf"]<-0
# taking care of infinity values
Cereal[is.na(Cereal)]<-0
# making nan values equal to 0 again (had nan in pricing column)

#-------------------------------------------------------------------
######### 4. MAKING PARENT LEVEL DATA SETS: AS TIBBLES #############
#-------------------------------------------------------------------

General_Mills<-filter(Cereal, AttrParentLevel =="GENERAL MILLS")
Kellogg_Co<-filter(Cereal, AttrParentLevel =="KELLOGG CO")
PepsiCo<-filter(Cereal, AttrParentLevel =="PEPSICO INC")
US_Mills<-filter(Cereal, AttrParentLevel =="US MILLS INC")
Health_Valley<-filter(Cereal, AttrParentLevel =="HEALTH VALLEY -10000000000TURAL FOODS")
McKee<-filter(Cereal, AttrParentLevel =="MCKEE FOODS CORPORATION")
MaltOMeal<-filter(Cereal, AttrParentLevel =="MALT-O-MEAL CO")
Private_Label<-filter(Cereal, AttrParentLevel =="PRIVATE LABEL")
Weetabix<-filter(Cereal, AttrParentLevel =="WEETABIX LTD")
Organic_Milling<-filter(Cereal, AttrParentLevel =="ORGANIC MILLING COMPANY")
World_Finer_Foods<-filter(Cereal, AttrParentLevel =="WORLD FINER FODDS")
Golden_Temple<-filter(Cereal, AttrParentLevel =="GOLDEN TEMPLE BAKERY INC")
The_Baker<-filter(Cereal, AttrParentLevel =="THE BAKER")
Philip_Morris<-filter(Cereal, AttrParentLevel =="PHILIP MORRIS CO INC")
Hain_Celestial<-filter(Cereal, AttrParentLevel =="THE HAIN CELESTIAL GROUP INC")
Weetibakes<-filter(Cereal, AttrParentLevel =="WEETIBAKES LTD")
Outrageous_FruitandGrains<-filter(Cereal, AttrParentLevel =="OUTRAGEOUS FRUIT & GRAINS")
Natures_Path<-filter(Cereal, AttrParentLevel =="-10000000000TURE'S PATH")

#-------------------------------------------------------------------
################## 5. Exploring the Data ###########################
#-------------------------------------------------------------------

# four biggest we are most concerned with are:
# GENERAL MILLS
# KELLOGG
# PEPSICO
# PRIVATE LABEL

#-------------------------------------------------------------------
# FINDING HOW MANY PRODUCT DESCRIPTIONS THERE WILL BE 
#-------------------------------------------------------------------

unique(sort(General_Mills$ProductDescription))
# 86
unique(sort(Kellogg_Co$ProductDescription))
# 108
unique(sort(PepsiCo$ProductDescription))
# 149
unique(sort(Private_Label$ProductDescription))
# 142
# TOTAL = 485
#       *95 WEEKS = 46075 RECORDS

#-------------------------------------------------------------------
# MAKING DATA FRAMES OF THE 4 PARENT GROUPS FOR PRICE/QUANTITY
#-------------------------------------------------------------------

GeneralMillsQuantPrice<-cbind(General_Mills$Pricing,General_Mills$UnitSales)
GeneralMillsQuant<-data.frame(GeneralMillsQuantPrice)
colnames(GeneralMillsQuant)[1] = 'Price'
colnames(GeneralMillsQuant)[2] = 'Quantity'

KelloggQuantPrice<-cbind(Kellogg_Co$Pricing,Kellogg_Co$UnitSales)
KelloggQuant<-data.frame(KelloggQuantPrice)
colnames(KelloggQuant)[1] = 'Price'
colnames(KelloggQuant)[2] = 'Quantity'

PrivateQuantPrice<-cbind(Private_Label$Pricing,Private_Label$UnitSales)
PrivateQuant<-data.frame(PrivateQuantPrice)
colnames(PrivateQuant)[1] = 'Price'
colnames(PrivateQuant)[2] = 'Quantity'

PepsiCoQuantPrice<-cbind(PepsiCo$Pricing,PepsiCo$UnitSales)
PepsiCoQuant<-data.frame(PepsiCoQuantPrice)
colnames(PepsiCoQuant)[1] = 'Price'
colnames(PepsiCoQuant)[2] = 'Quantity'

#-------------------------------------------------------------------
# MAKING GGPLOTS OF THE 4 PARENT GROUPS FOR PRICE/QUANTITY
#-------------------------------------------------------------------

ggplot(data=GeneralMillsQuant,aes(x=Quantity, y =Price))+
  geom_point() +
  coord_cartesian() +
  scale_color_gradient() +
  theme_bw()+
  ggtitle("General Mills")

ggplot(data = KelloggQuant, aes(x= Quantity, y = Price))+
  geom_point() +
  coord_cartesian() +
  scale_color_gradient() +
  theme_bw()+
  ggtitle("Kellogg")

ggplot(data = PrivateQuant, aes(x= Quantity, y = Price))+
  geom_point() +
  coord_cartesian() +
  scale_color_gradient() +
  theme_bw()+
  ggtitle("Private Label")

ggplot(data = PepsiCoQuant, aes(x= Quantity, y = Price))+
  geom_point() +
  coord_cartesian() +
  scale_color_gradient() +
  theme_bw()+
  ggtitle("PepsiCo")
  
 #-------------------------------------------------------------------
# MAKING DATA FRAMES OF THE 4 PARENT GROUPS FOR PRICE/WEEK
#-------------------------------------------------------------------

GeneralMillsWeekPrice<-cbind(General_Mills$Pricing,General_Mills$Week)
GeneralMillsWeek<-data.frame(GeneralMillsWeekPrice)
colnames(GeneralMillsWeek)[1] = 'Price'
colnames(GeneralMillsWeek)[2] = 'Week'

KelloggWeekPrice<-cbind(Kellogg_Co$Pricing,Kellogg_Co$Week)
KelloggWeek<-data.frame(KelloggWeekPrice)
colnames(KelloggWeek)[1] = 'Price'
colnames(KelloggWeek)[2] = 'Week'

PrivateWeekPrice<-cbind(Private_Label$Pricing,Private_Label$Week)
PrivateWeek<-data.frame(PrivateWeekPrice)
colnames(PrivateWeek)[1] = 'Price'
colnames(PrivateWeek)[2] = 'Week'

PepsiCoWeekPrice<-cbind(PepsiCo$Pricing,PepsiCo$Week)
PepsiCoWeek<-data.frame(PepsiCoWeekPrice)
colnames(PepsiCoWeek)[1] = 'Price'
colnames(PepsiCoWeek)[2] = 'Week'

#-------------------------------------------------------------------
# MAKING GGPLOTS OF THE 4 PARENT GROUPS FOR PRICE/WEEK
#-------------------------------------------------------------------

ggplot(data=GeneralMillsWeek,aes(x=Week, y =Price))+
  geom_point() +
  coord_cartesian() +
  scale_color_gradient() +
  theme_bw()+
  ggtitle("General Mills")

ggplot(data=KelloggWeek, aes(x=Week, y=Price))+
  geom_point() +
  coord_cartesian() +
  scale_color_gradient() +
  theme_bw()+
  ggtitle("Kellogg")

ggplot(data=PrivateWeek, aes(x=Week, y=Price))+
  geom_point() +
  coord_cartesian() +
  scale_color_gradient() +
  theme_bw()+
  ggtitle("Private Label")

ggplot(data = PepsiCoWeek, aes(x=Week, y =Price))+
  geom_point() +
  coord_cartesian() +
  scale_color_gradient() +
  theme_bw()+
  ggtitle("PepsiCo")
  
 #-------------------------------------------------------------------
# MAKING DATA FRAMES OF THE 4 PARENT GROUPS FOR PRICE PER OZ/WEEK
#-------------------------------------------------------------------

GeneralMillsWeekPPO<-cbind(General_Mills$PricePerOZ,General_Mills$Week)
GeneralMillsWeekPPO<-data.frame(GeneralMillsWeekPPO)
colnames(GeneralMillsWeekPPO)[1] = 'Price'
colnames(GeneralMillsWeekPPO)[2] = 'Week'

KelloggWeekPPO<-cbind(Kellogg_Co$PricePerOZ,Kellogg_Co$Week)
KelloggWeekPPO<-data.frame(KelloggWeekPPO)
colnames(KelloggWeekPPO)[1] = 'Price'
colnames(KelloggWeekPPO)[2] = 'Week'

PrivateWeekPPO<-cbind(Private_Label$PricePerOZ,Private_Label$Week)
PrivateWeekPPO<-data.frame(PrivateWeekPPO)
colnames(PrivatePPO)[1] = 'Price'
colnames(PrivatePPO)[2] = 'Week'

PepsiCoWeekPPO<-cbind(PepsiCo$PricePerOZ,PepsiCo$Week)
PepsiCoWeekPPO<-data.frame(PepsiCoWeekPPO)
colnames(PepsiCoWeekPPO)[1] = 'Price'
colnames(PepsiCoWeekPPO)[2] = 'Week'

#-------------------------------------------------------------------
# MAKING GGPLOTS OF THE 4 PARENT GROUPS FOR PRICE PER OZ/WEEK
#-------------------------------------------------------------------

ggplot(data=GeneralMillsWeekPPO,aes(x=Week, y=Price))+
  geom_point() +
  coord_cartesian() +
  scale_color_gradient() +
  theme_bw()+
  ggtitle("General Mills")

ggplot(data = KelloggWeekPPO, aes(x=Week, y=Price))+
  geom_point() +
  coord_cartesian() +
  scale_color_gradient() +
  theme_bw()+
  ggtitle("Kellogg")

ggplot(data = PrivateWeekPPO, aes(x=Week, y=Price))+
  geom_point() +
  coord_cartesian() +
  scale_color_gradient() +
  theme_bw()+
  ggtitle("Private Label")

ggplot(data = PepsiCoWeekPPO, aes(x=Week, y=Price))+
  geom_point() +
  coord_cartesian() +
  scale_color_gradient() +
  theme_bw()+
  ggtitle("PepsiCo")
  
 #-------------------------------------------------------------------
# MAKING DATA FRAMES OF THE 4 PARENT GROUPS FOR Quantity/Week
#-------------------------------------------------------------------

GeneralMillsWeekQ<-cbind(General_Mills$UnitSales,General_Mills$Week)
GeneralMillsWeekQ<-data.frame(GeneralMillsWeekQ)
colnames(GeneralMillsWeekQ)[1] = 'Quantity'
colnames(GeneralMillsWeekQ)[2] = 'Week'

KelloggWeekQ<-cbind(Kellogg_Co$UnitSales,Kellogg_Co$Week)
KelloggWeekQ<-data.frame(KelloggWeekQ)
colnames(KelloggWeekQ)[1] = 'Quantity'
colnames(KelloggWeekQ)[2] = 'Week'

PrivateWeekQ<-cbind(Private_Label$UnitSales,Private_Label$Week)
PrivateWeekQ<-data.frame(PrivateWeekQ)
colnames(PrivateQ)[1] = 'Quantity'
colnames(PrivateQ)[2] = 'Week'

PepsiCoWeekQ<-cbind(PepsiCo$UnitSales,PepsiCo$Week)
PepsiCoWeekQ<-data.frame(PepsiCoWeekQ)
colnames(PepsiCoWeekQ)[1] = 'Quantity'
colnames(PepsiCoWeekQ)[2] = 'Week'

#-------------------------------------------------------------------
# MAKING GGPLOTS OF THE 4 PARENT GROUPS FOR QUANTITY/WEEK
#-------------------------------------------------------------------

ggplot(data=GeneralMillsWeekQ,aes(x=Week, y=Quantity))+
  geom_point() +
  coord_cartesian() +
  scale_color_gradient() +
  theme_bw()+
  ggtitle("General Mills")

ggplot(data = KelloggWeekQ, aes(x=Week, y=Quantity))+
  geom_point() +
  coord_cartesian() +
  scale_color_gradient() +
  theme_bw()+
  ggtitle("Kellogg")

ggplot(data = PrivateWeekQ, aes(x=Week, y=Quantity))+
  geom_point() +
  coord_cartesian() +
  scale_color_gradient() +
  theme_bw()+
  ggtitle("Private Label")

ggplot(data = PepsiCoWeekQ, aes(x=Week, y=Quantity))+
  geom_point() +
  coord_cartesian() +
  scale_color_gradient() +
  theme_bw()+
  ggtitle("PepsiCo")
  
 #-------------------------------------------------------------------
# MAKING DATA FRAMES OF THE 4 PARENT GROUPS FOR PRICE/GAS
#-------------------------------------------------------------------

GeneralMillsGas<-cbind(General_Mills$Pricing,General_Mills$gas)
GeneralMillsGas<-data.frame(GeneralMillsGas)
colnames(GeneralMillsGas)[1] = 'Price'
colnames(GeneralMillsGas)[2] = 'Gas'

KelloggGas<-cbind(Kellogg_Co$Pricing,Kellogg_Co$gas)
KelloggGas<-data.frame(KelloggGas)
colnames(KelloggGas)[1] = 'Price'
colnames(KelloggGas)[2] = 'Gas'

PrivateGas<-cbind(Private_Label$Pricing,Private_Label$gas)
PrivateGas<-data.frame(PrivateGas)
colnames(PrivateGas)[1] = 'Price'
colnames(PrivateGas)[2] = 'Gas'

PepsiCoGas<-cbind(PepsiCo$Pricing,PepsiCo$gas)
PepsiCoGas<-data.frame(PepsiCoGas)
colnames(PepsiGas)[1] = 'Price'
colnames(PepsiGas)[2] = 'Gas'

#-------------------------------------------------------------------
# MAKING GGPLOTS OF THE 4 PARENT GROUPS FOR PRICE/GAS
#-------------------------------------------------------------------

ggplot(data=GeneralMillsGas,aes(x=Gas, y =Price))+
  geom_point() +
  coord_cartesian() +
  scale_color_gradient() +
  theme_bw()+
  ggtitle("General Mills")

ggplot(data=KelloggGas, aes(x=Gas, y=Price))+
  geom_point() +
  coord_cartesian() +
  scale_color_gradient() +
  theme_bw()+
  ggtitle("Kellogg")

ggplot(data=PrivateGas, aes(x=Gas, y=Price))+
  geom_point() +
  coord_cartesian() +
  scale_color_gradient() +
  theme_bw()+
  ggtitle("Private Label")

ggplot(data = PepsiCoGas, aes(x=Gas, y =Price))+
  geom_point() +
  coord_cartesian() +
  scale_color_gradient() +
  theme_bw()+
  ggtitle("PepsiCo")
  
 #-------------------------------------------------------------------
# MAKING DATA FRAMES OF THE 4 PARENT GROUPS FOR PRICE/LABOR COSTS
#-------------------------------------------------------------------

GeneralMillsLaborCost<-cbind(General_Mills$Pricing,General_Mills$LaborCost)
GeneralMillsLaborCost<-data.frame(GeneralMillsLaborCost)
colnames(GeneralMillsLaborCost)[1] = 'Price'
colnames(GeneralMillsLaborCost)[2] = 'LaborCost'

KelloggLaborCost<-cbind(Kellogg_Co$Pricing,Kellogg_Co$LaborCost)
KelloggLaborCost<-data.frame(KelloggLaborCost)
colnames(KelloggLaborCost)[1] = 'Price'
colnames(KelloggLaborCost)[2] = 'LaborCost'

PrivateLaborCost<-cbind(Private_Label$Pricing,Private_Label$LaborCost)
PrivateLaborCost<-data.frame(PrivateLaborCost)
colnames(PrivateLaborCost)[1] = 'Price'
colnames(PrivateLaborCost)[2] = 'LaborCost'

PepsiCoLaborCost<-cbind(PepsiCo$Pricing,PepsiCo$LaborCost)
PepsiCoLaborCost<-data.frame(PepsiCoLaborCost)
colnames(PepsiCoLaborCost)[1] = 'Price'
colnames(PepsiCoLaborCost)[2] = 'LaborCost'

#-------------------------------------------------------------------
# MAKING DATA FRAMES OF THE 4 PARENT GROUPS FOR PRICE/CALORIES
#-------------------------------------------------------------------

GeneralMillsCalories<-cbind(General_Mills$Pricing,General_Mills$char1_calories)
GeneralMillsCalories<-data.frame(GeneralMillsCalories)
colnames(GeneralMillsCalories)[1] = 'Price'
colnames(GeneralMillsCalories)[2] = 'Calories'

KelloggCalories<-cbind(Kellogg_Co$Pricing,Kellogg_Co$char1_calories)
KelloggCalories<-data.frame(KelloggCalories)
colnames(KelloggCalories)[1] = 'Price'
colnames(KelloggCalories)[2] = 'Calories'

PrivateCalories<-cbind(Private_Label$Pricing,Private_Label$char1_calories)
PrivateCalories<-data.frame(PrivateCalories)
colnames(PrivateCalories)[1] = 'Price'
colnames(PrivateCalories)[2] = 'Calories'

PepsiCoCalories<-cbind(PepsiCo$Pricing,PepsiCo$char1_calories)
PepsiCoCalories<-data.frame(PepsiCoCalories)
colnames(PepsiCoCalories)[1] = 'Price'
colnames(PepsiCoCalories)[2] = 'Calories'

#-------------------------------------------------------------------
# MAKING DATA FRAMES OF THE 4 PARENT GROUPS FOR PRICE/CARBS
#-------------------------------------------------------------------

GeneralMillsCarbs<-cbind(General_Mills$Pricing,General_Mills$char2_carb)
GeneralMillsCarbs<-data.frame(GeneralMillsCarbs)
colnames(GeneralMillsCarbs)[1] = 'Price'
colnames(GeneralMillsCarbs)[2] = 'Carbs'

KelloggCarbs<-cbind(Kellogg_Co$Pricing,Kellogg_Co$char2_carb)
KelloggCarbs<-data.frame(KelloggCarbs)
colnames(KelloggCarbs)[1] = 'Price'
colnames(KelloggCarbs)[2] = 'Carbs'

PrivateCarbs<-cbind(Private_Label$Pricing,Private_Label$char2_carb)
PrivateCarbs<-data.frame(PrivateCarbs)
colnames(PrivateCarbs)[1] = 'Price'
colnames(PrivateCarbs)[2] = 'Carbs'

PepsiCoCarbs<-cbind(PepsiCo$Pricing,PepsiCo$char2_carb)
PepsiCoCarbs<-data.frame(PepsiCoCarbs)
colnames(PepsiCoCarbs)[1] = 'Price'
colnames(PepsiCoCarbs)[2] = 'Carbs'

#-------------------------------------------------------------------
# MAKING DATA FRAMES OF THE 4 PARENT GROUPS FOR PRICE/PROTEIN
#-------------------------------------------------------------------

GeneralMillsProtein<-cbind(General_Mills$Pricing,General_Mills$char3_protein)
GeneralMillsProtein<-data.frame(GeneralMillsProtein)
colnames(GeneralMillsProtein)[1] = 'Price'
colnames(GeneralMillsProtein)[2] = 'Protein'

KelloggProtein<-cbind(Kellogg_Co$Pricing,Kellogg_Co$char3_protein)
KelloggProtein<-data.frame(KelloggProtein)
colnames(KelloggProtein)[1] = 'Price'
colnames(KelloggProtein)[2] = 'Protein'

PrivateProtein<-cbind(Private_Label$Pricing,Private_Label$char3_protein)
PrivateProtein<-data.frame(PrivateProtein)
colnames(PrivateProtein)[1] = 'Price'
colnames(PrivateProtein)[2] = 'Protein'

PepsiCoProtein<-cbind(PepsiCo$Pricing,PepsiCo$char3_protein)
PepsiCoProtein<-data.frame(PepsiCoProtein)
colnames(PepsiCoProtein)[1] = 'Price'
colnames(PepsiCoProtein)[2] = 'Protein'

#-------------------------------------------------------------------
# MAKING DATA FRAMES OF THE 4 PARENT GROUPS FOR PRICE/BOXSIZE
#-------------------------------------------------------------------

GeneralMillsBox<-cbind(General_Mills$Pricing,General_Mills$char4_boxsize)
GeneralMillsBox<-data.frame(GeneralMillsBox)
colnames(GeneralMillsBox)[1] = 'Price'
colnames(GeneralMillsBox)[2] = 'Box'

KelloggBox<-cbind(Kellogg_Co$Pricing,Kellogg_Co$char4_boxsize)
KelloggBox<-data.frame(KelloggBox)
colnames(KelloggBox)[1] = 'Price'
colnames(KelloggBox)[2] = 'Box'

PrivateBox<-cbind(Private_Label$Pricing,Private_Label$char4_boxsize)
PrivateBox<-data.frame(PrivateBox)
colnames(PrivateBox)[1] = 'Price'
colnames(PrivateBox)[2] = 'Box'

PepsiCoBox<-cbind(PepsiCo$Pricing,PepsiCo$char4_boxsize)
PepsiCoBox<-data.frame(PepsiCoBox)
colnames(PepsiCoBox)[1] = 'Price'
colnames(PepsiCoBox)[2] = 'Box'

#-------------------------------------------------------------------
# MAKING DATA FRAMES OF THE 4 PARENT GROUPS FOR PRICE/SUGAR
#-------------------------------------------------------------------

GeneralMillsSugar<-cbind(General_Mills$Pricing,General_Mills$char5_sugar)
GeneralMillsSugar<-data.frame(GeneralMillsSugar)
colnames(GeneralMillsSugar)[1] = 'Price'
colnames(GeneralMillsSugar)[2] = 'Sugar'

KelloggSugar<-cbind(Kellogg_Co$Pricing,Kellogg_Co$char5_sugar)
KelloggSugar<-data.frame(KelloggSugar)
colnames(KelloggSugar)[1] = 'Price'
colnames(KelloggSugar)[2] = 'Sugar'

PrivateSugar<-cbind(Private_Label$Pricing,Private_Label$char5_sugar)
PrivateSugar<-data.frame(PrivateSugar)
colnames(PrivateSugar)[1] = 'Price'
colnames(PrivateSugar)[2] = 'Sugar'

PepsiCoSugar<-cbind(PepsiCo$Pricing,PepsiCo$char5_sugar)
PepsiCoSugar<-data.frame(PepsiCoSugar)
colnames(PepsiCoSugar)[1] = 'Price'
colnames(PepsiCoSugar)[2] = 'Sugar'

#-------------------------------------------------------------------
# MAKING DATA FRAMES OF THE 4 PARENT GROUPS FOR PRICE/CORN
#-------------------------------------------------------------------

GeneralMillsCorn<-cbind(General_Mills$Pricing,General_Mills$corn)
GeneralMillsCorn<-data.frame(GeneralMillsCorn)
colnames(GeneralMillsCorn)[1] = 'Price'
colnames(GeneralMillsCorn)[2] = 'Corn'

KelloggCorn<-cbind(Kellogg_Co$Pricing,Kellogg_Co$corn)
KelloggCorn<-data.frame(KelloggCorn)
colnames(KelloggCorn)[1] = 'Price'
colnames(KelloggCorn)[2] = 'Corn'

PrivateCorn<-cbind(Private_Label$Pricing,Private_Label$corn)
PrivateCorn<-data.frame(PrivateCorn)
colnames(PrivateCorn)[1] = 'Price'
colnames(PrivateCorn)[2] = 'Corn'

PepsiCoCorn<-cbind(PepsiCo$Pricing,PepsiCo$corn)
PepsiCoCorn<-data.frame(PepsiCoCorn)
colnames(PepsiCoCorn)[1] = 'Price'
colnames(PepsiCoCorn)[2] = 'Corn'

#-------------------------------------------------------------------
# MAKING DATA FRAMES OF THE 4 PARENT GROUPS FOR PRICE/WHEAT
#-------------------------------------------------------------------

GeneralMillsWheat<-cbind(General_Mills$Pricing,General_Mills$wheat)
GeneralMillsWheat<-data.frame(GeneralMillsWheat)
colnames(GeneralMillsWheat)[1] = 'Price'
colnames(GeneralMillsWheat)[2] = 'Wheat'

KelloggWheat<-cbind(Kellogg_Co$Pricing,Kellogg_Co$wheat)
KelloggWheat<-data.frame(KelloggWheat)
colnames(KelloggWheat)[1] = 'Price'
colnames(KelloggWheat)[2] = 'Wheat'

PrivateWheat<-cbind(Private_Label$Pricing,Private_Label$wheat)
PrivateWheat<-data.frame(PrivateWheat)
colnames(PrivateWheat)[1] = 'Price'
colnames(PrivateWheat)[2] = 'Wheat'

PepsiCoWheat<-cbind(PepsiCo$Pricing,PepsiCo$wheat)
PepsiCoWheat<-data.frame(PepsiCoWheat)
colnames(PepsiCoWheat)[1] = 'Price'
colnames(PepsiCoWheat)[2] = 'Wheat'

#-------------------------------------------------------------------
# MAKING DATA FRAMES OF THE 4 PARENT GROUPS FOR PRICE/SUGAR COST
#-------------------------------------------------------------------

GeneralMillsSugCost<-cbind(General_Mills$Pricing,General_Mills$sugar)
GeneralMillsSugCost<-data.frame(GeneralMillsSugCost)
colnames(GeneralMillsSugCost)[1] = 'Price'
colnames(GeneralMillsSugCost)[2] = 'SugCost'

KelloggSugCost<-cbind(Kellogg_Co$Pricing,Kellogg_Co$sugar)
KelloggSugCost<-data.frame(KelloggSugCost)
colnames(KelloggSugCost)[1] = 'Price'
colnames(KelloggSugCost)[2] = 'SugCost'

PrivateSugCost<-cbind(Private_Label$Pricing,Private_Label$sugar)
PrivateSugCost<-data.frame(PrivateSugCost)
colnames(PrivateSugCost)[1] = 'Price'
colnames(PrivateSugCost)[2] = 'SugCost'

PepsiCoSugCost<-cbind(PepsiCo$Pricing,PepsiCo$sugar)
PepsiCoSugCost<-data.frame(PepsiCoSugCost)
colnames(PepsiCoSugCost)[1] = 'Price'
colnames(PepsiCoSugCost)[2] = 'SugCost'

#-------------------------------------------------------------------
# GENERAL MILLS SUMMARY TABLES
#-------------------------------------------------------------------

stargazer(summary(GeneralMillsQuant))
stargazer(summary(GeneralMillsWeek))
stargazer(summary(GeneralMillsWeekPPO))
stargazer(summary(GeneralMillsWeekQ))
stargazer(summary(GeneralMillsCalories))
stargazer(summary(GeneralMillsCarbs))
stargazer(summary(GeneralMillsProtein))
stargazer(summary(GeneralMillsBox))
stargazer(summary(GeneralMillsSugar))
stargazer(summary(GeneralMillsGas))
stargazer(summary(GeneralMillsCorn))
stargazer(summary(GeneralMillsWheat))
stargazer(summary(GeneralMillsSugCost))
stargazer(summary(GeneralMillsLaborCost))

#-------------------------------------------------------------------
# KELLOGG SUMMARY TABLES
#-------------------------------------------------------------------

stargazer(summary(KelloggQuant))
stargazer(summary(KelloggWeek))
stargazer(summary(KelloggWeekPPO))
stargazer(summary(KelloggWeekQ))
stargazer(summary(KelloggCalories))
stargazer(summary(KelloggCarbs))
stargazer(summary(KelloggProtein))
stargazer(summary(KelloggBox))
stargazer(summary(KelloggSugar))
stargazer(summary(KelloggGas))
stargazer(summary(KelloggCorn))
stargazer(summary(KelloggWheat))
stargazer(summary(KelloggSugCost))
stargazer(summary(KelloggLaborCost))

#-------------------------------------------------------------------
# PRIVATE LABEL SUMMARY TABLES
#-------------------------------------------------------------------

stargazer(summary(PrivateQuant))
stargazer(summary(PrivateWeek))
stargazer(summary(PrivateWeekPPO))
stargazer(summary(PrivateWeekQ))
stargazer(summary(PrivateCalories))
stargazer(summary(PrivateCarbs))
stargazer(summary(PrivateProtein))
stargazer(summary(PrivateBox))
stargazer(summary(PrivateSugar))
stargazer(summary(PrivateGas))
stargazer(summary(PrivateCorn))
stargazer(summary(PrivateWheat))
stargazer(summary(PrivateSugCost))
stargazer(summary(PrivateLaborCost))

#-------------------------------------------------------------------
# PEPSI CO SUMMARY TABLES
#-------------------------------------------------------------------

stargazer(summary(PepsiCoQuant))
stargazer(summary(PepsiCoWeek))
stargazer(summary(PepsiCoWeekPPO))
stargazer(summary(PepsiCoWeekQ))
stargazer(summary(PepsiCoCalories))
stargazer(summary(PepsiCoCarbs))
stargazer(summary(PepsiCoProtein))
stargazer(summary(PepsiCoBox))
stargazer(summary(PepsiCoSugar))
stargazer(summary(PepsiCoGas))
stargazer(summary(PepsiCoCorn))
stargazer(summary(PepsiCoWheat))
stargazer(summary(PepsiCoSugCost))
stargazer(summary(PepsiCoLaborCost))

#-------------------------------------------------------------------
# GENERAL MILLS SIMPLE LINEAR REGRESSIONS
#-------------------------------------------------------------------

GenralMillsPQ<-lm(General_Mills$Pricing ~ General_Mills$UnitSales -1)
stargazer(GenralMillsPQ)
GenralMillsPWeek<-lm(General_Mills$Pricing ~ General_Mills$Week -1)
stargazer(GenralMillsPWeek)
GenralMillsPPOWeek<-lm(General_Mills$PricePerOZ ~ General_Mills$Week -1)
stargazer(GenralMillsPPOWeek)
GenralMillsQuantWeek<-lm(General_Mills$UnitSales ~ General_Mills$Week -1)
stargazer(GenralMillsQuantWeek)
GenralMillsCal<-lm(General_Mills$Pricing ~ General_Mills$char1_calories -1)
stargazer(GenralMillsCal)
GenralMillsCarb<-lm(General_Mills$Pricing ~ General_Mills$char2_carb -1)
stargazer(GenralMillsCarb)
GenralMillsProt<-lm(General_Mills$Pricing ~ General_Mills$char3_protein -1)
stargazer(GenralMillsProt)
GenralMillsBsize<-lm(General_Mills$Pricing ~ General_Mills$char4_boxsize -1)
stargazer(GenralMillsBsize)
GenralMillsSugarChar<-lm(General_Mills$Pricing ~ General_Mills$char5_sugar -1)
stargazer(GenralMillsSugarChar)
GenralMillsGasChar<-lm(General_Mills$Pricing ~ General_Mills$gas -1)
stargazer(GenralMillsGasChar)
GenralMillsCornChar<-lm(General_Mills$Pricing ~ General_Mills$corn -1)
stargazer(GenralMillsCornChar)
GenralMillsWhtChar<-lm(General_Mills$Pricing ~ General_Mills$wheat -1)
stargazer(GenralMillsWhtChar)
GenralMillsSUGcost<-lm(General_Mills$Pricing ~ General_Mills$sugar -1)
stargazer(GenralMillsSUGcost)
GenralMillsLABcost<-lm(General_Mills$Pricing ~ General_Mills$LaborCost -1)
stargazer(GenralMillsLABcost)

#-------------------------------------------------------------------
# KELLOGG SIMPLE LINEAR REGRESSIONS
#-------------------------------------------------------------------

Kellogg_CoPQ<-lm(Kellogg_Co$Pricing ~ Kellogg_Co$UnitSales -1)
stargazer(Kellogg_CoPQ)
Kellogg_CoPWeek<-lm(Kellogg_Co$Pricing ~ Kellogg_Co$Week -1)
stargazer(Kellogg_CoPWeek)
Kellogg_CoPPOWeek<-lm(Kellogg_Co$PricePerOZ ~ Kellogg_Co$Week -1)
stargazer(Kellogg_CoPPOWeek)
Kellogg_CoQuantWeek<-lm(Kellogg_Co$UnitSales ~ Kellogg_Co$Week -1)
stargazer(Kellogg_CoQuantWeek)
Kellogg_CoCal<-lm(Kellogg_Co$Pricing ~ Kellogg_Co$char1_calories -1)
stargazer(Kellogg_CoCal)
Kellogg_CoCarb<-lm(Kellogg_Co$Pricing ~ Kellogg_Co$char2_carb -1)
stargazer(Kellogg_CoCarb)
Kellogg_CoProt<-lm(Kellogg_Co$Pricing ~ Kellogg_Co$char3_protein -1)
stargazer(Kellogg_CoProt)
Kellogg_CoBsize<-lm(Kellogg_Co$Pricing ~ Kellogg_Co$char4_boxsize -1)
stargazer(Kellogg_CoBsize)
Kellogg_CoSugarChar<-lm(Kellogg_Co$Pricing ~ Kellogg_Co$char5_sugar -1)
stargazer(Kellogg_CoSugarChar)
Kellogg_CoGasChar<-lm(Kellogg_Co$Pricing ~ Kellogg_Co$gas -1)
stargazer(Kellogg_CoGasChar)
Kellogg_CoCornChar<-lm(Kellogg_Co$Pricing ~ Kellogg_Co$corn -1)
stargazer(Kellogg_CoCornChar)
Kellogg_CoWhtChar<-lm(Kellogg_Co$Pricing ~ Kellogg_Co$wheat -1)
stargazer(Kellogg_CoWhtChar)
Kellogg_CoSUGcost<-lm(Kellogg_Co$Pricing ~ Kellogg_Co$sugar -1)
stargazer(Kellogg_CoSUGcost)
Kellogg_CoLABcost<-lm(Kellogg_Co$Pricing ~ Kellogg_Co$LaborCost -1)
stargazer(Kellogg_CoLABcost)

#-------------------------------------------------------------------
# PRIVATE LABEL SIMPLE LINEAR REGRESSIONS
#-------------------------------------------------------------------

Private_LabelPQ<-lm(Private_Label$Pricing ~ Private_Label$UnitSales -1)
stargazer(Private_LabelPQ)
Private_LabelPWeek<-lm(Private_Label$Pricing ~ Private_Label$Week -1)
stargazer(Private_LabelPWeek)
Private_LabelPPOWeek<-lm(Private_Label$PricePerOZ ~ Private_Label$Week -1)
stargazer(Private_LabelPPOWeek)
Private_LabelQuantWeek<-lm(Private_Label$UnitSales ~ Private_Label$Week -1)
stargazer(Private_LabelQuantWeek)
Private_LabelCal<-lm(Private_Label$Pricing ~ Private_Label$char1_calories -1)
stargazer(Private_LabelCal)
Private_LabelCarb<-lm(Private_Label$Pricing ~ Private_Label$char2_carb -1)
stargazer(Private_LabelCarb)
Private_LabelProt<-lm(Private_Label$Pricing ~ Private_Label$char3_protein -1)
stargazer(Private_LabelProt)
Private_LabelBsize<-lm(Private_Label$Pricing ~ Private_Label$char4_boxsize -1)
stargazer(Private_LabelBsize)
Private_LabelSugarChar<-lm(Private_Label$Pricing ~ Private_Label$char5_sugar -1)
stargazer(Private_LabelSugarChar)
Private_LabelGasChar<-lm(Private_Label$Pricing ~ Private_Label$gas -1)
stargazer(Private_LabelGasChar)
Private_LabelCornChar<-lm(Private_Label$Pricing ~ Private_Label$corn -1)
stargazer(Private_LabelCornChar)
Private_LabelWhtChar<-lm(Private_Label$Pricing ~ Private_Label$wheat -1)
stargazer(Private_LabelWhtChar)
Private_LabelSUGcost<-lm(Private_Label$Pricing ~ Private_Label$sugar -1)
stargazer(Private_LabelSUGcost)
Private_LabelLABcost<-lm(Private_Label$Pricing ~ Private_Label$LaborCost -1)
stargazer(Private_LabelLABcost)

#-------------------------------------------------------------------
# PEPSICO SIMPLE LINEAR REGRESSIONS
#-------------------------------------------------------------------

PepsiCoPQ<-lm(PepsiCo$Pricing ~ PepsiCo$UnitSales -1)
stargazer(PepsiCoPQ)
PepsiCoPWeek<-lm(PepsiCo$Pricing ~ PepsiCo$Week -1)
stargazer(PepsiCoPWeek)
PepsiCoPPOWeek<-lm(PepsiCo$PricePerOZ ~ PepsiCo$Week -1)
stargazer(PepsiCoPPOWeek)
PepsiCoQuantWeek<-lm(PepsiCo$UnitSales ~ PepsiCo$Week -1)
stargazer(PepsiCoQuantWeek)
PepsiCoCal<-lm(PepsiCo$Pricing ~ PepsiCo$char1_calories -1)
stargazer(PepsiCoCal)
PepsiCoCarb<-lm(PepsiCo$Pricing ~ PepsiCo$char2_carb -1)
stargazer(PepsiCoCarb)
PepsiCoProt<-lm(PepsiCo$Pricing ~ PepsiCo$char3_protein -1)
stargazer(PepsiCoProt)
PepsiCoBsize<-lm(PepsiCo$Pricing ~ PepsiCo$char4_boxsize -1)
stargazer(PepsiCoBsize)
PepsiCoSugarChar<-lm(PepsiCo$Pricing ~ PepsiCo$char5_sugar -1)
stargazer(PepsiCoSugarChar)
PepsiCoGasChar<-lm(PepsiCo$Pricing ~ PepsiCo$gas -1)
stargazer(PepsiCoGasChar)
PepsiCoCornChar<-lm(PepsiCo$Pricing ~ PepsiCo$corn -1)
stargazer(PepsiCoCornChar)
PepsiCoWhtChar<-lm(PepsiCo$Pricing ~ PepsiCo$wheat -1)
stargazer(PepsiCoWhtChar)
PepsiCoSUGcost<-lm(PepsiCo$Pricing ~ PepsiCo$sugar -1)
stargazer(PepsiCoSUGcost)
PepsiCoLABcost<-lm(PepsiCo$Pricing ~ PepsiCo$LaborCost -1)
stargazer(PepsiCoLABcost)

#-------------------------------------------------------------------
# GENERAL MILLS CORRELATION TESTS
#-------------------------------------------------------------------

cor.test(General_Mills$Pricing, General_Mills$Week, alternative = "two.sided")
cor.test(General_Mills$Pricing, General_Mills$UnitSales, alternative = "two.sided")
cor.test(General_Mills$PricePerOZ, General_Mills$Week, alternative = "two.sided")
cor.test(General_Mills$Pricing, General_Mills$char1_calories, alternative = "two.sided")
cor.test(General_Mills$Pricing, General_Mills$char2_carb, alternative = "two.sided")
cor.test(General_Mills$Pricing, General_Mills$char3_protein, alternative = "two.sided")
cor.test(General_Mills$Pricing, General_Mills$char4_boxsize, alternative = "two.sided")
cor.test(General_Mills$Pricing, General_Mills$char5_sugar, alternative = "two.sided")
cor.test(General_Mills$Pricing, General_Mills$gas, alternative = "two.sided")
cor.test(General_Mills$Pricing, General_Mills$corn, alternative = "two.sided")
cor.test(General_Mills$Pricing, General_Mills$wheat, alternative = "two.sided")
cor.test(General_Mills$Pricing, General_Mills$sugar, alternative = "two.sided")
cor.test(General_Mills$Pricing, General_Mills$LaborCost, alternative = "two.sided")
cor.test(General_Mills$UnitSales, General_Mills$Week, alternative = "two.sided")

#-------------------------------------------------------------------
# KELLOGG CORRELATION TESTS
#-------------------------------------------------------------------

cor.test(Kellogg_Co$Pricing, Kellogg_Co$Week, alternative = "two.sided")
cor.test(Kellogg_Co$Pricing, Kellogg_Co$UnitSales, alternative = "two.sided")
cor.test(Kellogg_Co$PricePerOZ, Kellogg_Co$Week, alternative = "two.sided")
cor.test(Kellogg_Co$Pricing, Kellogg_Co$char1_calories, alternative = "two.sided")
cor.test(Kellogg_Co$Pricing, Kellogg_Co$char2_carb, alternative = "two.sided")
cor.test(Kellogg_Co$Pricing, Kellogg_Co$char3_protein, alternative = "two.sided")
cor.test(Kellogg_Co$Pricing, Kellogg_Co$char4_boxsize, alternative = "two.sided")
cor.test(Kellogg_Co$Pricing, Kellogg_Co$char5_sugar, alternative = "two.sided")
cor.test(Kellogg_Co$Pricing, Kellogg_Co$gas, alternative = "two.sided")
cor.test(Kellogg_Co$Pricing, Kellogg_Co$corn, alternative = "two.sided")
cor.test(Kellogg_Co$Pricing, Kellogg_Co$wheat, alternative = "two.sided")
cor.test(Kellogg_Co$Pricing, Kellogg_Co$sugar, alternative = "two.sided")
cor.test(Kellogg_Co$Pricing, Kellogg_Co$LaborCost, alternative = "two.sided")
cor.test(Kellogg_Co$UnitSales, Kellogg_Co$Week, alternative = "two.sided")

#-------------------------------------------------------------------
# PRIVATE LABEL CORRELATION TESTS
#-------------------------------------------------------------------

cor.test(Private_Label$Pricing, Private_Label$Week, alternative = "two.sided")
cor.test(Private_Label$Pricing, Private_Label$UnitSales, alternative = "two.sided")
cor.test(Private_Label$PricePerOZ, Private_Label$Week, alternative = "two.sided")
cor.test(Private_Label$Pricing, Private_Label$char1_calories, alternative = "two.sided")
cor.test(Private_Label$Pricing, Private_Label$char2_carb, alternative = "two.sided")
cor.test(Private_Label$Pricing, Private_Label$char3_protein, alternative = "two.sided")
cor.test(Private_Label$Pricing, Private_Label$char4_boxsize, alternative = "two.sided")
cor.test(Private_Label$Pricing, Private_Label$char5_sugar, alternative = "two.sided")
cor.test(Private_Label$Pricing, Private_Label$gas, alternative = "two.sided")
cor.test(Private_Label$Pricing, Private_Label$corn, alternative = "two.sided")
cor.test(Private_Label$Pricing, Private_Label$wheat, alternative = "two.sided")
cor.test(Private_Label$Pricing, Private_Label$sugar, alternative = "two.sided")
cor.test(Private_Label$Pricing, Private_Label$LaborCost, alternative = "two.sided")
cor.test(Private_Label$UnitSales, Private_Label$Week, alternative = "two.sided")

#-------------------------------------------------------------------
# PEPSI CO CORRELATION TESTS
#-------------------------------------------------------------------

cor.test(PepsiCo$Pricing, PepsiCo$Week, alternative = "two.sided")
cor.test(PepsiCo$Pricing, PepsiCo$UnitSales, alternative = "two.sided")
cor.test(PepsiCo$PricePerOZ, PepsiCo$Week, alternative = "two.sided")
cor.test(PepsiCo$Pricing, PepsiCo$char1_calories, alternative = "two.sided")
cor.test(PepsiCo$Pricing, PepsiCo$char2_carb, alternative = "two.sided")
cor.test(PepsiCo$Pricing, PepsiCo$char3_protein, alternative = "two.sided")
cor.test(PepsiCo$Pricing, PepsiCo$char4_boxsize, alternative = "two.sided")
cor.test(PepsiCo$Pricing, PepsiCo$char5_sugar, alternative = "two.sided")
cor.test(PepsiCo$Pricing, PepsiCo$gas, alternative = "two.sided")
cor.test(PepsiCo$Pricing, PepsiCo$corn, alternative = "two.sided")
cor.test(PepsiCo$Pricing, PepsiCo$wheat, alternative = "two.sided")
cor.test(PepsiCo$Pricing, PepsiCo$sugar, alternative = "two.sided")
cor.test(PepsiCo$Pricing, PepsiCo$LaborCost, alternative = "two.sided")
cor.test(PepsiCo$UnitSales, PepsiCo$Week, alternative = "two.sided")

#-------------------------------------------------------------------
## 6. MAKING ONE CEREAL DATA FRAME OF JUST THE 4 PARENT COMPANIES ##
#-------------------------------------------------------------------

Cereal$PARENT<-NA
Cereal$PARENT[Cereal$AttrParentLevel == "PRIVATE LABEL"]<-1
Cereal$PARENT[Cereal$AttrParentLevel == "KELLOGG CO"]<-1 
Cereal$PARENT[Cereal$AttrParentLevel == "PEPSICO INC"]<-1 
Cereal$PARENT[Cereal$AttrParentLevel == "GENERAL MILLS"]<-1

CEREAL<-filter(Cereal, PARENT == "1")  

dim(CEREAL)

#-------------------------------------------------------------------
# DATA FRAMES OF THE PRICING DETERMINANTS AND CEREAL MAIN TIBBLE
#-------------------------------------------------------------------

CEREALcal<-cbind(CEREAL$Pricing,CEREAL$char1_calories)
CEREALcal<-data.frame(CEREALcal)
colnames(CEREALcal)[1] = 'Price'
colnames(CEREALcal)[2] = 'Calories'

CEREALcarbs<-cbind(CEREAL$Pricing,CEREAL$char2_carbs)
CEREALcarbs<-data.frame(CEREALGas)
colnames(CEREALcarbs)[1] = 'Price'
colnames(CEREALcarbs)[2] = 'Carbohydrates'

CEREALprotein<-cbind(CEREAL$Pricing,CEREAL$char3_protein)
CEREALprotein<-data.frame(CEREALprotein)
colnames(CEREALprotein)[1] = 'Price'
colnames(CEREALprotein)[2] = 'Protein'

CEREALsize<-cbind(CEREAL$Pricing,CEREAL$char4_boxsize)
CEREALsize<-data.frame(CEREALsize)
colnames(CEREALsize)[1] = 'Price'
colnames(CEREALsize)[2] = 'Box Size'

CEREALsugar<-cbind(CEREAL$Pricing,CEREAL$char5_sugar)
CEREALsugar<-data.frame(CEREALsugar)
colnames(CEREALsugar)[1] = 'Price'
colnames(CEREALsugar)[2] = 'Sugar Content'

CEREALcorn<-cbind(CEREAL$Pricing,CEREAL$corn)
CEREALcorn<-data.frame(CEREALcorn)
colnames(CEREALcorn)[1] = 'Price'
colnames(CEREALcorn)[2] = 'Corn Cost'

CEREALGas<-cbind(CEREAL$Pricing,CEREAL$gas)
CEREALGas<-data.frame(CEREALGas)
colnames(CEREALGas)[1] = 'Price'
colnames(CEREALGas)[2] = 'Gas Cost'

CEREALwheat<-cbind(CEREAL$Pricing,CEREAL$wheat)
CEREALwheat<-data.frame(CEREALwheat)
colnames(CEREALwheat)[1] = 'Price'
colnames(CEREALwheat)[2] = 'Wheat Cost'

CEREALSC<-cbind(CEREAL$Pricing,CEREAL$sugar)
CEREALSC<-data.frame(CEREALSC)
colnames(CEREALSC)[1] = 'Price'
colnames(CEREALSC)[2] = 'Sugar Cost'

CEREALele<-cbind(CEREAL$Pricing,CEREAL$electricity)
CEREALele<-data.frame(CEREALele)
colnames(CEREALele)[1] = 'Price'
colnames(CEREALele)[2] = 'Electricity Cost'

CEREALlabor<-cbind(CEREAL$Pricing,CEREAL$LaborCost)
CEREALlabor<-data.frame(CEREALlabor)
colnames(CEREALlabor)[1] = 'Price'
colnames(CEREALlabor)[2] = 'Labor Cost'

CEREALWeek<-cbind(CEREAL$Pricing,CEREAL$Week)
CEREALWeek<-data.frame(CEREALWeek)
colnames(CEREALWeek)[1] = 'Price'
colnames(CEREALWeek)[2] = 'Week'

CEREALPD<-cbind(CEREAL$Pricing,CEREAL$ProductDescription)
CEREALPD<-data.frame(CEREALPD)
colnames(CEREALPD)[1] = 'Price'
colnames(CEREALPD)[2] = 'Product Description'
  
#-------------------------------------------------------------------
################# 7. MAKING PREDICTIVE MODELS ######################
#-------------------------------------------------------------------

#-------------------------------------------------------------------
# MAKING P HAT WITH DUMMIES FOR PRODUCT DESCRIPTION AND TIME
#-------------------------------------------------------------------
# P hat = price determined by costs and product chracteristics

Phat<-lm(CEREAL$Pricing ~ CEREAL$char1_calories + CEREAL$char2_carb + 
         CEREAL$char3_protein + CEREAL$char4_boxsize + CEREAL$char5_sugar +
         CEREAL$corn + CEREAL$gas + CEREAL$wheat + CEREAL$sugar + 
         CEREAL$electricity + CEREAL$LaborCost + CEREAL$Week +
         CEREAL$ProductDescription)
summary(Phat)
stargazer(Phat)

CEREAL$Phat<-NA
CEREAL$Phat<-predict(Phat, data = CEREAL$Pricing)           
               
#-------------------------------------------------------------------
# MAKING THE MARKET SHARE MODEL WITH DUMMIES FOR P.D. AND TIME
#-------------------------------------------------------------------

MODEL<-lm(CEREAL$Yit ~ CEREAL$char1_calories + CEREAL$char2_carb + 
CEREAL$char3_protein + CEREAL$char4_boxsize + CEREAL$char5_sugar +
CEREAL$Phat + CEREAL$Week + CEREAL$ProductDescription)
stargazer(MODEL)

print(MODEL)
# Coefficients:
#          (Intercept)  Cereal$char1_calories      Cereal$char2_carb   Cereal$char3_protein  
#             -4.13624               -0.00292                0.02692               -0.07762  
#  Cereal$char4_boxsize     Cereal$char5_sugar         Cereal$Pricing  
#              0.03211                0.04530               -1.30963 


