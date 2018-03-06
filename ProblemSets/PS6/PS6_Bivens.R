#!/bin/sh
Rbatch

# Data Science PS6
# 3/6/2018
# Clean and visualize data

install.packages("ggthemes")
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
# ^ bringing out ggplot2 for visualizations
#   and other packages for cleaning, converting data file, whatever

cereal<-read_dta("/Users/christin/Downloads/Cereal_PD_03052016.dta",
                 encoding = 'latin1')
# converting STATA file to csv of ready-to-eat cereal market data

head(cereal, n=2)
# viewing header labels

library(dplyr)
library(tidyverse)
Cereal <-as_tibble(cereal)
# converting .csv to tibble format

## below is making tibbles for the 4 biggest cereal parent companies ##
General_Mills<-filter(Cereal, AttrParentLevel =="GENERAL MILLS")
Kellogg_Co<-filter(Cereal, AttrParentLevel =="KELLOGG CO")
PepsiCo<-filter(Cereal, AttrParentLevel =="PEPSICO INC")
Private_Label<-filter(Cereal, AttrParentLevel =="PRIVATE LABEL")

## making data frames for parent companies quantity sold and price

GeneralMillsQuantPrice<-cbind(General_Mills$AvgPriceperUnit,General_Mills$UnitSales)
GeneralMillsQuant<-data.frame(GeneralMillsQuantPrice)
colnames(GeneralMillsQuant)[1] = 'Price'
colnames(GeneralMillsQuant)[2] = 'Quantity'

KelloggQuantPrice<-cbind(Kellogg_Co$AvgPriceperUnit,Kellogg_Co$UnitSales)
KelloggQuant<-data.frame(KelloggQuantPrice)
colnames(KelloggQuant)[1] = 'Price'
colnames(KelloggQuant)[2] = 'Quantity'

PrivateQuantPrice<-cbind(Private_Label$AvgPriceperUnit,Private_Label$UnitSales)
PrivateQuant<-data.frame(PrivateQuantPrice)
colnames(PrivateQuant)[1] = 'Price'
colnames(PrivateQuant)[2] = 'Quantity'

PepsiCoQuantPrice<-cbind(PepsiCo$AvgPriceperUnit,PepsiCo$UnitSales)
PepsiCoQuant<-data.frame(PepsiCoQuantPrice)
colnames(PepsiCoQuant)[1] = 'Price'
colnames(PepsiCoQuant)[2] = 'Quantity'

## making ggplots for each to see if there's any correlation

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
