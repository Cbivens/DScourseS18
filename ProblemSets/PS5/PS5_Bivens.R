#!/bin/sh
Rbatch

# data science ps5 homework, parts 1 and 2
# webscraping non api site and making a table

library(rvest)
library(tidyverse)
# ^ calling out packages for web scraping

web <- read_html("https://en.wikipedia.org/wiki/List_of_grand_couturiers")
web
# brigning in wikipedia page on haute couture fashion

results <- web %>% html_nodes("ul:nth-child(14) a")
results
# getting names of members of 
# the Chambre syndicale de la haute couture

members <-html_text(results)
# converting css data of members to text
head(members)

formem <- web %>% html_nodes("ul:nth-child(18) a")
# foreign members
foreign <-html_text(formem)
head(foreign)

invitedmem <- web %>% html_nodes("ul:nth-child(16) li > a")
# invited members
invited <-html_text(invitedmem)
head(invited)

Native <-c("Y","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y",
           "Y","Y","N","N","N","N","N","N","N","N","N",
           "N","N","N","N","N","N","N","N","N","N","N",
           "N","N","N")
Invited <-c("N","N","N","N","N","N","N","N","N","N",
            "N","N","N","Y","Y","Y","Y","Y","Y","Y",
            "Y","Y","Y","Y","Y","Y","Y","Y","Y","Y",
            "Y","N","N","N","N","N")
Foreign <-c("N","N","N","N","N","N","N","N","N",
           "N","N","N","N","N","N","N","N","N","N","N",
           "N","N","N","N","N","N","N","N","N","N","N",
           "Y","Y","Y","Y","Y")
# ^ assigning Y/N values to each member type 
#   to create a data frame

Design_House <-c("Adeline André","Alexandre Vauthier",
                "Alexis Mabille","Atelier Gustavolins",
                "Bouchra Jarrar","Chanel",
                "Christian Dior","Frank Sorbier",
                "Givenchy","Jean Paul Gaultier",
                "Maurizio Galante","Stéphane Rolland",
                "Yiqing Yin", "Béatrice Demulder Ferrant",
                "Hervé L. LeRoux","Iris van Herpen",           
                "J. Mendel",                
                "Julien Fournié","Maison Rabih Kayrouz",     
                "On Aura Tout Vu","Yanna Samouilov",          
                "Livia Stoianova","Rad Hourani",              
                "Ralph & Russo","Serkan Cura",              
                "Schiaparelli","Yiqing Yin",               
                "Georges Hobeika","Zuhair Murad",             
                "Marco Zanini","Harime Nui",
                "Armani","Elie Saab","Versace",
                "Valentino","Giambattista Valli")
# ^ making vector of design house names

Member_Type <-data.frame(Native, Invited,
                    Foreign, Design_House)
Member_Type
# creating and viewing data frame of types

###### PART TWO: API DATA FRAME


library(quantmod)
# ^ calling out the beast

getSymbols("CDI", src = "google")
# using google finance through quantmod to get info on 
# brand Christian Dior

head(CDI)
# ^ Viewing first 6 records, goes back to 2007
tail(CDI)
# ^ viewing last 6, only as far back as september 12th, 2017
#   end of fiscal year that year

###   also used view for record # reference, but didn't put in 
###   shell script

DIORopen <-c(CDI$CDI.Close[169:175],CDI$CDI.Close[2687:2693])
# ^ making vector of closing numbers for first september 
#   and last semptember on record

DIORclose <-c(CDI$CDI.Open[169:175], CDI$CDI.Open[2687:2693]) 
# ^ making vector of opening #s for first/last septembers on 
#   record

DIOR <-cbind(DIORopen,DIORclose)
# ^ combining into data set

Dior_Septembers <-as.data.frame(DIOR)
View(Dior_Septembers)
# ^ turning data into a data frame and viewing... 
#   poor Dior
