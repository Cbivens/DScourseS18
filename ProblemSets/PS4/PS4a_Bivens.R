# R script for sparkR

sparkR
# ^ going into sparkR from the command line to use API

system('wget -O nflstats.json
 "http://api.fantasy.nfl.com/v1/players/stats?statType=seasonStats&season=2010&week=1&format=json"')
# ^ downloading stats file

library("jsonlite")
# ^ pulling out json package

system('cat nflstats.json')
# ^ printing data to console to view

mydf <- fromJSON('nflstats.json')
# ^ making a data frame from the downloaded data

class(mydf$players)
# ^ checking class of players object in mydf

head(mydf$players)
# ^ listing first n rows of the players dataframe
