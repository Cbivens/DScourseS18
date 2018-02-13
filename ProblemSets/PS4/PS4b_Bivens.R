# R exercise part 2 

sparkR 

df1 <-(iris)
# ^ assigning df1 to load iris data

df <-createDataFrame(df1)
# ^ making data frame

class(df1)
# ^ viewing df1 classification

class(df)
# ^ viewing df classification

head(select(df, df$Sepal_Length, df$Species))
# ^ viewing first 6 rows
 
head(select(df1, df1$Sepal_Length, df1$Species))
# ^ viewing first 6 rows

head(filter(df, df$Sepal_Length>5.5))
# ^ viewing first 6 rows of all columns where sepal length<5.5 

head(filter(df1, df1$Sepal_Length>5.5))
# ^ viewing first 6 rows of all columns where sepal length<5.5

head(select(filter(df, df$Sepal_Length>5.5),
     df$Sepal_Length, df$Species))
# ^ combining previous code into one line

df2 <-head(summarize(groupBy(df, df$Species),
     mean=mean(df$Sepal_Length),
     count=n(df$Sepal_Length)))
# ^ computing avg sepal length, and # of observations, by each of iris species
#   and assigning object name 'df2'

head(arrange(df2, asc(df2$Species)))
# ^ using the arrange function

## df1 cannot take same commands as df due to class differences
##  => command differences
