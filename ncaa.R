setwd("C:/Users/chris_kopel/Documents/Pers/Misc/DS/NCAA")
df2016 <- read.csv("TestStats2016.csv", stringsAsFactors = F)
df2017 <- read.csv("TestStats2017.csv", stringsAsFactors = F)

#transpose/wrangle/clean/compile:
library(data.table)
df16 <- as.data.frame(t(df2016))
colnames(df16) <- as.character(unlist(df16[1,]))
df16 <- df16[-1,]
df16 <- setDT(df16, keep.rownames = T)[]
colnames(df16)[1] <- "Team"
df16$Team <- paste0("16", df16$Team)
df16$Year <- 2016


df17 <- as.data.frame(t(df2017))
colnames(df17) <- as.character(unlist(df17[1,]))
df17 <- df17[-1,]
df17 <- setDT(df17, keep.rownames = T)[]
colnames(df17)[1] <- "Team"
df17$Team <- paste0("17", df17$Team)
df17$Year <- 2017
df17 <- head(df17, -1)

dfTotal <- rbind(df16,df17)

#save for later
write.csv(df16, "df16.csv", row.names = F)
write.csv(df17, "df17.csv", row.names = F)
write.csv(dfTotal, "dfTotal.csv", row.names = F)

#begin here
setwd("C:/Users/chris_kopel/Documents/Pers/Misc/DS/NCAA")
df16 <- read.csv("df16.csv", stringsAsFactors = F)
df17 <- read.csv("df17.csv", stringsAsFactors = F)
dfTotal <- read.csv("dfTotal.csv", stringsAsFactors = F)

#configure Ratings appropriately
dfTotal$Rating <- ifelse(dfTotal$Year == 2016, dfTotal$Overall.Rating..2016.Eq., 
                                               dfTotal$Overall.Rating..2017.Eq.)

###analysis
library(ggplot2)
colnames(dfTotal)
ggplot(dfTotal, aes(Rating, Top.25.Wins)) + 
  geom_point()
#no discernable relationship

#test regression (simple)
fit1 <- lm(Rating ~ Wins, data=dfTotal)
summary(fit1)

#now more complex
colnames(dfTotal)
str(dfTotal)
#let's look at per game metrics (Feature Engineering)
dfTotal$FGPG <- dfTotal$FGM/dfTotal$Total.Games
dfTotal$FTPG <- dfTotal$FTM/dfTotal$Total.Games


fit2 <- lm(Rating ~ FGPG + FTPG + Clutch.Wins + Top.25.Win.Percentage + Field.Goal.Percentage +
                    Free.Throw.Percentage + Three.Point.Percentage + Offensive.Rebounds.Per.Game +
                    Defensive.Rebounds.Per.Game + Assists.Per.Game + Turnovers.Per.Game + Steals.Per.Game +
                    Blocks.Per.Game + Wins + Win.Percentage + Top.25.Teams, data = dfTotal)
summary(fit2)







