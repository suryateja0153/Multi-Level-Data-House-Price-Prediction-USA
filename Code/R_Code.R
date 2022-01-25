#Setting the Working Directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#clear environment & plot
rm(list=ls())
dev.off()
options(scipen=999)

#load relavent libraries
library(pacman)
p_load(rio, stargazer,Hmisc, PerformanceAnalytics, tidyverse)

#import main data
df <- import("housingData.xlsx")
names(df) <- stringr::str_to_lower(names(df))
str(df)

#import gdp data
gdp <- read.csv("current_dollar_gdp.csv", header = TRUE, sep = ",")
colnames(gdp)[3:13] <- c("2009":"2019")
gdp$`2009` <- NULL
gdp %>% pivot_longer(col = starts_with("2"), names_to = "Year", values_to = "GDP" ) -> gdp
str(gdp)

names(gdp) <- stringr::str_to_lower(names(gdp))
names <- c("state", "year", "region") 
gdp[,names] <- lapply(gdp[,names],factor)

# merge temp & stores dataframes by store
d <- merge(df, gdp, by=c("year", "state"))

#categorize features, 
names <- c("state", "year") 
d[,names] <- lapply(d[,names],factor)
colnames(d)[7:11] <- c("mort90plus_delinquentrate","mort30to89_delinquentrate", "subsd_units_avail", 
                       "pctsubsd_units_occupied", "numhouse_subsidized")

#check for missing
which(!complete.cases(d))
colSums(is.na(d))


#Data Visualizations
hist(d$avghomeprice)
hist(log(d$avghomeprice))

library(lattice)
histogram(~avghomeprice,data=d,
          type="count",
          xlab="Average Home Price",
          main="Distribution of Avg Home Price")

hist.data.frame(d)

library(purrr)
d %>% keep(is.numeric) %>% pairs(. , panel= panel.smooth)
d %>% keep(is.numeric) %>% chart.Correlation(., histogram=TRUE, pch=19)


#https://liuyanguu.github.io/post/2019/04/17/ggplot-heatmap-us-50-states-map-and-china-province-map/
par(mfrow = c(1,2))

start = levels(d$year)[1]
end = levels(d$year)[10]
library(usmap)
for (i in c(start,end)){
  df = subset(d, year == i)
  map <- plot_usmap(data = df, values = "minwage", labels = T) +
          labs(fill = 'Min Wage') +
          scale_fill_gradientn(colours=hcl.colors(10, rev = TRUE),na.value="grey",
                               guide = guide_colourbar(barwidth = 25, barheight = 0.4)) +
          theme(legend.position = "bottom",
                legend.title=element_text(size=10),
                legend.text=element_text(size=10))
  
  print(map)

}

d %>% subset(d$year == 2019) %>% group_by(region) %>% summarise(mean = mean(avghomeprice)) %>% arrange(mean)
d %>% subset(d$year == 2019) %>% group_by(state) %>% summarise(homeprice = avghomeprice) %>% arrange(homeprice) %>% top_n(5)
d %>% subset(d$year == 2010) %>% group_by(state) %>% summarise(homeprice = avghomeprice) %>% arrange(homeprice) %>% top_n(5)


d %>% subset(d$year == 2010) %>% group_by(state) %>% summarise(homeprice = minwage) %>% arrange(homeprice) %>% top_n(5)
temp <- d %>% subset(d$year == 2019) %>% group_by(state) %>% summarise(homeprice = minwage) %>% arrange(homeprice) %>% top_n(-5)


library(corrplot)
d %>% keep(is.numeric) %>% cor() %>% corrplot(., method = "number", type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
#population: subsd_units_avaial, %sub_units_occupied
# unemploymentrate: mort_30to89

d$numhouse_subsidized <- NULL
d$gdp <- NULL
d$subsd_units_avail <- NULL

with(d, interaction.plot(year,minwage,avghomeprice )) #interaction check


# #standardize the variables using min max if needed
# normalized <- function(x){
#   minimum = min(x)
#   maximum = max(x)
#   
#   (x-minimum)/ (maximum-minimum)
# }
# 
# denormalized <- function(normalized,x){
#   (normalized)*(max(x)-min(x))+min(x)
# }
# 


#Model
library(lme4)
m1 <- lmer(log(avghomeprice) ~ log(medianincome) + unemploymentrate+ mort90plus_delinquentrate+ 
             mort30to89_delinquentrate + pctsubsd_units_occupied + minwage*year + povertyrate + 
             log(population) + (1 | state), data = d, REML=FALSE)
m2 <- lmer(log(avghomeprice) ~ log(medianincome) + minwage*year +unemploymentrate+ mort90plus_delinquentrate+ 
             mort30to89_delinquentrate + pctsubsd_units_occupied+ povertyrate + 
             log(population) + (1 | state), data = d, REML=FALSE)
m3 <- lmer(log(avghomeprice) ~ log(medianincome) + minwage + unemploymentrate+ mort90plus_delinquentrate+ 
             mort30to89_delinquentrate + pctsubsd_units_occupied+ povertyrate + 
             log(population) + (1 | year) + (1 | state), data = d, REML=FALSE)
summary(m1)
summary(m2)
stargazer(m1, m2, m3, type='text', single.row = TRUE)

#State-wise and yearly random effects
dotplot(ranef(m3, condVar=TRUE))
ranef <- ranef(m3)
ranef_df <- as.data.frame(ranef)
ranef_df$term <- NULL
ranef_df$condsd <- NULL
df_out <- split( ranef_df , f = ranef_df$grpvar )
df_state <- df_out$state
df_year <- df_out$year
df_state <- df_state[order(df_state$condval, decreasing = TRUE), ]
df_year <- df_year[order(df_year$condval, decreasing = TRUE), ]
df_state$grpvar <- NULL
df_year$grpvar <- NULL
colnames(df_state) <- c("StateName", "Coefficient")
colnames(df_year) <- c("Year", "Coefficient")
row.names(df_state) <- NULL
row.names(df_year) <- NULL

fixef(m3)
fixef <- fixef(m3)
fixef_df <- as.data.frame(fixef)
temp <- fixef_df$fixef[1]
df_state$betacoeff <- df_state$Coefficient + temp
df_state$Coefficient <- NULL

df_year$betacoeff <- df_year$Coefficient + temp
df_year$Coefficient <- NULL

head(df_state,5)
tail(df_state,5)
df_year


#model assumptions
library(lmtest)
library(car)
library(DHARMa) # for autocorrelation test 

lmer_assumptions <- function(model){
  #' Tests for assumptions
  plot(resid(model) ~ fitted(model))                     # Residual plot for Linearity
  hist(resid(model))
  
  qqnorm(resid(model))                                   # Q-Q plot for normality
  qqline(resid(model), col="red")
  print(shapiro.test(resid(m1)))
  norm <- rnorm(200)
  print(ks.test(norm, resid(model)))                     # Kolmogorov-Smirnov test for normality
  
  
  print(bartlett.test(list(resid(model), fitted(model))))# Bartlett test of homoskedasticity
  
  print(vif(model))                                      # Variance inflation factor
  
  print(testTemporalAutocorrelation(model, time = NULL)) # Autocorrelation 
  
}


lmer_assumptions(m1)
lmer_assumptions(m2)
lmer_assumptions(m3)
summary(m3)
confint(m3)