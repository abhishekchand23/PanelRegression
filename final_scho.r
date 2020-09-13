library(foreign)
library(car)
library(readxl)
library(ggplot2)
library(gplots)
library(apsrtable)
library(plm)
library(tseries)
library(lmtest)

df = read_excel("scho_data.xlsx", col_names = TRUE)

summary(df)

coplot(Enrolment ~ YEAR|DISTRICT , type ="b", data=df)

plotmeans(Enrolment ~ DISTRICT, main="Heterogeineity across Districts", data=df, bars=TRUE )
plotmeans(Enrolment ~ YEAR,main="Heterogeineity across years", data = df)

#Regular OLS regression does not consider heterogeneity across groups or time
ols4 =lm(Enrolment ~ Scholarship + Altitude + Dist_percap_inc + sc_pop_perc, data = df)
summary(ols4)

ols =lm(Enrolment ~ Scholarship , data = df)
summary(ols)

theme_set(theme_bw())  # pre-set the bw theme.

g <- ggplot(df, aes(Scholarship,Enrolment))
# Scatterplot
g + geom_point() + 
  geom_smooth(method="lm", se=F) +
  labs(subtitle="Does not consider heterogeneity across groups or time ", 
       y="Enrolment", 
       x="Scholarship", 
       title="Regular OLS regression ", 
       caption="Source: UK Gov")

#Fixed effects using Least squares dummy variable model
fixed.dum <-lm(Enrolment ~ Scholarship + factor(DISTRICT) - 1, data=df)
summary(fixed.dum)

yhat <- fixed.dum$fitted
scatterplot(yhat~df$Scholarship|df$DISTRICT, boxplots=FALSE, xlab="Scholarship", ylab="Enrolment",smooth=FALSE)
abline(lm(df$Enrolment~df$Scholarship),lwd=3, col="red")

#Fixed effects: n entity-specific intercepts (using plm)
fixed = plm(Enrolment ~ Scholarship, data= df, index = c("DISTRICT", "YEAR"), model = "within")
summary(fixed)

# Display the fixed effects (constants for each country)
fixef(fixed)
# Testing for fixed effects, null: OLS better than fixed
pFtest(fixed,ols)
#F test reveals that fixed effect is a better choice 

#Random Effects
random = plm(Enrolment ~ Scholarship, data= df, index = c("DISTRICT", "YEAR"), model = "random")
summary(random)

#Hausman Test (if p value lt 0.05 use FE)
phtest(fixed,random)

#Testing for time fixed effects

fixed.time = plm(Enrolment ~ Scholarship +factor(YEAR), data= df, index = c("DISTRICT", "YEAR"), model = "within")
summary(fixed.time)

# Testing time-fixed effects. The null is that no time-fixed effects needed
pFtest(fixed.time,fixed)

plmtest(fixed,c("time"),type = "bp")
#there is time fixed effect


#Testing for random effects: Breusch-Pagan Lagrange multiplier (LM)
 # Regular OLS (pooling model) using plm

pool = plm(Enrolment~Scholarship, data = df, index = c('DISTRICT','YEAR'),model = 'pooling')
summary(pool)

# Breusch-Pagan Lagrange Multiplier for random effects. Null is no panel effect (i.e. OLS better).
plmtest(pool, type=c("bp"))

#Testing for cross-sectional dependence/contemporaneous correlation: 
#using Breusch-Pagan LM test of independence and Pasaran CD test

pcdtest(fixed, test = c("lm"))
pcdtest(fixed, test = c("cd"))

#Testing for serial correlation
pbgtest(fixed)


#Testing for unit roots/stationarity
df.set = pdata.frame(df, index=c('DISTRICT','YEAR'))
df.set  

adf.test(df.set$Enrolment)  

#Testing for heteroskedasticity
bptest(Enrolment~df$Scholarship+factor(df$DISTRICT), data=df, studentize = F)


#Controlling for heteroskedasticity: Random effects  
 coeftest(random) 
 coeftest(random, vcovHC) # Heteroskedasticity consistent coefficients
 coeftest(random, vcovHC(random, type = "HC3")) # Heteroskedasticity consistent coefficients, type 3
 # The following shows the HC standard errors of the coefficients
 t(sapply(c("HC0", "HC1", "HC2", "HC3", "HC4"), function(x) sqrt(diag(vcovHC(random, type = x)))))

 #Controlling for heteroskedasticity: Fixed effects  
 coeftest(fixed) # Original coefficients
 coeftest(fixed, vcovHC) # Heteroskedasticity consistent coefficients 
 coeftest(fixed, vcovHC(fixed, method = "arellano")) # Heteroskedasticity consistent coefficients (Arellano) 
 coeftest(fixed, vcovHC(fixed, type = "HC3")) # Heteroskedasticity consistent coefficients, type 3
 t(sapply(c("HC0", "HC1", "HC2", "HC3", "HC4"), function(x) sqrt(diag(vcovHC(fixed, type = x)))))   

  