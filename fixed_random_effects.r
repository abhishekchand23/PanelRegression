library(foreign)
library(car)
Panel = read.dta("http://dss.princeton.edu/training/Panel101.dta")

library(readxl)
df = read_excel("scho_data.xlsx", col_names = TRUE)
df
Panel
coplot(y ~ year|country, type="l",data=Panel)
coplot(y ~ year|country, type="b",data=Panel)
scatterplot(y~year|country, boxplots=FALSE, smooth=TRUE, reg.line=FALSE, data=Panel)

coplot(Enrolment ~ YEAR|DISTRICT , type ="l", data=df)
coplot(Enrolment ~ YEAR|DISTRICT , type ="b", data=df)

library(ggplot2)
ggplot(Panel, aes(x=year, y=y)) + geom_line(aes(col=country))
ggplot(df, aes(x=YEAR, y=Enrolment)) + geom_line(aes(col=DISTRICT))
library(gplots)

#Heterogeneity: unobserved variables that do not change over time
plotmeans(y ~ country, main="Heterogeineity across countries", data=Panel)
plotmeans(Enrolment ~ DISTRICT, main="Heterogeineity across Districts", data=df, bars=TRUE )

warning()
# plotmeans draw a 95% confidence interval around the means
#detach("package:gplots")
# Remove package ‘gplots’ from the workspace
library(gplots)
plotmeans(y ~ year, main="Heterogeineity across years", data=Panel , bars = TRUE)
plotmeans(Enrolment ~ YEAR,main="Heterogeineity across years", data = df)

#Regular OLS regression does not consider heterogeneity across groups or time
ols <-lm(y ~ x1, data=Panel)
summary(ols)
yhat <- ols$fitted

ols =lm(Enrolment ~ Scholarship + Altitude + Dist_percap_inc + sc_pop_perc, data = df)
summary(ols)

library(faraway)
step(lm(Enrolment ~ Scholarship + Altitude + Dist_percap_inc + sc_pop_perc, data = df,direction='backward'))
step(lm(Enrolment ~ Scholarship + Altitude + Dist_percap_inc + sc_pop_perc, data = df,direction='forward'))
step(lm(Enrolment ~ Scholarship + Altitude + Dist_percap_inc + sc_pop_perc, data = df,direction='stepwise'))


theme_set(theme_bw())  # pre-set the bw theme.

g <- ggplot(df, aes(log(Scholarship),log(Enrolment)))
# Scatterplot
g + geom_point() + 
  geom_smooth(method="lm", se=T) +
  labs(subtitle="Does not consider heterogeneity across groups or time ", 
       y="Enrolment", 
       x="Scholarship", 
       title="Regular OLS regression ", 
       caption="Source: UK Gov")

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


theme_set(theme_bw())  # pre-set the bw theme.

g <- ggplot(Panel, aes(x1,y))

# Scatterplot
g + geom_point() + 
  geom_smooth(method="lm", se=T) +
  labs(subtitle="Does not consider heterogeneity across groups or time ", 
       y="y", 
       x="x1", 
       title="Regular OLS regression ", 
       caption="Source: Princeton")

#Fixed effects using Least squares dummy variable model
fixed.dum <-lm(y ~ x1 + factor(country) - 1, data=Panel)
summary(fixed.dum)

yhat <- fixed.dum$fitted
scatterplot(yhat~Panel$x1|Panel$country, boxplots=FALSE, xlab="x1", ylab="yhat",smooth=FALSE)
abline(lm(Panel$y~Panel$x1),lwd=3, col="red")

library(apsrtable)
apsrtable(ols,fixed.dum, model.names = c("OLS", "OLS_DUM"))
cat(apsrtable(ols, fixed.dum, model.names = c("OLS", "OLS_DUM"), Sweave=F), file="ols_fixed1.txt")

fixed.dum <-lm(Enrolment ~ Scholarship + factor(DISTRICT) - 1, data=df)
summary(fixed.dum)

yhat <- fixed.dum$fitted
scatterplot(yhat~df$Scholarship|df$DISTRICT, boxplots=FALSE, xlab="Scholarship", ylab="Enrolment",smooth=FALSE)
abline(lm(df$Enrolment~df$Scholarship),lwd=3, col="red")


library(plm)
#Fixed effects: n entity-specific intercepts (using plm)
fixed = plm(y ~ x1, data= Panel, index = c("country", "year"), model = "within")
summary(fixed)

# Display the fixed effects (constants for each country)
fixef(fixed)
# Testing for fixed effects, null: OLS better than fixed
pFtest(fixed,ols)
#F test reveals that fixed effect is a better choice 

#Fixed effects: n entity-specific intercepts (using plm)
fixed = plm(Enrolment ~ Scholarship, data= df, index = c("DISTRICT", "YEAR"), model = "within")
summary(fixed)

# Display the fixed effects (constants for each country)
fixef(fixed)
# Testing for fixed effects, null: OLS better than fixed
pFtest(fixed,ols)
#F test reveals that fixed effect is a better choice 

#Random Effects
random = plm(y~x1, data=Panel, index = c("country","year"), model = "random")
summary(random)

#Hausman Test
phtest(fixed,random)


#Random Effects
random = plm(Enrolment ~ Scholarship + Altitude, data= df, index = c("DISTRICT", "YEAR"), model = "random")
summary(random)

#Hausman Test
phtest(fixed,random)
