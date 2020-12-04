install.packages(psych) 
install.packages(tidyverse) 
install.packages("cAIC4") 
install.packages("r2glmm") 
install.packages("lme4") 
install.packages("lmerTest") 
install.packages("MuMIn")
install.packages("optimx") 

stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object,"y"))
  sdx <- apply(getME(object,"X"), 2, sd)
  sc <- fixef(object)*sdx/sdy
  se.fixef <- coef(summary(object))[,"Std. Error"]
  se <- se.fixef*sdx/sdy
  return(data.frame(stdcoef=sc, stdse=se))
}

data_sample_3 <- read.csv("https://tinyurl.com/ha-dataset3")
data_sample_4 <- read.csv("https://tinyurl.com/ha-dataset4")

#data exploration
describe(data_sample_3)
summary(data_sample_3)

data_sample_3 %>% ggplot() +aes(x = age) + geom_histogram(bins = 50)
data_sample_3 %>% ggplot() +aes(x = STAI_trait) + geom_histogram(bins = 50)
data_sample_3 %>% ggplot() +aes(x = pain_cat) + geom_histogram(bins = 50)
data_sample_3 %>% ggplot() +aes(x = mindfulness) + geom_histogram(bins = 50)
data_sample_3 %>% ggplot() +aes(x = IQ) + geom_histogram(bins = 50)

#building random intercept model
data_sample_3 %>%
  mutate(hospital = factor(hospital))
randomINT_model = lmer(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + (1|hospital), data = data_sample_3)

#model exploration
data_sample_3 %>% ggplot() + aes(y = pain, x = age) + geom_point(aes(color = hospital), size = 4) + geom_smooth(method = "lm", se = F)


# model coefficients and CI
stdCoef.merMod(randomINT_model)
confint(randomINT_model)

#marginal and conditional R^2
summary(randomINT_model)

r2beta(randomINT_model, method = "nsj", data = data_sample_3)

r.squaredGLMM(randomINT_model)

#variance on datasample 4
pred_test = predict(randomINT_model, data_sample_4, allow.new.levels = TRUE)
pred_test

#calculate the sum of squared residuals
RSS_randomINT = sum((data_sample_4[, "pain"] - pred_test)^2)
RSS_randomINT

mod_mean = lm(pain~1, data = data_sample_4)
TSS =sum((data_sample_4$pain- predict(mod_mean))^2)
TSS
R2 = 1-(RSS_randomINT/TSS)
R2

#new linear mixed model with both int and slope
randomINT_SLO_model = lmer(pain ~ cortisol_serum + (cortisol_serum|hospital), data = data_sample_3)
pred_INT_SLO = predict(randomINT_SLO_model)

#visualisation
data_sample_3 %>% ggplot() +
aes(y = pain, x = cortisol_serum, group = hospital)+
geom_point(aes(color = hospital), size = 4) +
geom_line(color='red', aes(y=pred_INT_SLO, x=cortisol_serum))+
facet_wrap( ~ hospital, ncol = 2)