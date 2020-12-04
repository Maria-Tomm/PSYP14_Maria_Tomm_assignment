install.packages("psych")
install.packages("lm.beta")
install.packages("gridExtra")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("car")
install.packages("lmtest")
install.packages("sandwich")
install.packages("boot")
install.packages("lmboot")

coef_table = function(model) {
  require(lm.beta)
  mod_sum = summary(model)
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,
                                                             4], 3))
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values !=
                     "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" &
                                                      mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values !=
                                                                                                            "0" & mod_sum_p_values != "1"]))
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model),
                                                  confint(model), c(0, lm.beta(model)$standardized.coefficients[c(2:length(model$coefficients))])),
                                            2)), mod_sum_p_values)
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta",
                           "p-value")
  mod_sum_table["(Intercept)", "Std.Beta"] = "0"
  return(mod_sum_table)
}

data_sample_1 = read.csv("https://tinyurl.com/ha-dataset1")

#data exploration
summary(data_sample_1)
describe(data_sample_1)
table(data_sample_1$sex)

#visual data exploration - predictors
data_sample_1 %>% ggplot() +aes(x = age) +geom_histogram(bins = 50)
data_sample_1 %>% ggplot() +aes(x = STAI_trait) +geom_histogram(bins = 50)
data_sample_1 %>% ggplot() +aes(x = pain_cat) +geom_histogram(bins = 50)
data_sample_1 %>% ggplot() +aes(x = mindfulness) +geom_histogram(bins = 50)
serumplot <- data_sample_1 %>% ggplot() +aes(x = cortisol_serum) + geom_histogram(bins = 50)
salivaplot <- data_sample_1 %>% ggplot() +aes(x = cortisol_saliva) + geom_histogram(bins = 50)
grid.arrange(serumplot, salivaplot, ncol = 2)

#error correction
data_corrected <- data_sample_1 %>%
  mutate(sex = factor(sex))
data_corrected <- data_sample_1[-c(93, 150),]

ageplot_old <-
  data_sample_1 %>% ggplot() +aes(x = age) +geom_histogram(bins = 50)

ageplot_new <-
  data_corrected %>% ggplot() +aes(x = age) +geom_histogram(bins = 50)
grid.arrange(ageplot_old, ageplot_new, ncol = 2)


STAIplot_old <-
  data_sample_1 %>% ggplot() +aes(x = STAI_trait) +geom_histogram(bins = 50)

STAIplot_new <-
  data_corrected %>% ggplot() +aes(x = STAI_trait) +geom_histogram(bins = 50)
grid.arrange(STAIplot_old, STAIplot_new, ncol = 2)

summary(data_corrected)
describe(data_corrected)

#model_1
model_1 <-lm(pain ~ age + sex, data = data_corrected)

#visual data exploration - model diagnostics
data_corrected %>%ggplot() +aes(x = age, y = pain) +geom_point()
data_corrected %>%ggplot() +aes(x = sex, y = pain) +geom_point()

model_1 %>% ggplot() +aes(x = age, y = pain) +geom_point() +geom_smooth(method = "lm")

#outliers
model_1 %>% plot(which = 4)
wdata_corrected %>% slice(c(100, 128, 141))

#normality of residuals
model_1 %>% plot(which = 2)

model_1 %>% residuals_model_1 = enframe(residuals(model_1)) %>%
residuals_model_1 %>% ggplot() + aes(x = value) + geom_histogram()

describe(residuals(model_1))

#linearity
model_1 %>% residualPlots()

#homoscedasity
model_1 %>% plot(which = 3)

model_1 %>% ncvTest()
model_1  %>% bptest()

#multicolinarity
model_1 %>% vif()



#model_2
model_2 <-lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva, data = data_corrected)

#visual data exploration - model diagnostics
data_corrected %>% ggplot() +aes(x = STAI_trait, y = pain) + geom_point()
data_corrected %>% ggplot() +aes(x = pain_cat, y = pain) + geom_point()
data_corrected %>% ggplot() +aes(x = mindfulness, y = pain) + geom_point()
data_corrected %>% ggplot() +aes(x = cortisol_serum, y = pain) + geom_point()
data_corrected %>% ggplot() +aes(x = cortisol_saliva, y = pain) + geom_point()

model_2 %>% ggplot() +aes(x = STAI_trait, y = pain) + geom_point() + geom_smooth(method = "lm")
model_2 %>% ggplot() +aes(x = pain_cat, y = pain) + geom_point() + geom_smooth(method = "lm")
model_2 %>% ggplot() +aes(x = mindfulness, y = pain) + geom_point() + geom_smooth(method = "lm")
model_2 %>% ggplot() +aes(x = cortisol_serum, y = pain) + geom_point() + geom_smooth(method = "lm")
model_2 %>% ggplot() +aes(x = cortisol_saliva, y = pain) + geom_point() + geom_smooth(method = "lm")

#outliers
model_2 %>% plot(which = 4)
data_corrected %>% slice(c(68, 100, 114))


#normality of residuals
model_2 %>% plot(which = 2)

model_2 %>%
residuals_model_2 = enframe(residuals(model_2)) %>%
residuals_model_2 %>% ggplot() + aes(x = value) + geom_histogram()

describe(residuals(model_2))

#linearity
model_2 %>% residualPlots()

#homoscedasticity
model_2 %>% plot(which = 3)

model_2 %>% ncvTest()
model_2 %>% bptest()

#multicollinearity
model_2 %>% vif()

#model_2final (without cortisol_saliva)
model_2final <-lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data_corrected)

#normality of residuals
model_2final %>% plot(which = 2)

model_2final %>%
residuals_model_2final = enframe(residuals(model_2final)) %>%
residuals_model_2final %>% ggplot() + aes(x = value) + geom_histogram()

describe(residuals(model_2final))

#linearity
model_2final %>% residualPlots()

#homoscedasticity
model_2final %>% plot(which = 3)

model_2final %>% ncvTest()
model_2final %>% bptest()

#multicollinearity
model_2final %>% vif()

#test statistics model_1
sm = summary(model_1)
sm
AIC(model_1)

coef_table(model_1)

#test statistics model_2final
sm2 = summary(model_2final)
sm2
AIC(model_2final)

coef_table(model_2final)

# model_1 & model_2final
anova(model_1, model_2final)

