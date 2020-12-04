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
data_sample_1 = read.csv("https://tinyurl.com/ha-dataset1")

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



#data and new model diagnostics
summary(data_sample_1)
describe(data_sample_1)

data_sample_1 %>% ggplot() +aes(x = age) +geom_histogram(bins = 50)
data_sample_1 %>% ggplot() +aes(x = STAI_trait) +geom_histogram(bins = 50)
data_sample_1 %>% ggplot() +aes(x = pain_cat) +geom_histogram(bins = 50)
data_sample_1 %>% ggplot() +aes(x = mindfulness) +geom_histogram(bins = 50)
data_sample_1 %>% ggplot() +aes(x = weight) +geom_histogram(bins = 50)
data_sample_1 %>% ggplot() +aes(x = IQ) +geom_histogram(bins = 50)
data_sample_1 %>% ggplot() +aes(x = household_income) +geom_histogram(bins = 50)

data_corrected <- data_sample_1 %>%
  mutate(sex = factor(sex))
data_corrected <- data_sample_1[-c(93, 150),]

summary(data_corrected)
describe(data_corrected)

#new model + diagnostics
new_model <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + weight + IQ + household_income, data = data_corrected)

#visual data exploration - model diagnostics
data_corrected %>% ggplot() +aes(x = STAI_trait, y = pain) +geom_point()
data_corrected %>% ggplot() +aes(x = pain_cat, y = pain) +geom_point()
data_corrected %>% ggplot() +aes(x = mindfulness, y = pain) +geom_point()
data_corrected %>% ggplot() +aes(x = cortisol_serum, y = pain) +geom_point()
data_corrected %>% ggplot() +aes(x = weight, y = pain) +geom_point()
data_corrected %>% ggplot() +aes(x = IQ, y = pain) +geom_point()
data_corrected %>% ggplot() +aes(x = household_income, y = pain) +geom_point()

new_model %>% ggplot() +aes(x = STAI_trait, y = pain) +geom_point() +geom_smooth(method = "lm")
new_model %>% ggplot() +aes(x = pain_cat, y = pain) +geom_point() +geom_smooth(method = "lm")
new_model %>% ggplot() +aes(x = mindfulness, y = pain) +geom_point() +geom_smooth(method = "lm")
new_model %>% ggplot() +aes(x = cortisol_serum, y = pain) +geom_point() +geom_smooth(method = "lm")
new_model %>% ggplot() +aes(x = weight, y = pain) +geom_point() +geom_smooth(method = "lm")
new_model %>% ggplot() +aes(x = IQ, y = pain) +geom_point() +geom_smooth(method = "lm")
new_model %>% ggplot() +aes(x = household_income, y = pain) +geom_point() +geom_smooth(method = "lm")


new_model %>% plot(which = 4)
data_corrected %>% slice(c(3, 103, 114))


#normality of residuals
new_model %>% plot(which = 2)

new_model %>%
residuals_new_model = enframe(residuals(new_model)) %>%
residuals_new_model %>% ggplot() + aes(x = value) + geom_histogram()

describe(residuals(new_model))

#linearity
new_model %>% residualPlots()

#homoscedasity
new_model %>% plot(which = 3)

new_model %>% ncvTest()
new_model %>% bptest()

#multicolinearity
new_model %>% vif()



#running backwards regression
backwards_model <- step(new_model, direction = "backward")

#theorybased model
theorybased_model <-lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data_corrected)

#test statistics new_model
sm0 = summary(new_model)
sm0

#test statistics backwards_model
sm = summary(backwards_model)
sm

coef_table(backwards_model)

#test statistics theorybased_model
sm2 = summary(theorybased_model)
sm2

coef_table(theorybased_model)

#compairing AIC
AIC(new_model)
AIC(backwards_model)
AIC(theorybased_model)

#new dataset, compairing predicted with actual pain ratings
data_sample_2 = read.csv("https://tinyurl.com/ha-dataset2")

pred_test = predict(theorybased_model, data_sample_2)
pred_test_back = predict(backwards_model, data_sample_2)

#calculate the sum of squared residuals
RSS_test = sum((data_sample_2[, "pain"] - pred_test)^2)
RSS_test_back = sum((data_sample_2[, "pain"] - pred_test_back)^2)
RSS_test
RSS_test_back


