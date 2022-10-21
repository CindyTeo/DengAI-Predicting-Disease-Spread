library(dplyr)
library(MASS)
library(taRifx)

rm(list=ls())

features.train=read.csv("dengue_features_train.csv", header=TRUE,sep=",")
features.test=read.csv("dengue_features_test.csv", header=TRUE,sep=",")
labels.train=read.csv("dengue_labels_train.csv", header=TRUE,sep=",")
labels.test=read.csv("submission_format.csv", header=TRUE,sep=",")

features.train = within(features.train, {
  month = as.factor(month)
  rm(week_start_date)
  rm(reanalysis_sat_precip_amt_mm)
})

features.test = within(features.test, {
  month = as.factor(month)
  rm(week_start_date)
  rm(reanalysis_sat_precip_amt_mm)
})

features.train.sj = features.train[features.train$city=="sj", ]
features.train.iq = features.train[features.train$city=="iq", ]

features.test.sj = features.test[features.test$city=="sj", ]
features.test.iq = features.test[features.test$city=="iq", ]

features.train.sj = within(features.train.sj, rm(city))
features.train.iq = within(features.train.iq, rm(city))
features.test.sj = within(features.test.sj, rm(city))
features.test.iq = within(features.test.iq, rm(city))

features.train.sj.new = data.frame(sapply(features.train.sj,
                                          function(x) ifelse(is.na(x), mean(x, na.rm=TRUE), x)))
features.train.iq.new = data.frame(sapply(features.train.iq,
                                          function(x) ifelse(is.na(x), mean(x, na.rm=TRUE), x)))
features.test.sj.new = data.frame(sapply(features.test.sj,
                                         function(x) ifelse(is.na(x), mean(x, na.rm=TRUE), x)))
features.test.iq.new = data.frame(sapply(features.test.iq,
                                         function(x) ifelse(is.na(x), mean(x, na.rm=TRUE), x)))

features.train.sj.new = within(features.train.sj.new, {month = as.factor(month)})
features.train.iq.new = within(features.train.iq.new, {month = as.factor(month)})
features.test.sj.new = within(features.test.sj.new, {month = as.factor(month)})
features.test.iq.new = within(features.test.iq.new, {month = as.factor(month)})

#MODEL POISSON
po.model.sj = glm(total_cases ~ . - year - weekofyear, family=poisson(link="log"),  data = features.train.sj.new)
summary(po.model.sj)

po.model.iq = glm(total_cases ~ . - year - weekofyear, family=poisson(link="log"),data = features.train.iq.new)
summary(po.model.iq)

features.train.sj.new$prediction_po = as.integer(predict(po.model.sj, type="response"))
features.train.iq.new$prediction_po = as.integer(predict(po.model.iq, type="response"))

features.test.sj.new$total_cases_po = as.integer(predict(po.model.sj, newdata=features.test.sj.new, type="response"))
features.test.sj.new$total_cases2_po = round(predict(po.model.sj, newdata=features.test.sj.new, type="response"), digits = 0)

features.test.iq.new$total_cases_po = as.integer(predict(po.model.iq, newdata=features.test.iq.new, type="response"))
features.test.iq.new$total_cases2_po = round(predict(po.model.iq, newdata=features.test.iq.new, type="response"), digits = 0)

View(features.train)

#MODEL POISSON BACKWARD SELECTION
po.step.model.sj = stepAIC(po.model.sj, direction = "backward", trace=FALSE)
summary(po.step.model.sj)

po.step.model.iq = stepAIC(po.model.iq, direction = "backward", trace=FALSE)
summary(po.step.model.iq)

features.train.sj.new$prediction_back_po = as.integer(predict(po.step.model.sj, type="response"))
features.train.iq.new$prediction_back_po = as.integer(predict(po.step.model.iq, type="response"))

features.test.sj.new$total_cases_back_po = as.integer(predict(po.step.model.sj, newdata=features.test.sj.new, type="response"))

features.test.iq.new$total_cases_back_po = as.integer(predict(po.step.model.iq, newdata=features.test.iq.new, type="response"))

write.csv(features.test.sj.new, "file:///Users/cindyteo/Desktop/SEMESTER 9/kapita selekta/laporan 2/hasil_sj.csv")
write.csv(features.test.iq.new, "file:///Users/cindyteo/Desktop/SEMESTER 9/kapita selekta/laporan 2/hasil_iq.csv")

#MODEL NEGATIVE BINOMIAL
nb.model.sj = glm.nb(total_cases ~ . - year - weekofyear, data = features.train.sj.new)
summary(nb.model.sj)

nb.model.iq = glm.nb(total_cases ~ . - year - weekofyear, data = features.train.iq.new)
summary(nb.model.iq)

features.train.sj.new$prediction = as.integer(predict(nb.model.sj, type="response"))
features.train.iq.new$prediction = as.integer(predict(nb.model.iq, type="response"))

features.test.sj.new$total_cases = as.integer(predict(nb.model.sj, newdata=features.test.sj.new, type="response"))
features.test.iq.new$total_cases = as.integer(predict(nb.model.iq, newdata=features.test.iq.new, type="response"))

write.csv(features.test.sj.new, "D:\\Documents\\Kuliah\\Kapita Selekta\\Tugas 2\\hasil_sj_nb.csv")
write.csv(features.test.iq.new, "D:\\Documents\\Kuliah\\Kapita Selekta\\Tugas 2\\hasil_iq_nb.csv")

#MODEL NEGATIVE BINOMIAL BACKWARD SELECTION
nb.step.model.sj = stepAIC(nb.model.sj, direction = "backward", trace=FALSE)
summary(nb.step.model.sj)

nb.step.model.iq = stepAIC(nb.model.iq, direction = "backward", trace=FALSE)
summary(nb.step.model.iq)

features.train.sj.new$prediction_back_nb = as.integer(predict(nb.step.model.sj, type="response"))
features.train.iq.new$prediction_back_nb = as.integer(predict(nb.step.model.iq, type="response"))

features.test.sj.new$total_cases_back_nb = as.integer(predict(nb.step.model.sj, newdata=features.test.sj.new, type="response"))

features.test.iq.new$total_cases_back_nb = as.integer(predict(nb.step.model.iq, newdata=features.test.iq.new, type="response"))
write.csv(features.test.sj.new, "file:///Users/cindyteo/Desktop/SEMESTER 9/kapita selekta/laporan 2/hasil_sj_nb_b.csv")
write.csv(features.test.iq.new, "file:///Users/cindyteo/Desktop/SEMESTER 9/kapita selekta/laporan 2/hasil_iq_nb_b.csv")


