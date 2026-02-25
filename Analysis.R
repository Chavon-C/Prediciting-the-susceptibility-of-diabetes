pacman::p_load(ggplot2, pROC, mfx)

load("diabetes.Rda")

diabetes$diabetes<-ifelse(diabetes$diabetes=="Yes",1,0)

set.seed(1)
training.rows <- sample(nrow(diabetes),(nrow(diabetes)/2))
training.data <- diabetes[training.rows,]
test.data <- diabetes[-training.rows,]
  
#model 1
#model with all variables
mod1<-glm(formula = diabetes ~ n_preg + glucose + bp + skin + bmi + heriditary + age, family = binomial(link = "logit"), data = training.data)
summary(mod1)

#model 2
#these were the significant variables from the full model
mod2<-glm(formula = diabetes ~ n_preg + glucose + bmi + heriditary , family = binomial(link = "logit"), data = training.data)
summary(mod2)

#model 3
#genetics play a big part, so i kept the ones from mod2 that were related to genetics
mod3<-glm(formula = diabetes ~ glucose + bmi + heriditary, family = binomial(link = "logit"), data = training.data)
summary(mod3)

#model 1 matrix and roc
logit.probs.test1 <- predict(mod1, test.data, type="response")
logit.preds.test1 <- ifelse(logit.probs.test1>0.4,1,0)

x1 <- table(logit.preds.test1, test.data$diabetes)
round((x1[1,2]+x1[2,1])/nrow(test.data)*100,2) # Error rate.
round(x1[2,2]/(x1[1,2]+x1[2,2])*100,2) # Sensitivity
round(x1[1,1]/(x1[1,1]+x1[2,1])*100,2) # Specificity

rocplot1 <- roc(test.data$diabetes, logit.probs.test1)
rocplot1$auc
plot(rocplot1,legacy.axes=T)

#model 2 matrix and roc
logit.probs.test2 <- predict(mod2, test.data, type="response")
logit.preds.test2 <- ifelse(logit.probs.test2>0.4,1,0)

x2 <- table(logit.preds.test2, test.data$diabetes)
round((x2[1,2]+x2[2,1])/nrow(test.data)*100,2) # Error rate. 
round(x2[2,2]/(x2[1,2]+x2[2,2])*100,2) # Sensitivity
round(x2[1,1]/(x2[1,1]+x2[2,1])*100,2) # Specificity

rocplot2 <- roc(test.data$diabetes, logit.probs.test2)
rocplot2$auc
plot(rocplot2,legacy.axes=T)

#model 3 matrix and roc
logit.probs.test3 <- predict(mod3, test.data, type="response")
logit.preds.test3 <- ifelse(logit.probs.test3>0.4,1,0)

x3 <- table(logit.preds.test3, test.data$diabetes)
round((x3[1,2]+x3[2,1])/nrow(test.data)*100,2) # Error rate.
round(x3[2,2]/(x3[1,2]+x3[2,2])*100,2) # Sensitivity
round(x3[1,1]/(x3[1,1]+x3[2,1])*100,2) # Specificity

rocplot3 <- roc(test.data$diabetes, logit.probs.test3)
rocplot3$auc
plot(rocplot3,legacy.axes=T)


#95% confidence interval plot

plotdata<- data.frame(
  "names"= as.factor(c("No. of Pregancies", "Glucose", "BMI", "Heriditary")),
  "coef"= coef(mod2)[2:5],
  "lower" = confint(mod2)[2:5,1],
  "upper" = confint(mod2)[2:5,2]
)

ggplot(plotdata, aes(x=coef, y=names)) +
  geom_point(col="red", size=3) +
  geom_errorbar(aes(xmin=lower, xmax=upper),
                col="red", width =0.1, size=1) +
  geom_vline(xintercept = 0)+
  xlim(-4.5,4.5)+
  theme_bw() +
  xlab("Estimated Coefficient") +
  ylab("") +
  ggtitle("Estimated effects of different varaibles on diabetes
(Coefficients and 95% Confidence Intervals)") +
  theme(axis.text = element_text(size=10),
        plot.title = element_text(size=12))

#Average marginal effects
ame<- logitmfx(mod2, atmean=F, data=diabetes) #ME

