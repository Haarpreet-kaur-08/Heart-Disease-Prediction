library(dplyr)
library(caret)
library(ROCR)
library(ROSE)
library(fastDummies)
library(mlbench)
library(Hmisc)
library(randomForest)
library(rpart)

df <- read.csv("Heart Disease Prediction.csv")

#dimension of dataset
dim(df)

#structure of dataset
str(df)

#summary of dataset
library(psych)
a <- psych::describe(df)
b <- data.frame(a)
View(b)

#missing value check
missing <- sapply(df,function(x)sum(is.na(x)))
data_missing <- data.frame(missing)
View(t(data_missing))

#duplicate check
any(duplicated(df))

#inserting median in numerical columns
library(dplyr)
df<- df %>% dplyr::mutate(BMI = ifelse(is.na(BMI),
                                   median(BMI, na.rm = T),
                                   BMI)) %>%
           mutate(glucose = ifelse(is.na(glucose),
                                   median(glucose, na.rm = T),
                                   glucose)) %>%
           mutate(heartRate = ifelse(is.na(heartRate),
                                       median(heartRate, na.rm = T),
                                     heartRate)) %>%
          mutate(totChol = ifelse(is.na(totChol),
                            median(totChol, na.rm = T),
                          totChol)) %>%
          mutate(cigsPerDay = ifelse(is.na(cigsPerDay),
                          median(cigsPerDay, na.rm = T),
                          cigsPerDay))
colSums(is.na(df))


#education is categorical variable so omiting categorical na values

df<- na.omit(df)
View(df)
colSums(is.na(df))

#changed gender name
names(df)[names(df) == "male"] <- "gender"
names(df)

#converted categorical dataset into factor
df$gender <- as.factor(df$gender)
df$education <- as.factor(df$education)
df$currentSmoker <- as.factor(df$currentSmoker)
df$BPMeds <- as.factor(df$BPMeds)
df$prevalentStroke <- as.factor(df$prevalentStroke)
df$prevalentHyp <- as.factor(df$prevalentHyp)
df$TenYearCHD <- as.factor(df$TenYearCHD)
df$diabetes <- as.factor(df$diabetes)
df$heartRate <- as.numeric(df$heartRate)
df$age <- as.numeric(df$age)
df$cigsPerDay <- as.numeric(df$cigsPerDay)
str(df)
dim(df)

#outlier detection
#Boxplots for numeric data
library(plotly)
ggplotly(ggplot(data=df, aes(y = BMI)) +
           geom_boxplot())
ggplotly(ggplot(data=df, aes(y = age)) +
           geom_boxplot())
ggplotly(ggplot(data=df, aes(y = heartRate)) +
           geom_boxplot())
ggplotly(ggplot(data=df, aes(y = glucose)) +
           geom_boxplot())
ggplotly(ggplot(data=df, aes(y = diaBP)) +
           geom_boxplot())
ggplotly(ggplot(data=df, aes(y = sysBP)) +
           geom_boxplot())
ggplotly(ggplot(data=df, aes(y = totChol)) +
           geom_boxplot())
ggplotly(ggplot(data=df, aes(y = cigsPerDay)) +
           geom_boxplot())

#Categorical data set plot
library(plotly)
View(df)

ggplotly(ggplot(df, aes(x = gender)) +
           geom_bar(aes(col=gender)))
ggplotly(ggplot(df, aes(x = TenYearCHD)) +
           geom_bar(aes(col=TenYearCHD)))
ggplotly(ggplot(df, aes(x = diabetes)) +
           geom_bar(aes(col=diabetes)))
ggplotly(ggplot(df, aes(x = BPMeds)) +
           geom_bar(aes(col=BPMeds)))
ggplotly(ggplot(df, aes(x = education)) +
           geom_bar(aes(col=education)))
ggplotly(ggplot(df, aes(x = currentSmoker)) +
           geom_bar(aes(col=currentSmoker)))
ggplotly(ggplot(df, aes(x = prevalentHyp)) +
           geom_bar(aes(col=prevalentHyp)))
ggplotly(ggplot(df, aes(x = prevalentStroke)) +
           geom_bar(aes(col=prevalentStroke)))

#correlation plot
pc1 <- df %>% select(c(age, cigsPerDay,totChol,sysBP,diaBP, BMI, heartRate,glucose))
library(ggcorrplot)
corr <- round(cor(pc1,method = "pearson"), 2)
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_col = "black",
           outline.color = "white",
           colors = c("skyblue",  "white","red"),
           lab_size = 3, 
           method="square", 
           show.legend = TRUE, legend.title = "correlation", 
           title="Correlation Plot", 
           ggtheme=theme_bw)

#categorical
idx <- 1
for(i in 1:(ncol(df)-1)) {
  for(j in (i+1):ncol(df)-1) {
    selected_condition <- !(is.numeric(df[,i]) | is.numeric(df[,j]) | i == j)
    if (selected_condition) {
      col_i <- df[,i]
      col_j <- df[,j]
      contingency <- table(col_i,col_j)
      chi <- suppressWarnings(chisq.test(contingency)$p.value)
      if(chi<0.05){
        ith_col <- colnames(df)[i]
        jth_col <- colnames(df)[j]
        print(paste(idx,". ", ith_col,' and', jth_col, ' are ', 'Dependent'))
        idx <- idx + 1
      }
    }
  }
}


#t-test 

t.test(age ~ TenYearCHD, data = df) #Significant p-value------include "age"
t.test(cigsPerDay ~ TenYearCHD, data = df)
t.test(totChol ~ TenYearCHD, data = df)
t.test(sysBP ~ TenYearCHD, data = df)
t.test(diaBP ~ TenYearCHD, data = df)
t.test(BMI ~ TenYearCHD, data = df)
t.test(age ~ TenYearCHD, data = df)
t.test(heartRate ~ TenYearCHD, data = df)
t.test(glucose ~ TenYearCHD, data = df)

chisq.test(df$TenYearCHD, df$currentSmoker) #not include current smoker
chisq.test(df$TenYearCHD, df$BPMeds) #npt include BPMeds as no interaction is shown

model_a <- glm(TenYearCHD ~.-heartRate -BPMeds - currentSmoker , family = "binomial", data=df)
summary(model_a)
library(car)
vif(model_a)

library(interactions)
fit1 <- glm(data = df, formula = TenYearCHD ~ gender*prevalentStroke, family = 'binomial')
cat_plot(fit1,pred = prevalentStroke, modx = gender, geom = 'line')

fit2 <- glm(data = df, formula = TenYearCHD ~ gender*BPMeds, family = 'binomial')
cat_plot(fit2,pred = BPMeds, modx = gender, geom = 'line')

fit3 <- glm(data = df, formula = TenYearCHD ~ gender * diabetes, family = 'binomial')
cat_plot(fit3,pred = diabetes, modx = gender, geom = 'line')



#splitting data
#Splitting the dataset
split <- floor(0.8* nrow(df))
set.seed(123)
train_test <- sample(seq_len(nrow(df)), size = split)

training <- df[train_test, ]
testing <- df[-train_test, ]
dim(training)
dim(testing)


##################Models
#without interaction
model1 <- glm(TenYearCHD ~ .-heartRate-BPMeds - currentSmoker, family = "binomial", data=training)
#BPMeds is not useful for the model
summary(model1)
vif(model1)

model2 <- step(model1,direction="backward")
summary(model2)
vif(model2)
#With interaction

model3 <- glm(TenYearCHD ~ .-heartRate-BPMeds - currentSmoker+ age*BMI+gender*prevalentStroke +
                cigsPerDay*diaBP+prevalentHyp*totChol+totChol*sysBP+sysBP*diaBP+sysBP*BMI, family = "binomial", data=training)

model4 <- step(model3,direction="backward")
summary(model4)



#model comparison
anova(model2, model4, test = "LRT")


#confusion matrix
#without interaction
library(caret)
p1<- predict(model2, testing, type='response')
pred1<- ifelse(p1>0.5, 1, 0)
i1<- table(predicted=pred1, Actual=testing$TenYearCHD)
i1

1-sum(diag(i1))/sum(i1)


#sensitivity and specificity for testing
sensitivity(i1)
specificity(i1)

library(caret)
threshold=0.5
predicted_values<-as.factor(ifelse(predict(model2, testing, type="response")>threshold,1,0))
predicted_values
confusionMatrix(predicted_values,testing$TenYearCHD)

#with interaction


threshold=0.5
predicted_values<-as.factor(ifelse(predict(model4, testing, type="response")>threshold,1,0))
predicted_values
confusionMatrix(predicted_values,testing$TenYearCHD)

p2<- predict(model4, testing, type='response')
pred2<- ifelse(p2>0.5, 1, 0)
i2<- table(predicted=pred2, Actual=testing$TenYearCHD)
i2

1-sum(diag(i2))/sum(i2)

#sensitivity and specificity for model4 testing
library(caret)
sensitivity(i2)
specificity(i2)


# ROC curve


library(yardstick)
library(broom)

options(yardstick.event_first = FALSE) 

a <- model2 %>% 
  augment() %>% 
  mutate(d="model2") 

b <- model4 %>% 
  augment() %>% 
  mutate(d="model4") 


c <- full_join(a,b)



options(repr.plot.width = 20, repr.plot.height = 10)

c %>% group_by(d) %>% roc_curve(truth=as.factor(TenYearCHD),.fitted) %>%
  ggplot(
    aes(
      x = 1 - specificity, 
      y = sensitivity, 
      color=d
    )
  ) + 
  geom_path() +
  geom_abline(lty = 3) +
  ylab("Sensitivity") +
  xlab("False Positive Rate") +
  ggtitle("ROC curve with and without interaction") +
  theme_bw() +
  labs(color="Models")
  

do.call("rbind",list(augment(model2) %>% roc_auc(truth=as.factor(TenYearCHD),.fitted) %>% mutate(model="Model2"),
                     augment(model4) %>% roc_auc(truth=as.factor(TenYearCHD),.fitted) %>% mutate(model="model4"))) %>% 
  relocate(model) %>% rename(auc_value=.estimate) %>% dplyr::select(model,auc_value)

#Hosmer test
library(ResourceSelection)

h1 = hoslem.test(model2$y, fitted(model2), g=10)
h1



h2 = hoslem.test(model4$y, fitted(model4), g=10)
h2




