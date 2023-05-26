pacman::p_load(
  
  # usos generales
  tidyverse, 
  printr,    
  
  # exploracion
  skimr,
  GGally, 
  correlation, #easystats 

  # modelado 
  
  ## easystats https://easystats.github.io/easystats/
  # easystats, # carga todos
  # parameters,
  modelbased,
  see,

  relaimpo, 
  ggeffects,
  car,
  sjPlot 
)

conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("select", "dplyr")

# Modelos lineares
triticum <- rio::import("https://raw.githubusercontent.com/juanchiem/agro_data/master/triticum_monococcum.txt")

# Exploracion

str(triticum)

triticum %>% 
  skim()

triticum %>% 
  select(-ID)  %>% # correr conflicted::... 
  ggpairs() +
  theme_bw()

triticum %>%
  select(-ID)  %>% 
  correlation(method = "Pearson")


# RLS

m0 <- lm(
  weight ~ diameter, 
  data = triticum
)
# check_model(m0)
check_normality(m0)
check_heteroscedasticity(m0)
# significancia de los parametros
anova(m0)

# coeficientes 
summary(m0)

check_predictions(m0)
performance_accuracy(m0)

# RLM

m1 <- lm(
  weight ~ length + diameter + hardness + moisture, 
  data = triticum
)
check_normality(m1)
check_heteroscedasticity(m1)
anova(m1)
summary(m1)

m2 <- lm(
  weight ~ length + diameter + hardness, 
  data = triticum
)
# m2 <- update(m1, . ~ . -moisture)
compare_performance(m1, m2)

# performance::
check_model(m2)
check_normality(m2)
check_heteroscedasticity(m2) %>% plot
check_collinearity(m2) %>% plot
summary(m2)

m3 <- lm(
  weight ~ diameter + hardness, 
  data = triticum
)

# performance::
check_collinearity(m3) %>% plot() # corregimos multcol!
performance_accuracy(m3)
compare_models(m2, m3)
compare_performance(m2, m3)# se ve que no se gana en precision pero el m2 tenia colinealidad

# relaimpo::
ri_m <- calc.relimp(m3, type =  "car", rela = TRUE )  
ri_m
plot(ri_m)

summary(m3)
tab_model(m3)

# modelbased::
estimate_relation(m3) %>% 
  plot(ribbon = list(alpha = 0)) # Make CI ribbon transparent for clarity

# sjPlot::  
plot_model(m3, type = "pred", terms = c("diameter", "hardness"))
plot_model(m3, type = "pred", terms = c("hardness", "diameter"))
# Se nota la mayor importancia de `diameter`en este grafico no?

#ggeffects::
ggpredict(m3, terms = c("diameter [2.5]", "hardness [-1]"))

```{r}
new_data = data.frame(diameter = 2.5, hardness = -1)
new_data <- new_data %>% 
  bind_cols(predict(m2, newdata = new_data, interval = "confidence")) %>% 
  rename(weight=fit)
```

```{r}
plot_model(m2, type = "pred", terms = c("diameter", "hardness"))+
  geom_point(data=new_data, aes(x=diameter, y=weight), 
             inherit.aes = FALSE)
```

# a modo anecdotico vemos una interaccion de 2 var continuas
m4 <- lm(
  weight ~ diameter * hardness, 
  data = triticum
)

anova(m4)
summary(m4)
compare_models(m3, m4)
compare_performance(m3, m4)

preds4 <- estimate_relation(m4)
plot(preds4, ribbon = list(alpha = 0)) # Make CI ribbon transparent for clarity


# triticum2 <- triticum %>% 
#   mutate(bin_cut = cut_interval(diameter, n = 3),
#          bin_ntile = factor(ntile(diameter, 3)))
# 
# triticum2 %>% 
#   pivot_longer(contains("bin")) %>% 
#   ggplot()+ 
#   aes(x=diameter, fill=value) +
#   geom_histogram()+
#   facet_wrap("name", ncol = 1)
# 
# m5 <- lm(
#   weight ~ diameter * bin_cut, 
#   data = triticum2
# )
# anova(m5)
# compare_performance(m1,m5)
# 
# preds5 <- estimate_relation(m5)
# plot(preds5, ribbon = list(alpha = 0)) # Make CI ribbon transparent for clarity


# SPlit dataset ------------------
# Create training set
train <- triticum %>% sample_frac(.70)
# Create test set
test <- anti_join(triticum, train, by = 'ID')
#----- 

m0_1 <- lm(
  weight ~ diameter, 
  data = train
)

test <- test %>% mutate(pred_rls = predict(m0_1, test))

m3_1 <- lm(
  weight ~ diameter + hardness, 
  data = train
)

test <- test %>% mutate(pred_rlm = predict(m3_1, test))
head(test)
rls <- lm(pred_rls ~ weight, data = test)
rlm <- lm(pred_rlm ~ weight, data = test)
compare_performance(rls, rlm)

test %>% 
  ggplot()+
  aes(weight)+
  coord_equal()+
  geom_abline(slope=1)+
  geom_point(aes(y=pred_rls),col="blue")+
  geom_smooth(aes(y=pred_rls),col="blue")+
  geom_point(aes(y=pred_rlm), col="red")+
  geom_smooth(aes(y=pred_rlm), col="red")


# Polinomial
df <- data.frame(
  hours = c(7.7,8.7,10.7,14.1,
            7,14,14.4,11.6,11.3,5.6,7.1,6.8,11.9,8.8,
            12.7,10,12.2,14.9,8.8,12.8,14.3,7.1,11.5,
            6.3,7.7,8.9,5.1,8.8,13.7,8.4,9.8,11,9.9,6.9,
            13.3,11.7,12.9,6.1,12.2,9.1,13.2,11.5,12.8,
            10.5,10.3,12.9,5.2,9.8,12.3,11.9),
  score = c(64.3,70.7,73.7,
            86.1,59.8,83.6,89.1,78.1,78.4,59.1,65.9,60.8,
            78.5,66.4,84.6,69.2,80,98.5,64.1,87.9,88.9,
            65.5,75.7,60,64.3,71.4,60.5,66.8,91.5,70.4,
            70.4,77.7,70.4,61.2,88.9,74.7,86.6,58.3,77.5,
            65.5,81.7,72.2,85.1,77.5,75.6,87.5,58.6,70,
            84.8,80.5)
)

p1 <- df %>%
  ggplot()+
  aes(hours, score)+
  geom_point()+
  geom_smooth()

p1 + 
  geom_smooth(method = lm)+
  geom_smooth(method = lm, formula = y ~ poly(x, 2), col="red")

base <- lm(score ~ poly(hours,1, raw=F), data=df)
check_model(base, check="linearity")

quad <- lm(score ~ poly(hours,2, raw=F), data=df)
check_model(quad, check="linearity")

AIC(base, quad)
summary(quad)
compare_performance(base, quad)

preds_quad <- estimate_relation(quad)
plot(preds_quad, ribbon = list(alpha = .2)) + 
  theme_bw() + 
  get_parameters(quad)



# https://daviddalpiaz.github.io/r4sl/linear-models.html