library(lme4)
library(modelbased)

model <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
preds <- estimate_relation(model, include_random = TRUE)
plot(preds, ribbon = list(alpha = 0)) # Make CI ribbon transparent for clarity

model1 <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
preds1 <- estimate_relation(model1, include_random = TRUE)
plot(preds1, ribbon = list(alpha = 0.1))

compare_models(model, model1)
compare_performance(model, model1)
