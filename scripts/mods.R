
library(nlme)

mod1 <- lme(mean ~ substrate, random = ~ 1|inst/(substrate/exp), data = bmp)
summary(mod1)

summary(mod1)$coefficients$random

mod2 <- lm(cvCH4 ~ substrate + inst + inst:exp + inst:exp:substrate, data = bmp)
summary(mod2)
anova(mod2)
drop1(mod2, test = 'F')
