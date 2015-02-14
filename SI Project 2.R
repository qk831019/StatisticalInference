library(datasets)
data(ToothGrowth)
library(ggplot2)
ggplot(data=ToothGrowth, aes(x=as.factor(dose), y=len, fill=supp)) +
  geom_bar(stat="identity",) +
  facet_grid(. ~ supp) +
  xlab("Dose in miligrams") +
  ylab("Tooth length") +
  guides(fill=guide_legend(title="Supplement type"))



#--------------------------------------------------
t.test(len ~ supp, paired = FALSE, var.equal = FALSE, data = ToothGrowth)
#--------------------------------------------------
dose12 <- subset(ToothGrowth, dose %in% c(0.5, 1))
t.test(len ~ dose, paired = FALSE, var.equal = FALSE, data = dose12)

dose23 <- subset(ToothGrowth, dose %in% c(1, 2))
t.test(len ~ dose, paired = FALSE, var.equal = FALSE, data = dose23)

dose13 <- subset(ToothGrowth, dose %in% c(0.5, 2))
t.test(len ~ dose, paired = FALSE, var.equal = FALSE, data = dose13)

fit <- lm(len ~ dose + supp, data=ToothGrowth)
summary(fit)

confint(fit)
