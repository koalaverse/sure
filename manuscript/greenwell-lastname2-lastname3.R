################################################################################
# Setup
################################################################################

# Load required packages
library(ggplot2)
library(MASS)
library(ordinal)
library(rms)
library(VGAM)
library(ordr)


################################################################################
# Introduction
################################################################################

# Load the simulated quadratic data
data(df.quadratic)

# Fit a (correct) probit model
fit.polr <- polr(y ~ x + I(x ^ 2), data = df.quadratic, method = "probit")

# Surrogate residuals
set.seed(101)  # for reproducibility
sres <- resids(fit.polr)

# Probability-scale residuals
pres <- presid(fit.polr)

# Figure 1
p1 <- ggplot(data.frame(x = df.quadratic$x, y = sres), aes(x, y)) +
  geom_point(alpha = 0.5) +
  geom_smooth(color = "red", se = FALSE) +
  ylab("Surrogate residual")
p2 <- ggplot(data.frame(y = sres), aes(sample = y)) +
  stat_qq(alpha = 0.5) +
  xlab("Sample quantile") +
  ylab("Theoretical quantile")
p3 <- ggplot(data.frame(x = df.quadratic$x, y = pres), aes(x, y)) +
  geom_point(alpha = 0.5) +
  geom_smooth(color = "red", se = FALSE) +
  ylab("Probability residual")
p4 <- ggplot(data.frame(y = pres), aes(sample = y)) +
  stat_qq(distribution = qunif, dparams = list(min = -1, max = 1), alpha = 0.5) +
  xlab("Sample quantile") +
  ylab("Theoretical quantile")

pdf(file = "manuscript\\correct-model.pdf", width = 7, height = 7)
grid.arrange(p1, p2, p3, p4, ncol = 2)
dev.off()


fit2.polr <- update(fit.polr, y ~ x)
p1 <- autoplot(fit2.polr, what = "covariate", x = df.quadratic$x, alpha = 0.5) +
  xlab("x") +
  ylab("Surrogate residual") +
  ggtitle("")
p2 <- ggplot(data.frame(x = df.quadratic$x, y = presid(fit2.polr)), aes(x, y)) +
  geom_point(alpha = 0.5) +
  geom_smooth(color = "red", se = FALSE) +
  xlab("x") +
  ylab("Probability-scale residual")

pdf(file = "manuscript\\quadratic.pdf", width = 8, height = 4)
grid.arrange(p1, p2, ncol = 2)
dev.off()


################################################################################
# Detecting heteroscedasticty
################################################################################


# Fit a probit model
fit.polr <- polr(y ~ x, data = df.heteroscedastic, method = "probit")
set.seed(101)  # for reproducibility
sur.res <- resids(fit.polr)  # surrogate-based residuals

# Compute Li-Shepherd/probability scale residuals
ls.res <- PResiduals::presid(fit.polr)

# Residual vs covariate plots
p1 <- ggplot(data.frame(x = hd$x, y = sur.res), aes(x, y)) +
  geom_point(size = 2, alpha = 0.25) +
  geom_smooth(color = "red", se = FALSE) +
  ylab("Surrogate residual")
p2 <-   ggplot(data.frame(x = hd$x, y = ls.res), aes(x, y)) +
  geom_point(size = 2, alpha = 0.25) +
  geom_smooth(col = "red", se = FALSE) +
  ylab("Probability scale residual")

# Figure ?
pdf(file = "manuscript\\heteroscedasticity.pdf", width = 8, height = 4)
grid.arrange(p1, p2, ncol = 2)
dev.off()


################################################################################
# Quality of wine
################################################################################

library(ordinal)
data(wine, package = "ordinal")
wine.clm <- clm(rating ~ temp * contact, data = wine)  # default logit link

# Figure ?
pdf(file = "manuscript\\wine.pdf", width = 8, height = 8)
set.seed(101)  # for reproducibility
grid.arrange(
  autoplot(wine.clm, nsim = 10, what = "qq"),
  autoplot(wine.clm, nsim = 10, what = "fitted"),
  autoplot(wine.clm, nsim = 10, what = "cov", x = wine$temp),
  autoplot(wine.clm, nsim = 10, what = "cov", x = wine$contact),
  ncol = 2
)
dev.off()
