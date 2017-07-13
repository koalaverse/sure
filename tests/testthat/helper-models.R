housing <- data.frame(
  Sat = c('Low', 'Medium', 'High', 'Low', 'Medium', 'High', 'Low', 'Medium',
         'High', 'Low', 'Medium', 'High', 'Low', 'Medium', 'High', 'Low',
         'Medium', 'High', 'Low', 'Medium', 'High', 'Low', 'Medium', 'High',
         'Low', 'Medium', 'High', 'Low', 'Medium', 'High', 'Low', 'Medium',
         'High', 'Low', 'Medium', 'High', 'Low', 'Medium', 'High', 'Low',
         'Medium', 'High', 'Low', 'Medium', 'High', 'Low', 'Medium', 'High',
         'Low', 'Medium', 'High', 'Low', 'Medium', 'High', 'Low', 'Medium',
         'High', 'Low', 'Medium', 'High', 'Low', 'Medium', 'High', 'Low',
         'Medium', 'High', 'Low', 'Medium', 'High', 'Low', 'Medium', 'High'),
  Infl = c('Low', 'Low', 'Low', 'Medium', 'Medium', 'Medium', 'High', 'High',
           'High', 'Low', 'Low', 'Low', 'Medium', 'Medium', 'Medium', 'High',
           'High', 'High', 'Low', 'Low', 'Low', 'Medium', 'Medium', 'Medium',
           'High', 'High', 'High', 'Low', 'Low', 'Low', 'Medium', 'Medium',
           'Medium', 'High', 'High', 'High', 'Low', 'Low', 'Low', 'Medium',
           'Medium', 'Medium', 'High', 'High', 'High', 'Low', 'Low', 'Low',
           'Medium', 'Medium', 'Medium', 'High', 'High', 'High', 'Low', 'Low',
           'Low', 'Medium', 'Medium', 'Medium', 'High', 'High', 'High', 'Low',
           'Low', 'Low', 'Medium', 'Medium', 'Medium', 'High', 'High', 'High'),
  Type = c('Tower', 'Tower', 'Tower', 'Tower', 'Tower', 'Tower', 'Tower',
           'Tower', 'Tower', 'Apartment', 'Apartment', 'Apartment', 'Apartment',
           'Apartment', 'Apartment', 'Apartment', 'Apartment', 'Apartment',
           'Atrium', 'Atrium', 'Atrium', 'Atrium', 'Atrium', 'Atrium', 'Atrium',
           'Atrium', 'Atrium', 'Terrace', 'Terrace', 'Terrace', 'Terrace',
           'Terrace', 'Terrace', 'Terrace', 'Terrace', 'Terrace', 'Tower',
           'Tower', 'Tower', 'Tower', 'Tower', 'Tower', 'Tower', 'Tower',
           'Tower', 'Apartment', 'Apartment', 'Apartment', 'Apartment',
           'Apartment', 'Apartment', 'Apartment', 'Apartment', 'Apartment',
           'Atrium', 'Atrium', 'Atrium', 'Atrium', 'Atrium', 'Atrium', 'Atrium',
           'Atrium', 'Atrium', 'Terrace', 'Terrace', 'Terrace', 'Terrace',
           'Terrace', 'Terrace', 'Terrace', 'Terrace', 'Terrace'),
  Cont = c('Low', 'Low', 'Low', 'Low', 'Low', 'Low', 'Low', 'Low', 'Low', 'Low',
           'Low', 'Low', 'Low', 'Low', 'Low', 'Low', 'Low', 'Low', 'Low', 'Low',
           'Low', 'Low', 'Low', 'Low', 'Low', 'Low', 'Low', 'Low', 'Low', 'Low',
           'Low', 'Low', 'Low', 'Low', 'Low', 'Low', 'High', 'High', 'High',
           'High', 'High', 'High', 'High', 'High', 'High', 'High', 'High',
           'High', 'High', 'High', 'High', 'High', 'High', 'High', 'High',
           'High', 'High', 'High', 'High', 'High', 'High', 'High', 'High',
           'High', 'High', 'High', 'High', 'High', 'High', 'High', 'High',
           'High'),
  Freq = c(21, 21, 28, 34, 22, 36, 10, 11, 36, 61, 23, 17, 43, 35, 40, 26, 18,
           54, 13, 9, 10, 8, 8, 12, 6, 7, 9, 18, 6, 7, 15, 13, 13, 7, 5, 11, 14,
           19, 37, 17, 23, 40, 3, 5, 23, 78, 46, 43, 48, 45, 86, 15, 25, 62, 20,
           23, 20, 10, 22, 24, 7, 10, 21, 57, 23, 13, 31, 21, 13, 5, 6, 13)
)
housing$Sat <- as.ordered(housing$Sat)

# Ordinal
fit.clm <- ordinal::clm(Sat ~ Infl + Type + Cont, weights = Freq,
                        data = housing, link = "probit")

# MASS
fit.polr <- MASS::polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing,
                       method = "probit")

# VGAM
fit.vglm <- VGAM::vglm(Sat ~ Infl + Type + Cont, weights = Freq, data = housing,
                       family = VGAM::cumulative(link = probit, parallel = TRUE))
