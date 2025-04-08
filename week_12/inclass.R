
library(tidyverse)
library(gssr)


theme_set(theme_light())


gss2022 <- gss_get_yr(year = 2022) |> # get 2022 GSS data
  haven::zap_labels()                 # remove labels

d <- gss2022 |>
  select(polviews,attend,degree) |>
  drop_na() |>
  mutate(conserv = if_else(polviews >= 5, 1, 0),
         weekly = if_else(attend >= 7, 1, 0),
         college = if_else(degree >= 3, 1, 0)) |>
  select(polviews, conserv, weekly, college)

d_tab <- d |>
  group_by(weekly, college) |>
  summarize(n = n(),
            pcons = mean(conserv),
            log_odds = log((mean(conserv)/(1 - mean(conserv)))))


alpha = -0.873
b1weekly = .010-(-.873)
b2college = -1.27-(-.873)
b3 = .087 - alpha - b1weekly - b2college

logitm <- glm(conserv ~ college * weekly,
              data = d,
              family = binomial(link = "logit"))

d_tab2 <- d |>
  group_by(weekly, college) |>
  summarize (n = n(),
             m = mean(polviews))

linearm <- lm(polviews ~ college * weekly,
              data = d)

a = 4.01
b1w = 4.72 - 4.01
b2c = 3.46 - 4.01
b3both = 4.65 - a - b1w - b2c

linearm2 <- lm(polviews ~ college + weekly,
               data = d)

logLik(linearm)
logLik(linearm2)

# model selection
## likelihood ratio test
anova(linearm2, linearm, test = "Chisq")

##aic
-2*as.numeric(logLik(linearm)) + 2*5
AIC(linearm)

-2*as.numeric(logLik(linearm2)) + 2*4
AIC(linearm2)

## pick the model with the lower AIC

# hw

do 6 models by hand - 2 linear regressionos (continuous, using means), 2 logit models, 2 poisson models (log count).
we can use the same predictors for all 6, but outcome variables should be different
calculate the parameters for the saturated model by hand. then get rid of b3 for all models and estimate with a computer.
do 6 rest model, 6 sat model by computer. do aic or anova likelihood ratio test
do likelihood ratio and AIC comparison for all models.
