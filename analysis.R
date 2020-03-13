# Load package and functions ----------------------------------------------

library(datarium)
library(tidyverse)
library(car)

# Read and prepare data ---------------------------------------------------

data("anxiety", package = "datarium")

anxiety <- anxiety %>% 
  as_tibble() %>% 
  select(id, group, pre_test = t1, post_test = t3)

# Explore data ------------------------------------------------------------

# Reshape data
anxiety_long <- anxiety %>% 
  pivot_longer(
    c(pre_test, post_test),
    names_to = "time",
    values_to = "score"
  )

# Recode time into a factor
anxiety_long$time <- as_factor(anxiety_long$time)
anxiety_long$time <- recode_factor(
  anxiety_long$time, 
  "pre_test" = "Pre-test",
  "post_test" = "Post-test"
)

# Boxplot
ggplot(data = anxiety_long, mapping = aes(x = group, y = score)) +
  geom_boxplot() +
  geom_dotplot(
    binaxis = "y",
    stackdir = "center",
    dotsize = 0.7,
    binwidth = 0.3,
    fill= "white"
  ) +
  facet_wrap(~time)

# Check assumptions -------------------------------------------------------

# Linearity assumption ----------------------------------------------------

# Build linear regression models using data from each group separately
lm(formula = post_test ~ pre_test, data = filter(anxiety, group == "grp1")) %>% 
  summary()

lm(formula = post_test ~ pre_test, data = filter(anxiety, group == "grp2")) %>% 
  summary()

lm(formula = post_test ~ pre_test, data = filter(anxiety, group == "grp3")) %>% 
  summary()

# Plot
ggplot(data = anxiety, mapping = aes(x = pre_test, y = post_test, colour = group)) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = FALSE
  )

# Homegeneity of regression slopes ----------------------------------------

aov(formula = post_test ~ pre_test * group, data = anxiety) %>% summary()

# Homogeneity of variance -------------------------------------------------

leveneTest(anxiety$post_test, anxiety$group, center = median)