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

# Plot
ggplot(data = anxiety_long) +
  geom_boxplot(mapping = aes(x = group, y = score)) +
  facet_wrap(~time) +

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
ggplot(data = anxiety) +
  geom_point(mapping = aes(x = pre_test, y = post_test, colour = group)) +
  geom_smooth(
    mapping = aes(x = pre_test, y = post_test, colour = group),
    method = "lm",
    se = FALSE
  )

# Homegeneity of regression slopes ----------------------------------------

aov(formula = post_test ~ pre_test * group, data = anxiety) %>% summary()

# Homogeneity of variance -------------------------------------------------

leveneTest(anxiety$post_test, anxiety$group, center = median)