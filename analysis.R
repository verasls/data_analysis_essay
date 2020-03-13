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
  facet_wrap(~time) +
  labs(x = "Group", y = "Score")

# Histogram
# Define function to select best bin width
bin_width <- function(variable) {
  bw <- 2 * IQR(variable) / length(variable)^(1/3)
  return(bw)
}

ggplot(data = anxiety, mapping = aes(pre_test)) +
  geom_histogram(binwidth = bin_width(anxiety$pre_test)) +
  facet_wrap(~group) +
  labs(x = "Pre-test", y = "")

bw <- 2 * IQR(anxiety$post_test) / length(anxiety$post_test)^(1/3)
ggplot(data = anxiety, mapping = aes(post_test)) +
  geom_histogram(binwidth = bin_width(anxiety$post_test)) +
  facet_wrap(~group) +
  labs(x = "Post-test", y = "")

# Normality tests
# Separate the groups into 3 different data frames
anxiety_grp1 <- filter(anxiety, group == "grp1")
anxiety_grp2 <- filter(anxiety, group == "grp2")
anxiety_grp3 <- filter(anxiety, group == "grp3")
# Run normality tests
shapiro.test(anxiety_grp1$pre_test)
shapiro.test(anxiety_grp2$pre_test)
shapiro.test(anxiety_grp3$pre_test)

shapiro.test(anxiety_grp1$post_test)
shapiro.test(anxiety_grp2$post_test)
shapiro.test(anxiety_grp3$post_test)

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