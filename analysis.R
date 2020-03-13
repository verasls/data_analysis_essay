# Load package and functions ----------------------------------------------

library(datarium)
library(tidyverse)
library(car)

# Read and prepare data ---------------------------------------------------

data("anxiety", package = "datarium")

# Select and rename variables
anxiety <- anxiety %>% 
  as_tibble() %>% 
  select(id, exercise = group, pre_test = t1, post_test = t3)

# Recode exercise factors
anxiety$exercise <- recode_factor(
  anxiety$exercise,
  "grp1" = "Low",
  "grp2" = "Moderate",
  "grp3" = "High"
)

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
ggplot(data = anxiety_long, mapping = aes(x = exercise, y = score)) +
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
  facet_wrap(~exercise) +
  labs(x = "Pre-test", y = "")

ggplot(data = anxiety, mapping = aes(post_test)) +
  geom_histogram(binwidth = bin_width(anxiety$post_test)) +
  facet_wrap(~exercise) +
  labs(x = "Post-test", y = "")

# Normality tests
# Separate the groups into 3 different data frames
anxiety_l <- filter(anxiety, exercise == "Low")
anxiety_m <- filter(anxiety, exercise == "Moderate")
anxiety_h <- filter(anxiety, exercise == "High")
# Run normality tests
shapiro.test(anxiety_l$pre_test)
shapiro.test(anxiety_m$pre_test)
shapiro.test(anxiety_h$pre_test)

shapiro.test(anxiety_l$post_test)
shapiro.test(anxiety_m$post_test)
shapiro.test(anxiety_h$post_test)

# Check assumptions -------------------------------------------------------

# ** Linearity assumption -------------------------------------------------

# Build linear regression models using data from each group separately
lm(formula = post_test ~ pre_test, data = anxiety_l) %>% summary()

lm(formula = post_test ~ pre_test, data = anxiety_m) %>% summary()

lm(formula = post_test ~ pre_test, data = anxiety_h) %>% summary()

# Plot
ggplot(
  data = anxiety, 
  mapping = aes(x = pre_test, y = post_test, colour = exercise)
) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  guides(color=guide_legend("Exercise")) +
  labs(
    x = "Pre-test",
    y = "Post-test"
  )
  
# ** Homegeneity of regression slopes -------------------------------------

aov(formula = post_test ~ pre_test * exercise, data = anxiety) %>% summary()

# ** Homogeneity of variance ----------------------------------------------

leveneTest(anxiety$post_test, anxiety$exercise, center = median)