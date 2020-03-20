# Load package and functions ----------------------------------------------

library(tidyverse)
library(car)
library(emmeans)

# Read and prepare data ---------------------------------------------------

data("anxiety", package = "datarium")

# Select and rename variables
anxiety <- anxiety %>% 
  as_tibble() %>% 
  select(id, exercise = group, pre_test = t1, post_test = t3)

# Recode exercise factors
anxiety$exercise <- recode_factor(
  anxiety$exercise,
  "grp1" = "Control",
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
anxiety_c <- filter(anxiety, exercise == "Control")
anxiety_m <- filter(anxiety, exercise == "Moderate")
anxiety_h <- filter(anxiety, exercise == "High")
# Run normality tests
shapiro.test(anxiety_c$pre_test)
shapiro.test(anxiety_m$pre_test)
shapiro.test(anxiety_h$pre_test)

shapiro.test(anxiety_c$post_test)
shapiro.test(anxiety_m$post_test)
shapiro.test(anxiety_h$post_test)

# Descriptives
descriptives <- anxiety_long %>% 
  group_by(exercise, time) %>% 
  summarise(
    n = n(),
    mean = mean(score),
    sd = sd(score)
  )
descriptives %>% as.data.frame()

# Check assumptions -------------------------------------------------------

# ** Linearity between the covariate and the outcome variable -------------

# Build linear regression models using data from each group separately
lm(formula = post_test ~ pre_test, data = anxiety_c) %>% summary()

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

# ANCOVA ------------------------------------------------------------------

# Run ANCOVA
ancova <- aov(formula = post_test ~ pre_test + exercise, data = anxiety)

# Get type III sum of squares
Anova(ancova, type = "III")

# Estimated marginal means
emmeans(ancova, ~ exercise)

# Get model coefficients
summary.lm(ancova)

# Set orthogonal contrasts and re run the model
contrasts(anxiety$exercise) <- cbind(c(2, -1, -1), c(0, 1, -1))

# Run ANCOVA
ancova_2 <- aov(formula = post_test ~ pre_test + exercise, data = anxiety)

# Get type III sum of squares
Anova(ancova_2, type = "III")

# Estimated marginal means
emmeans(ancova_2, ~ exercise)

# Get model coefficients
summary.lm(ancova_2)

# Model diagnostics plots
plot(ancova_2, 1) # Homogeneity of variance
plot(ancova_2, 2) # Q-Q plot

# Post hoc tests ----------------------------------------------------------

pairs(emmeans(ancova_2, ~ exercise), adjust = "Bonferroni")
pairs(emmeans(ancova_2, ~ exercise), adjust = "Holm")

# Plot pre- and post- test scores by group --------------------------------

# ** Without adjusting for baseline ---------------------------------------

# Build plot data frame
# Get descriptives and compute 95% confidence interval
no_adj_plot_df <- descriptives %>% 
  filter(time == "Post-test") %>% 
  mutate(
    lower_CI = mean - ((sd / sqrt(n)) * qt(0.975, df = n - 1)),
    upper_CI = mean + ((sd / sqrt(n)) * qt(0.975, df = n - 1))
  )

# Plot
ggplot(data = no_adj_plot_df) +
  geom_point(
    mapping = aes(x = exercise, y = mean),
    position = position_dodge(0.3)
  ) +
  geom_line(
    mapping = aes(x = exercise, y = mean, group = 1)
  ) +
  geom_errorbar(
    aes(x = exercise, ymin = lower_CI, ymax = upper_CI),
    position = position_dodge(0.3), width = 0.3
  )

# Run an ANOVA to confirm plot
aov(formula = post_test ~ exercise, data = anxiety) %>% summary()
pairwise.t.test(anxiety$post_test, anxiety$exercise, p.adjust.method = "bonferroni")

# ** Adjusting for baseline -----------------------------------------------

# Build plot dataframe
# Put estimated marginal means for post-test into a data frame
emmeans <- emmeans(ancova_2, ~ exercise) %>% as.data.frame()

# Plot
ggplot(data = emmeans) +
  geom_point(
    mapping = aes(x = exercise, y = emmean),
    position = position_dodge(0.3)
  ) +
  geom_errorbar(
    aes(x = exercise, ymin = lower.CL, ymax = upper.CL),
    position = position_dodge(0.3), width = 0.3
  )

# Publication plot --------------------------------------------------------

ggplot(data = emmeans) +
  geom_point(
    mapping = aes(x = exercise, y = emmean),
    position = position_dodge(0.3), size = 2
  ) +
  geom_errorbar(
    aes(x = exercise, ymin = lower.CL, ymax = upper.CL),
    position = position_dodge(0.3), width = 0.3
  )