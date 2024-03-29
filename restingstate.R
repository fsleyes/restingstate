library(tidyverse)
library(jtools)
library(haven)

rawData <- read.csv(file = "restingstate_data.csv", header = TRUE)

data <- rawData %>% #this is not switched
  filter(!is.na(pre_pair1)) %>%
  mutate(diff_pair1 = post_pair1 - pre_pair1,
         diff_pair3 = post_pair3 - pre_pair3,
         diff_pair4 = post_pair4 - pre_pair4,
         diff_target = post.target - Pre.target,
         diff_control = post.control - pre.control)
data$pre_date <- lubridate::mdy(data$pre_date)
data$post_date <- lubridate::mdy(data$post_date)
data <- data %>% 
  mutate(diff_date = difftime(post_date, pre_date, units = "days")) %>%
  mutate(target_control_pre_raw = Pre.target - pre.control,
         target_control_post_raw = post.target - post.control,
         target_control_diff = target_control_post_raw - target_control_pre_raw)


cor_target_pair1 <- cor(data$diff_target, data$diff_pair1, method = "pearson")
cor_target_pair3 <- cor(data$diff_target, data$diff_pair3, method = "pearson")
cor_target_pair4 <- cor(data$diff_target, data$diff_pair4, method = "pearson")

reg_target_pair1 <- lm(diff_target ~ diff_pair1 + diff_date + diff_control, data = data)
summary(reg_target_pair1)


# cor_pair1 <- cor(data$induction_avg, data$diff_pair1, method = "pearson")
# cor_pair3 <- cor(data$induction_avg, data$diff_pair3, method = "pearson")
# cor_pair4 <- cor(data$induction_avg, data$diff_pair4, method = "pearson")
# 
# reg_pair1 <- lm(induction_avg ~ diff_pair1, data = data)
# summary(reg_pair1)




ind_learn_scatter <- ggplot(data, aes(x = diff_pair1, y = induction_learning)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  labs(x = "Change in Resting State Network",
       y = "Induction Learning Score")
ind_learn_scatter


#calculations for clinicaltrials

CTdata <- read.csv(file = "restingstate_data.csv", header = TRUE) %>%
  mutate(diff_target = post.target - Pre.target)

avg_amyg_red <- CTdata %>%
  group_by(dosage) %>%
  summarise(avg = mean(diff_target),
            sd = sd(diff_target))



#writing new code for vincent's data to replicate analysis

#vincent_wd <- file.path("~/R Files/restingstate/vincent_data/DecNef_Review.sav")
vincent_df <- file.path("~/R Files/restingstate/vincent_data/vincent_df.csv")

vincent_final_data <- read.csv(file = vincent_df, header = TRUE)

vincent_long <- vincent_df %>% 
  select(Participant, Amyg_D_1:Amyg_C_2) %>% 
  pivot_longer(cols = Amyg_D_1:Amyg_C_2,
               names_to = "amygdala",
               values_to = "activation") 

test_plot <- ggplot(data = vincent_long, mapping = aes(x = amygdala, y = activation)) +
  geom_col()
test_plot 
