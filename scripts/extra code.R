factor_vars <- elt_w1_clean_5 %>%
  select(1:7, 136:168) %>%
  lapply(as.factor) %>% 
  data.frame() 


factor_vars <- factor_vars  %>% 
  lapply(scales::percent) %>% 
  data.frame()

summary(factor_vars) # this is the best I HAVE COME WITH SO FAR. 

num_vars <- elt_w1_clean_6 %>%
  select(1:4, 9:132, 135, 163, 164, 166, 168) %>%
  mutate(age = as.numeric(age)) # age needs to be numeric


num_vars <- elt_w1_clean_6 %>%
  select(9:132, 135, 163, 164, 166, 168) %>%
  mutate(age = as.numeric(age)) %>% # age needs to be numeric
  proportions() # didnt work 

  
# function to take proportions
prop_fun <- function(df, var) {
  df %>%
    group_by({{var}}) %>%
    summarize(n = n()) %>%
    mutate(prop = round(n/sum(n), 2))
}

prop_fun(elt_w1_clean_6, black_african_american)

library(gtsummary)

# nice frequencies table
elt_w1_clean_6 %>% 
  select(1:5, 136:168) %>% 
  gtsummary::tbl_summary()


  
## not this
elt_w1_clean_6 %>% 
  select(white, race_ethnicity_other, indigenous_americas, hispanic_latino_spanish, black_african_american, american_indian_alaska_native) %>% 
  pivot_longer(
    cols = everything(),
    names_to = "race",
    values_to = "value"
  ) %>% 
  group_by(race) %>% 
  summarize(n = sum(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(prop = (n/sum(n)* 100))
## not this



# plot not used
ggplot(viz, aes(fct_reorder(race, count), count)) +
  geom_point(aes(color = participant_role), position = "jitter", size = 3) +
  coord_flip() +
  scale_colour_viridis_d("Educator's roles", option = "plasma") +
  theme_minimal() +
  labs(title = "Educators' Races/ethnicities and Roles",
       y = "",
       x = "") +
  theme(plot.title = element_text(family = "sans", size = 25, face = "bold", hjust = 0.5, margin = margin(20, 20, 10, 10)),
        axis.text = element_text(family = "sans", size = 12))
