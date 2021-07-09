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
  select(1, 2, 5, 136:168) %>% 
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


# plot.title = element_text(family = "sans", size = 25, face = "bold", hjust = 0.5, margin = margin(20, 20, 10, 10))
# 
# plot.title = element_text(family = "sans", size = 25, face = "bold", hjust = 0.3, margin = margin(20, 20, 10, 10))


### dealing with suffixes
final_elt_w1 <- left_join(elt_w1_clean_5, elt_w1_scales, by = c("school_id", "response_id", "id")) %>%
  select(-169:-172) %>%
  rename(c("condition" = "condition.x"), 
         c("participant_role" = "participant_role.x"),
         c("participant_role_5_text" = "participant_role_5_text.x"),
         c("participant_role_6_text" = "participant_role_6_text.x"))

# note: I tried using the the gsub() function to remove the .x, but it didn't work: gsub(".x", "", colnames(final_elt_w1))

# Joe's feedback: A good rule of thumb is that if you find yourself copy/pasting code 3+ times, you can write a function. Or, with the code chunk starting on line 243, you can probably do all of these at once. Something like this might work:

test <- elt_w1_clean_3 %>% 
  select(id, starts_with("q132_1"), starts_with("q132_2"), starts_with("q132_3"), starts_with("q132_4"), -q132_4_text, -q132_3_text) %>% 
  pivot_longer(
    cols = starts_with("q132"),
    names_to = c("item", "language", "response"),
    names_sep = "_",
    values_to = "language_comfort",
    values_drop_na = TRUE) %>% 
  filter(language_comfort == 1) %>%
  mutate(language_comfort = as.numeric(language_comfort)) # not sure if this will work because there is a duplicated response.

test %>%
  pivot_wider(names_from = language, values_from = response, values_fn = length) # this is counting the responses, not giving the response option. 

# Try to make all of your reported stats responsive to the data (e.g., line 82, the Educator's characteristics section, internal consistencies, etc.). This will especially help when you use this code for the other two reports.
# I fixed what i could, but I don't know if I can take percentages using inline code. 
# 
# 



# selecting id vars to be able to join later
elt_w1_clean_6_id <- elt_w1_clean_5 %>%
  select(1:3)

elt_w1_clean_6_vars <- elt_w1_clean_5 %>%
  select(4:167)



--------------------------------------------------------------------------------
# trying to collapse some youth variables -- didn't work
  stress_test <- y_w1_clean_3 %>% 
  select(id, starts_with("q101_1"), starts_with("q102_2"), 
         starts_with("q102_1"), starts_with("q102_2"), 
         starts_with("q103_1"), starts_with("q103_2"), 
         starts_with("q104_1"), starts_with("q104_2"),
         starts_with("q105_1"), starts_with("q105_2"),
         starts_with("q106_1"), starts_with("q106_2"),
         starts_with("q107_1"), starts_with("q107_2")) %>% 
  pivot_longer(
    cols = starts_with("q10"),
    names_to = c("item", "place", "response"),
    names_sep = "_",
    values_to = "stress_lev",
    values_drop_na = TRUE) %>% 
  filter(stress_lev == 1) %>%
  mutate(stress_lev = as.numeric(stress_lev))

get_dupes(stress_test, id, place, item)

stress_test_2 <- stress_test %>% 
  distinct(id, place, item, .keep_all = TRUE)

get_dupes(stress_test_2, id, place, item)

# renaming vars
stress_test_3 <- stress_test_2 %>% 
  pivot_wider(names_from = place, values_from = response) %>%
  rename(c("10_school" = "1"),
         c("10_other" = "2")) %>%
  select(-item, -stress_lev) %>%
  pivot_longer(
    cols = starts_with("10_"),
    names_to = c("place"),
    values_to = "response",
    values_drop_na = TRUE)

stress_test_stress_4 <- stress_test_3 %>%
  pivot_wider(names_from = place, values_from = response) # this makes lists. Also, the var names gets lost. 
--------------------------------------------------------------------------------
  