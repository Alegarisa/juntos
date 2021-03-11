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


#### idea for function to take proportions ###
elt_w1_clean_6 %>%
  group_by(gender_id) %>%
  summarize(n = n()) %>%
  mutate(prop = n/sum(n))


fun <- function(df, var) {
  df %>%
    group_by(var) %>%
    summarize(n = n()) %>%
    mutate(prop = n/sum(n))
}