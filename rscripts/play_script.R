#Create a df of records with missing data
missing_soc_care10 <-         
  soc_care10 %>%
  filter(!complete.cases(.)) 

#Create a table counting total number records from each LA
la_missing <- 
  fct_count(soc_care10$council)

#Create a table of total numebr of records with missing data from each LA
missing_play <- 
  fct_count(missing_soc_care10$council)

#Add count of missing records to total records and calculate perecntages then plot
la_missing %<>%
  mutate(missing = as.numeric(missing_play$n)) %>%
  mutate(proportion = (missing / n) * 100) %>%
  arrange(-proportion) %>%
  mutate(f = factor(f, f))

ggplot(la_missing) +
  geom_col(aes(x = f, y = proportion)) +
  theme(axis.text.x = element_text(angle = 45, size = 10, hjust = 1, vjust = 1)) +
  theme_hc()

la_missing
