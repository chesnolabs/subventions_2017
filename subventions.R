library(tidyverse)
library(readxl)
library(openxlsx)
Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip.exe") # set correct Rtools path here
source("subventions_functions.R")

factions <- get_factions_open() %>% 
  mutate(district_num = as.character(district_num)) %>% 
  filter(!is.na(district_num)) %>% 
  rename(district = district_num, oblast = region_name) %>% 
  select(-id)

subv <- read_excel("data/subventions_with_districts.xlsx") %>% 
  left_join(select(factions, -oblast), by = "district")

sum(subv$amount, na.rm = T) 
sum(subv$special_amount, na.rm = T)
sum(subv$total)
summary(subv$total)

top_titles <- subv %>% 
  group_by(title) %>% 
  count() %>%
  arrange(desc(n)) %>% 
  head(10)
  
summary_by_mp <- subv %>% 
  group_by(district, fullname, oblast, faction) %>% 
  summarize(sum_total = sum(total), n_subv = n()) %>%
  right_join(factions, by = c("fullname", "district", "oblast", "faction")) %>% 
  replace_na(list(sum_total = 0, n_subv = 0)) %>% 
  arrange(desc(sum_total))

summary_by_faction <- subv %>% 
  group_by(fullname, faction) %>% 
  summarize(total_mp = sum(total)) %>% 
  ungroup(fullname) %>% 
  group_by(faction) %>% 
  summarize(mean_per_mp = mean(total_mp),
            median_per_mp = median(total_mp)) %>% 
  arrange(desc(mean_per_mp)) %>% 
  filter(!is.na(faction))

summary_by_region <- subv %>% 
  group_by(oblast) %>% 
  summarize(total = sum(total)) %>% 
  arrange(desc(total))

top_10 <- subv %>% 
  arrange(desc(total)) %>% 
  slice(1:10)

# population data from here http://www.ukrstat.gov.ua/
pop <- read_excel("data/kn1117_u.xls", skip = 5, col_names = F) %>% 
  slice(-((nrow(.)-1):nrow(.))) %>% 
  rename(oblast = X__1, 
         pop_dec_2017 = X__2, 
         pop_mean_2017 = X__3, 
         stable_pop_dec_2017 = X__4, 
         stable_pop_mean_2017 = X__5) %>% 
  mutate_if(is.numeric, ~./1000) %>% 
  mutate(oblast = recode(oblast, `м.Київ` = "м. Київ"))

# correction for occupied areas (see http://www.ipiend.gov.ua/uploads/nz/nz_85_86/skliar_chyselnist.pdf)
pop[pop$region == "Донецька", map_lgl(pop, is.numeric)] <- 
  pop[pop$region == "Донецька", map_lgl(pop, is.numeric)] - pop[pop$region == "Донецька", map_lgl(pop, is.numeric)]*0.44

pop[pop$region == "Луганська", map_lgl(pop, is.numeric)] <- 
  pop[pop$region == "Луганська", map_lgl(pop, is.numeric)] - pop[pop$region == "Донецька", map_lgl(pop, is.numeric)]*0.32

summary_by_region <- subv %>% 
  mutate(oblast = str_replace(oblast, " область", "")) %>% 
  group_by(oblast) %>% 
  summarize(total = sum(total)) %>% 
  mutate(oblast = str_trim(oblast)) %>% 
  left_join(pop[, 1:3], by = "oblast") %>% 
  mutate(amount_per_person = total/pop_mean_2017) %>% 
  arrange(desc(amount_per_person)) %>% 
  select(oblast, total, amount_per_person)

regions_factions <-  subv %>% 
  group_by(oblast, district) %>% 
  count(faction) %>% 
  select(-n) %>% ungroup() %>%
  group_by(oblast) %>% 
  count(faction) %>% 
  filter(!is.na(faction))

subv %>% group_by(district) %>% 
  ggplot(aes(x = total)) +
    geom_histogram(binwidth = 200) +
    scale_x_continuous(limits = c(0, 6000))  

# writing in file

subvwb <- createWorkbook("output/subventions_summary.xlsx")

addWorksheet(subvwb, "Підсумок за депутатами")
writeDataTable(subvwb, "Підсумок за депутатами", summary_by_mp, withFilter = F, rowNames = F)

addWorksheet(subvwb, "Підсумок за фракцією")
writeDataTable(subvwb, "Підсумок за фракцією", summary_by_faction, withFilter = F, rowNames = F)

addWorksheet(subvwb, "Підсумок за регіоном")
writeDataTable(subvwb, "Підсумок за регіоном", summary_by_region, withFilter = F, rowNames = F)

addWorksheet(subvwb, "Топ-10 субвенцій")
writeDataTable(subvwb, "Топ-10 субвенцій", top_10, withFilter = F, rowNames = F)

saveWorkbook(subvwb, "output/subventions_summary.xlsx", overwrite = TRUE)

