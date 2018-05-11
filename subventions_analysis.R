options(scipen = 999)
library(tidyverse)
library(lubridate)
library(readxl)
library(openxlsx)
library(rvest)
library(edata)
library(readxl)
Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip.exe") # set correct Rtools path here
source("subventions_functions.R")

laws <- read_excel("data/laws_2017.xlsx") %>%
  filter(voting_date != "18.01.2018") %>%
  mutate(id = paste(voting_date, voting_number, sep = "-"),
         shortname = str_replace(shortname, "’", "'"),
         shortname = str_replace(shortname, fixed(" ."), ""))

all_mps <- read_csv("http://data.rada.gov.ua/ogd/mps/skl8/mps08-data.csv")

actual_mps <- all_mps %>%
  filter(is.na(date_end))

mps <- all_mps %>%
  mutate(shortname = paste(last_name, short_name),
         shortname = str_replace(shortname, "-[МБ]\\.", ""),
         shortname = ifelse(shortname == "Тимошенко Ю.В." & first_name == "Юлія",
                            "Тимошенко Юлія В.",
                            ifelse(shortname == "Тимошенко Ю.В." & first_name == "Юрій",
                                   "Тимошенко Юрій В.", shortname))) %>%
  rename(fullname = full_name) %>%
  select(fullname, shortname) %>%
  arrange(fullname)

laws <- laws %>% left_join(mps, by = "shortname") %>%
  mutate(coalition = ifelse(faction %in%
                      c("Блок Петра Порошенка", "Народний фронт"), 1, 0)) %>%
  group_by(id, coalition) %>%
  summarize(percent_coal = mean(vote_simple)) %>%
  filter(coalition == 1) %>%
  right_join(laws, by = "id")%>%
  mutate(coal_for = ifelse(percent_coal > 0.5, 1, 0),
         like_coal = ifelse(vote_simple == coal_for, 1, 0)) %>% 
  left_join(mps, by = "shortname")

laws %>%
  group_by(faction) %>%
  summarize(with_coal = mean(like_coal)) %>%
  arrange(desc(with_coal))

voting_with_coal <- laws %>%
  group_by(fullname) %>%
  summarize(with_coal = mean(like_coal)) %>%
  arrange(desc(with_coal))

factions <- get_factions_open() %>% 
  mutate(district_num = as.character(district_num)) %>% 
  filter(!is.na(district_num)) %>% 
  rename(district = district_num, oblast = region_name) %>% 
  select(-id)

subv <- read_excel("data/subventions_with_districts.xlsx") %>% 
  left_join(factions, by = c("district", "oblast")) %>% 
  mutate(oblast = str_replace(oblast, " область", ""))

subv %>% 
  summarize(sum(total))

budget_ids <- c(10226, 13413, 11356, 16099, 16097, 15516)
budget_names <- paste0("vote_", c("5000", "6600", "6161", "7000", "6776d", "7258"))

voting_budget <- get_voting_results(budget_ids, budget_names) %>% 
  left_join(mps, by = "shortname") %>% 
  select(fullname, v_name, vote_simple) %>%
  spread(v_name, vote_simple)

mps_major <- actual_mps %>% 
  filter(!is.na(district_num)) %>% 
  rename(fullname = full_name, district = district_num, oblast = region_name) %>% 
  select(fullname, district, oblast) %>% 
  mutate(oblast = str_replace(oblast, " область", ""),
         district = as.character(district)) %>% 
  left_join(select(factions, fullname, faction), by = "fullname")

subv_coal <- subv %>% 
  group_by(fullname, faction, oblast) %>%
  summarize(total = sum(total)) %>% 
  filter(!str_detect(fullname, "не відомо")) %>% 
  right_join(mps_major) %>% 
  replace_na(list(total = 0))%>% 
  mutate(region = ifelse(oblast%in%c("Одеська", "Миколаївська", "Херсонська"),
                         "south", ifelse(oblast%in%c("Дніпропетровська", "Донецька", "Запорізька", "Луганська", "Харківська"),
                                         "east", ifelse(oblast%in%c("Волинська", "Закарпатська", "Івано-Франківська", "Львівська", "Рівненська", "Тернопільська", "Хмельницька", "Чернівецька"),
                                                        "west", "center")))) %>% 
  left_join(voting_with_coal, by = "fullname") %>%
  left_join(voting_budget, by = "fullname") %>%
  ungroup() %>% 
  rowwise() %>% 
  mutate(budget = sum(c(vote_5000, vote_6600, vote_6161,
                      vote_7000, vote_6776d, vote_7258))) %>% 
  select(-(vote_5000:vote_7258)) %>% 
  ungroup()

subv_coal %>% 
  ggplot(aes(with_coal, total, col = faction)) +
  geom_point()

subv_coal %>% 
  ggplot(aes(budget, total, col = faction)) +
  geom_point()

subv_coal %>% 
  group_by(faction) %>% 
  summarize(corr = cor(total, with_coal))

subv_coal %>% 
  group_by(faction) %>% 
  summarize(corr = cor(total, budget))
table(subv_coal$faction)

cor(subv_coal$total, subv_coal$with_coal)
cor(subv_coal$total, subv_coal$budget)

laws %>% 
  filter(str_detect(voting_subject, "бюджет")) %>% 
  group_by(voting_subject) %>% 
  count() %>% 
  print(nrow(.), width = Inf)

compare_by_var(subv_coal, "region")

subv_coal <- add_voting_as_var(subv_coal, 16099, "vote_budget_2018")
table(subv_coal$vote_budget_2018)
compare_by_var(subv_coal, "vote_budget_2018")
30.1/13.9

subv_coal <- add_voting_as_var(subv_coal, 10226, "vote_budget_2017")
compare_by_var(subv_coal, "vote_budget_2017") # diff twice less than in case of budget voting
30.8/12.7

# committee members

committees <- get_committees_open()
budget_members <- committees %>% 
  filter(unit %in% c("Комітет з питань бюджету", "Комітет з питань податкової та митної політики"))

subv_coal <- subv_coal %>% 
  mutate(budget_member = ifelse(fullname %in% budget_members$fullname,
                                1, 0))
table(subv_coal$budget_member) # 13 of 29 maj

compare_by_var(subv_coal, "budget_member")

# laws of 07.12

laws_1207 <- laws %>%
  filter(id %in% paste0("07.12.2017-", c(89, 61, 91, 101, 180)))

table(laws_1207$draft_number)

votes_1207 <- laws_1207 %>% 
  group_by(fullname) %>% 
  summarize(votes_1207 = sum(vote_simple == 1))

subv_coal <- subv_coal %>% 
  left_join(votes_1207)

cor(subv_coal$total, subv_coal$votes_1207)

# tax code
subv_coal <- add_voting_as_var(subv_coal, 16097, "tcode")
compare_by_var(subv_coal, "tcode") # diff much lower than in case of budget voting

# healthcare reform
subv_coal <- add_voting_as_var(subv_coal, 15101, "med")
compare_by_var(subv_coal, "med")

# education reform
subv_coal <- add_voting_as_var(subv_coal, 13622, "educ")
compare_by_var(subv_coal, "educ")

# pension reform
subv_coal <- add_voting_as_var(subv_coal, 14763, "pension")
compare_by_var(subv_coal, "pension")

# laws 

subv_coal$faction <- relevel(as.factor(subv_coal$faction), ref = "Позафракційні")

model <- lm(total ~ with_coal + faction, data = subv_coal)
summary(model)

model2 <- lm(total ~ faction + budget, data = subv_coal)
summary(model2)

model3 <- lm(total ~ faction + region + vote_budget_2018, data = subv_coal)
summary(model3)

model4 <- lm(total ~ budget + region, data = subv_coal)
summary(model4)

model5 <- lm(total ~ budget + region + faction, data = subv_coal)
summary(model5)
plot(model5)
sqrt(car::vif(model5)) # nowhere >2, so ok

model6 <- lm(total ~ budget + budget_member + region + faction, data = subv_coal)
summary(model6)

model7 <- lm(total ~ budget + budget_member + pension + region + faction, data = subv_coal)
summary(model7)

# add bills info & laws by budget committee

bills <- read_csv("data/Bill-2018-02-28.csv") %>% 
  select(number, committee)

laws <- laws %>% left_join(bills, by = c("draft_number" = "number"))
table(laws$committee, useNA = "ifany")

laws %>% 
  filter(is.na(committee)) %>% 
  group_by(draft_number) %>% 
  count() %>% 
  print(n = nrow(.))

laws_com <- laws %>% 
  filter(committee == "Комітет з питань бюджету")

laws_com %>% 
  group_by(voting_number) %>% 
  summarize(budget = mean(vote_simple)) %>% 
  arrange(desc(budget))

subv_coal <- laws_com %>% 
  group_by(fullname) %>% 
  summarize(vote_budget_com = mean(vote_simple)) %>% 
  right_join(subv_coal, by = "fullname") %>% 
  select(fullname, faction:vote_budget_2018, vote_budget_com)

model8 <- lm(total ~ vote_budget_com + region + faction, data = subv_coal)
summary(model8)

cor(subv_coal$total, subv_coal$vote_budget_com)

# change in voting patterns

change_halfyears <- laws %>% 
  mutate(voting_date = dmy(voting_date)) %>% 
  group_by(fullname, month = floor_date(voting_date, "6 months")) %>% 
  summarize(with_coal = mean(like_coal)) %>% 
  arrange(fullname, month) %>% 
  filter(fullname %in% subv_coal$fullname) %>% 
  spread(month, with_coal) %>% 
  rename(halfyear_1 = `2017-01-01`, halfyear_2 = `2017-07-01`) %>% 
  mutate(change = halfyear_2 - halfyear_1) %>% 
  arrange(desc(change)) %>% 
  select(fullname, change)
  
subv_coal <- left_join(subv_coal, change_halfyears)
cor(subv_coal$total, subv_coal$change) # no correlation

# companies

companies <- read_csv("data/companies_mps_2017.csv") %>%
  filter(fullname %in% subv_coal$fullname) %>%
  group_by(fullname) %>%
  summarize(companies_n = n())

subv_coal <- subv_coal %>%
  left_join(companies) %>%
  replace_na(list(companies_n = 0)) %>%
  mutate(companies_fact = ifelse(companies_n > 0, 1, 0))

cor(subv_coal$companies_n, subv_coal$total)
compare_by_var(subv_coal, "companies_fact")

model9 <- lm(total ~ faction + companies_fact, data = subv_coal)
summary(model9)

model10 <- lm(total ~ faction + region + budget + companies_fact, data = subv_coal)
summary(model10)

# wealth

wealth <- read_csv("data/wealth_mps_2017.csv") %>% 
  filter(fullname %in% subv_coal$fullname) %>% 
  group_by(fullname) %>% 
  summarize(amount = sum(in_hryvnias, na.rm = T))

subv_coal <- subv_coal %>% 
  left_join(wealth) %>% 
  mutate(amount = ifelse(is.na(amount), 0, amount))

cor(subv_coal$amount, subv_coal$total)

model12 <- lm(total ~ faction + region + budget + amount, data = subv_coal)
summary(model12)

# deputies whose companies received public money in 2017

mps_companies <- read_csv("data/companies_mps_2017.csv") 
codes <- mps_companies$beneficial_owner_company_code
codes

mps_trans <- map(codes, ~transactions(recievers_edrpous = .,
                                 startdate = "2017-01-01", enddate = "2017-12-31"))

companies_received <- mps_companies[which(map(mps_trans, length) > 0),]
mps_received <- unique(companies_received$fullname)
mps_received

subv_coal <- subv_coal %>% 
  mutate(company_received = as.numeric(fullname %in% mps_received))
table(subv_coal$companies_fact, subv_coal$company_received)

compare_by_var(subv_coal, "company_received")

model13 <- lm(total ~ faction + budget + region + companies_fact + company_received, data = subv_coal)
summary(model13)

subv_coal %>% 
  filter(total == 0) %>% 
  select(vote_budget_2018) %>% 
  print(n = nrow(.)) %>% 
  table

# add mp requests: proxy of proactive work by mps

ids <- actual_mps$id[actual_mps$full_name%in%subv_coal$fullname]
fullnames <- actual_mps$full_name[actual_mps$full_name%in%subv_coal$fullname]
links <- paste0("http://w1.c1.rada.gov.ua/pls/zweb2/wcadr42d?sklikannja=9&kod8011=", ids)

get_requests <- function(link){
  requests_n <- read_html(link) %>% 
    html_nodes(".TFOOT .THEAD00:nth-child(2) .AllNews") %>% 
    html_text() %>% 
    str_trim()
  Sys.sleep(0.1)
  return(requests_n)
}

requests <- map(links, possibly(get_requests, otherwise = NA))
requests[which(unlist(map(requests,length))==0)] <- 0

requests_df <- data.frame(fullname = fullnames,
                          req_n = as.integer(unlist(requests)))
requests_df %>% sample_n(4) # test - ok

subv_coal <- subv_coal %>% 
  left_join(requests_df, by = "fullname")

model14 <- lm(total ~ faction + budget + region + companies_fact + req_n, data = subv_coal)
summary(model14) # no effect
  
# random drafts

do_sampling <- function(i){
  
  sample_drafts <- laws %>% ungroup() %>% 
    #   filter(decision == 1) %>% 
    group_by(draft_number) %>% 
    count() %>% select(draft_number) %>% unlist() %>% sample(6)
  
  laws_sample <- laws %>% 
    filter(draft_number %in% sample_drafts) %>% 
    group_by(fullname) %>% 
    summarize(sample_votings = mean(vote_simple)) %>% 
    arrange(desc(sample_votings))
  
  subv_sample <- subv_coal %>% 
    left_join(laws_sample, by = "fullname") %>%
    filter(!is.na(fullname) & !str_detect(fullname, "не відомо")) %>% 
    ungroup() 

  cor_sample <- cor(subv_sample$total, subv_sample$sample_votings)
  
  sample_model <- lm(total ~ sample_votings + region + faction + companies_fact, data = subv_sample)
  adj_r_sample <- summary(sample_model)$adj.r.squared
  sample_model_tidy <- broom::tidy(sample_model)
  coef_p_sample <- sample_model_tidy[sample_model_tidy$term == "sample_votings", c("estimate", "p.value")]
  return(data.frame(cor_sample, adj_r_sample, coef_p_sample))
  
}

sample_df <- map_df(1:100, do_sampling)
map(sample_df, mean)
hist(sample_df$p.value)
abline(v = broom::tidy(model10)[broom::tidy(model10)$term == "budget",]$p.value, 
       col = "red", lty = "dashed")
sort(sample_df$p.value)

# so, most probably the effect of voting for budget is not due to a chance

library(stargazer)
stargazer(model10, type = "text",
          title = "Модель пояснення загальної суми субвенцій на округ",
          digits = 3, report = "vcp*",
          dep.var.labels.include = F, header = F, digit.separator = " ",
          single.row = T, dep.var.caption = "", omit.stat = c("ser", "f"),
          covariate.labels = c(levels(subv_coal$faction)[-1], "Схід (порівняно з Центром)", "Південь", "Захід",
          "Голосування за кожен ЗП про бюджет", "Наявність компаній", "Константа"),
          out = "best_model.html")

