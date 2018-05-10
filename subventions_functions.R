# get_subventions <- function(){
#   
#   subv <- read_excel("Subvention local 2017_full.xlsx", skip = 1,
#              col_names = c("budget_code", "oblast", "budget",
#                            "title", "amount", "special_amount", 
#                            "district", "faction", "fullname", "purpose",
#                            "lat", "long"))
#   
#   sum(is.na(subv$amount) & is.na(subv$special_amount))
#   
#   subv$amount[is.na(subv$amount)] <- 0
#   subv$special_amount[is.na(subv$special_amount)] <- 0
#   
#   subv$total <- subv$amount + subv$special_amount
#   
#   subv <- subv %>% 
#     mutate(faction = recode(faction,
#                             `НФ` = "Народний фронт",
#                             `БПП` = "Блок Петра Порошенка",
#                             `ПВ` = "Відродження",
#                             `ВН` = "Воля народу",
#                             `ОП` = "Опозиційний блок",
#                             `ПФ` = "Позафракційні",
#                             `ВОБ` = "Батьківщина",
#                             `ОБ` = "Опозиційний блок",
#                             `ВБ` = "Батьківщина",
#                             `ОС` = "Самопоміч",
#                             `ПС` = "Самопоміч",
#                             `ПБ` = "Батьківщина"))
#   subv <- subv %>% 
#     filter(!str_detect(title, "на формування інфраструктури об’єднаних територіальних громад"))
#   subv$district[subv$fullname == "Арешонков Володимир Юрійович"] <- 64
#   subv$district[subv$fullname == "Дзюблик Павло Володимирович"] <- 66
#   subv$district[subv$fullname == "Дехтярчук Олександр Володимирович"] <- 154
#   subv$district[subv$fullname == "Дідич Валентин Володимирович"] <- 40
#   subv$fullname[subv$district == 29] <- "Купрій Віталій Миколайович"
#   subv$district[subv$fullname == "Іллєнко Андрій Юрійович"] <- 215
#   subv$district[subv$fullname == "Кіссе Антон Іванович"] <- 142
#   subv$district[subv$fullname == "Святаш Дмитро Володимирович"] <- 170
#   subv$district[subv$fullname == "Гузь Ігор Володимирович"] <- 195
#   subv$fullname[subv$district == 87] <- "Дерев'янко Юрій Богданович"
#   subv$faction[subv$fullname == "Безбах Яків Якович"] <- "Позафракційні"
#   subv$faction[subv$fullname == "Микитась Максим Вікторович"] <- "Воля народу"
#   
#   subv$district[grepl("для навчальних закладів Святошинського району", subv$title)] <- 218
#   subv$fullname[grepl("для навчальних закладів Святошинського району", subv$title)] <- "Ар'єв Володимир Ігорович"
#   subv$faction[grepl("для навчальних закладів Святошинського району", subv$title)] <- "Блок Петра Порошенка"
#   
#   return(subv)
#   
# }

get_committees_open <- function(){
  posts <- read_csv("http://data.rada.gov.ua/ogd/mps/skl8/mp-posts_ids.csv")
  posts_ids <- read_tsv("http://data.rada.gov.ua/ogd/mps/skl8/mp-posts_unit.txt", 
                        locale(encoding = "windows-1251"), col_names = F, col_types = NULL) %>% 
    rename(unit_id = X1, unit = X2)
  mps <- read_csv("http://data.rada.gov.ua/ogd/mps/skl8/mps08-data.csv")
  
  posts_full <- posts %>% 
    left_join(mps[, c("id", "full_name")], by = c("mp_id" = "id")) %>% 
    mutate(unit_id = ifelse(is.na(parent_id), unit_id, parent_id)) %>% 
    left_join(posts_ids)
  
  committees <- posts_full %>% filter(unit_type %in% c("sct", "cmt")) %>% 
    mutate(unit = str_replace(unit, "Верховної Ради України ", "")) %>% 
    rename(fullname = full_name) %>% 
    distinct(fullname, unit)
  return(committees)
}

get_factions_open <- function(){
  posts <- read_csv("http://data.rada.gov.ua/ogd/mps/skl8/mp-posts_ids.csv")
  posts_ids <- read_tsv("http://data.rada.gov.ua/ogd/mps/skl8/mp-posts_unit.txt", 
                        locale(encoding = "windows-1251"), col_names = F, col_types = NULL) %>% 
    rename(unit_id = X1, unit = X2)
  mps <- read_csv("http://data.rada.gov.ua/ogd/mps/skl8/mps08-data.csv")
  
  factions_full <- posts %>% 
    left_join(mps[, c("id", "full_name", "district_num", "region_name")], by = c("mp_id" = "id")) %>% 
    left_join(posts_ids) %>% 
    filter(unit_type %in% c("grp", "fra")) %>% 
    mutate(district_num = as.character(district_num)) %>% 
    select(mp_id, full_name, unit)
  
  factions_df <-  mps %>% 
    filter(is.na(resignation_text)) %>% 
    select(id, full_name, district_num, region_name) %>% 
    left_join(factions_full, by = c("id" = "mp_id", "full_name")) %>% 
    mutate(unit = ifelse(is.na(unit), "Позафракційні", unit)) %>% 
    rename(faction = unit, fullname = full_name) %>% 
    mutate(faction = recode(faction,
                            `Група "Воля народу"` = "Воля народу",
                            `Група "Партія "Відродження"` = "Відродження",
                            `Група "Відродження"` = "Відродження",
                            `Група "Економічний розвиток"` = "Економічний розвиток",
                            `Фракція ПАРТІЇ "БЛОК ПЕТРА ПОРОШЕНКА"` = "Блок Петра Порошенка",
                            `Фракція політичної партії "Всеукраїнське об'єднання "Батьківщина" у Верховній Раді України` = "Батьківщина",
                            `Фракція Політичної партії "НАРОДНИЙ ФРОНТ"` = "Народний фронт",
                            `Фракція Політичної партії "Опозиційний блок" у Верховній Раді України восьмого скликання` = "Опозиційний блок",
                            `Фракція Радикальної партії Олега Ляшка` = "Радикальна партія Ляшка",
                            `Фракція Політичної партії "Об'єднання "САМОПОМІЧ"` = "Самопоміч"))
  return(factions_df)
}

get_voting_results_one <- function(number, v_name = NULL, old = F, convocation = 0){
  if(old == T){
    links <- paste0("http://w1.c1.rada.gov.ua/pls/radan_gs09/ns_arh_golos_print?g_id=",
                    number, "&vid=1&n_skl=", convocation)
  } else {
    
    links <- paste0("http://w1.c1.rada.gov.ua/pls/radan_gs09/ns_golos_print?g_id=",
                    number, "&vid=1")
  }
  
  html <- read_html(links, encoding = "Windows-1251")
  
  factions <- html %>% 
    html_nodes("center b") %>% 
    html_text()
  
  results <- html %>% 
    html_nodes("center") %>% 
    html_text()
  
  shortnames <- html %>% 
    html_nodes(".hcol1") %>% 
    html_text()
  
  votes <- html %>% 
    html_nodes(".hcol1+ td") %>% 
    html_text()
  
  descr <- html %>% 
    html_nodes(".f2") %>% 
    html_text()
  
  ndep_factions <- str_extract(results, "[:digit:]{1,3}")
  
  voting_df <- data.frame(shortname = shortnames, vote = votes,
                          faction = rep(factions, times = ndep_factions)) %>% 
    mutate(faction = str_replace(faction, "  ", " "),
           faction = recode(faction, 
                            `Група "Воля народу"` = "Воля народу",
                            `Група "Партія "Відродження"` = "Відродження",
                            `Група "Відродження"` = "Відродження",
                            `Група "Економічний розвиток"` = "Відродження",
                            `Фракція ПАРТІЇ "БЛОК ПЕТРА ПОРОШЕНКА"` = "Блок Петра Порошенка",
                            `Фракція політичної партії "Всеукраїнське об’єднання "Батьківщина"` = "Батьківщина",
                            `Фракція Політичної партії "НАРОДНИЙ ФРОНТ"` = "Народний фронт",
                            `Фракція Політичної партії "Опозиційний блок"` = "Опозиційний блок",
                            `Фракція Радикальної партії Олега Ляшка` = "Радикальна партія Ляшка",
                            `Фракція Політичної партії "Об’єднання "САМОПОМІЧ"` = "Самопоміч"),
           vote_simple = ifelse(vote == "За", 1, 0),
           shortname = as.character(shortname)) %>% 
    mutate(shortname = str_replace(shortname, "’", "'")) %>% 
    mutate(shortname = str_replace(shortname, fixed(" ."), ""))
  
  voting_df$shortname[voting_df$shortname == "Тимошенко Ю.В." &
                        voting_df$faction == "Народний фронт"] <- "Тимошенко Юрій В."
  voting_df$shortname[voting_df$shortname == "Тимошенко Ю.В." &
                        voting_df$faction == "Батьківщина"] <- "Тимошенко Юлія В."
  
  if(!missing(v_name)){
    voting_df <- mutate(voting_df, v_name = v_name)
  }
  
  return(voting_df)
  Sys.sleep(0.05)
}

get_voting_results <- function(number, v_name){
  if(length(number) == 1){
    voting_df <- get_voting_results_one(number, v_name)
  } else {
    voting_df <- map2(number, v_name, get_voting_results) %>% 
      bind_rows()
  }
  return(voting_df)
}

# tymoshenko <- function(df){
#   
#   df <- df %>% 
#     mutate(shortname = ifelse(shortname == "Тимошенко Ю.В." & faction == "Народний фронт",
#                               "Тимошенко Юрій В.",  ifelse(shortname == "Тимошенко Ю.В." & faction == "Батьківщина",
#                                                            "Тимошенко Юлія В.", shortname)))
#   return(df)
# }

add_voting_as_var <- function(df, v_number, title){
  
  voting <- get_voting_results(v_number) %>% 
    left_join(mps, by = "shortname") %>% 
    select(fullname, vote_simple)
  
  colnames(voting) <- c("fullname", title)
  
  df <- df %>% 
    left_join(voting[, c("fullname", title)])
  
}

compare_by_var <- function(df, var){
  
  df_agg <- df %>% 
    group_by_(var) %>% 
    summarize(mean = mean(total),
              median = median(total))
  return(df_agg)
}
