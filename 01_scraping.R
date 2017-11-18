library(tidyverse)
library(rvest)
library(stringr)

get_data_by_gemeente <- function(code){
  padded_code <- str_sub(paste0("00", code), -3, -1)
  full_page <- 
    read_html(glue::glue("https://www.rtlnieuws.nl/verkiezingen?dnaId={padded_code}&electionCode=TK17"))
  gemeente    <- html_nodes(full_page, ".inhabitants") %>% html_text()
  list(
    gemeente    = get_gemeente_name(full_page),
    inhabitants = get_inhabitants(gemeente),
    province    = get_province(gemeente),
    elections_table = get_table(full_page))
}

get_inhabitants <- function(gemeente_str) {
  str_match(gemeente_str, "inwoners: (.*)")[2] %>% as.numeric()
}

get_province <- function(gemeente_str) {
  str_match(gemeente_str, "provincie: (.*)\n")[2]
}

get_table <- function(full_page){
  table <- html_table(full_page)[[1]]
  colnames(table) <- c("party", "votes", "prop_votes", "delta_2012")
  table %>% 
    mutate(prop_votes = clean_commas(prop_votes),
           delta_2012 = clean_commas(delta_2012))
}

clean_commas <- function(x) {
  x %>% str_replace(",", ".") %>% 
    str_match("(.*)%") %>% 
    `[`(,2) %>% 
    as.numeric()
}

get_gemeente_name <- function(full_page) {
 html_nodes(full_page, "h1") %>% html_text() %>% `[`(2) %>% 
    str_match("Gemeente (.*) -") %>% `[`(2)
}

get_all_gemeentes <- function(nrs = 1:388) {
  result <- vector("list", length(nrs))
  for (i in nrs) {
    result[[i]] <- get_data_by_gemeente(i)
    Sys.sleep(time = 5)
    print(paste0("Completed ", i))
  }
  result
}

raw_data <- get_all_gemeentes(1:2)

get_results_set <- function(raw_data){
  raw_data %>% 
    map_df(~elections_table %>% mutate(gemeente = .x$gemeente)) %>% 
    select(gemeente, everything())
}

get_gemeente_set <- function(raw_data, results_sets) {
  raw_data %>% 
    map_df(~ data_frame(gemeente    = .x$gemeente, 
                        inhabitants = .x$inhabitants, 
                        province    = .x$province))
}

add_votes_casted <- function(results, gemeentes) {
  results %>% 
    group_by(gemeente) %>% 
    summarise(votes_casted = sum(votes)) %>% 
    left_join(gemeentes) %>% 
    mutate(percentage_voted = votes_casted / inhabitants)
}

add_votes_casted(get_results_set(raw_data),
                 get_gemeente_set(raw_data))
