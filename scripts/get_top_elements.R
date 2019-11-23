library(tidyverse)
library(janitor)

file_names <- list.files('data/onet_txt')
onet <- tibble(filename = file_names) %>% 
  mutate(data = map(filename, ~clean_names(read_delim(str_glue('data/onet_txt/{.}'), delim = '\t'))))

onet %>% mutate(rows = map_dbl(data, nrow)) %>% select(filename, rows) %>% View()


# Need to handle scales that are 0 based
dividers <- tribble(~scale_id, ~divider,
                    'AO', 5,
                    'CF', 5,
                    'CN', 5,
                    'CT', 3,
                    'CTP', 3,
                    'CX', 5,
                    'CXP', 5,
                    'EX', 7,
                    'FM', 5,
                    'FT', 7,
                    'HW', 3,
                    'IH', 6,
                    'IJ', 5,
                    'IM', 5,
                    'IP', 5,
                    'LC', 5,
                    'LV', 7,
                    'OI', 7,
                    'OJ', 9,
                    'PT', 9,
                    'PX', 5,
                    'RE', 5,
                    'RL', 12,
                    'RT', 100,
                    'RW', 11,
                    'SR', 5,
                    'TI', 5,
                    'VH', 6,
                    'WS', 3)


get_df <- function(filen){
  onet %>% 
    filter(str_detect(filename, filen)) %>% 
    select(data) %>% 
    unnest(data)
}

get_df('Work Context.txt') %>% 
  View()

get_df('Job Zones.txt') %>% View()

occupation_data <- get_df('Occupation Data.txt')

career_changers_matrix <- get_df('Career Changers Matrix')

expanded_matrix <- career_changers_matrix %>% 
  left_join(occupation_data %>% 
              select(o_net_soc_code, title),
            by = 'o_net_soc_code') %>% 
  left_join(occupation_data %>% 
              select(o_net_soc_code, related_title = title),
            by = c('related_o_net_soc_code' = 'o_net_soc_code')) %>% 
  left_join(get_df('Job Zones.txt') %>% select(o_net_soc_code, job_zone),
            by = 'o_net_soc_code') %>% 
  left_join(get_df('Job Zones.txt') %>% select(o_net_soc_code, related_job_zone = job_zone),
            by = c('related_o_net_soc_code' = 'o_net_soc_code'))

expanded_matrix %>% 
  group_by(job_zone) %>% 
  count(related_job_zone) %>% 
  ggplot(aes(x = related_job_zone, y = n)) +
  geom_col() +
  theme_minimal() + 
  facet_wrap(~job_zone)


context_category <- function(df){
  init_df <- get_df(df) %>% 
    select(o_net_soc_code, scale_id, element_name, category, data_value)
  
  na_cat <- init_df %>% 
    filter(category == 'n/a')%>% 
    left_join(dividers, by = 'scale_id') %>% 
    mutate(data_value = data_value / divider) %>% 
    select(-divider, -scale_id, -category)
  
  non_na_cat <- init_df  %>% 
    filter(category != 'n/a') %>% 
    group_by(o_net_soc_code, scale_id, element_name) %>% 
    summarize(data_value = category[which(data_value == max(data_value))[1]]) %>% 
    ungroup() %>% 
    left_join(dividers, by = 'scale_id') %>% 
    mutate(data_value = as.numeric(data_value) / divider) %>% 
    select(-divider, -scale_id)
  
  bind_rows(na_cat, non_na_cat) %>% 
    group_by(o_net_soc_code, element_name) %>% 
    summarize(data_value = mean(data_value)) %>% 
    ungroup()
}

non_context_category <- function(df){
  get_df(df) %>% 
    select(o_net_soc_code, scale_id, element_name, data_value) %>% 
    left_join(dividers, by = 'scale_id') %>% 
    mutate(data_value = as.numeric(data_value) / divider) %>% 
    select(-divider, -scale_id) %>% 
    group_by(o_net_soc_code, element_name) %>% 
    summarize(data_value = mean(data_value)) %>% 
    ungroup()
}

# get_df('Task Ratings.txt') %>% 
#   left_join(get_df('Task Statements.txt'))

data_features <- bind_rows(
  non_context_category('Abilities.txt') %>% mutate(dataset = 'abilities', domain = 'knowledge'),
  non_context_category('Skills.txt') %>% mutate(dataset = 'skills', domain = 'knowledge'),
  non_context_category('Knowledge.txt') %>% mutate(dataset = 'knowledge', domain = 'knowledge'),
  non_context_category('Interests.txt') %>% mutate(dataset = 'interests', domain = 'work_style'),
  non_context_category('Work Values.txt') %>% mutate(dataset = 'work_values', domain = 'work_style'),
  non_context_category('Work Styles') %>% mutate(dataset = 'work_styles', domain = 'work_style'),
  non_context_category('Work Activities') %>% mutate(dataset = 'work_activities', domain = 'work_style'),
  context_category('Work Context.txt')  %>% mutate(dataset = 'work_context', domain = 'work_style')
)


get_elements <- function(soc){
  data_features %>% 
    filter(o_net_soc_code == soc) %>%
    nest(-domain) %>%
    mutate(data = map(data, function(df){
      df %>% 
        arrange(desc(data_value)) %>% 
        head(10) %>% 
        mutate(rank = 1:nrow(.))
    })) %>% 
    unnest(data) %>% 
    arrange(desc(data_value))
}

top_elements <- occupation_data %>% 
  mutate(elements = map(o_net_soc_code, get_elements)) %>% 
  unnest(elements) %>% 
  select(-o_net_soc_code1)

write_csv(top_elements, 'data/top_elements_by_soc.csv')

#### A test for Ryane
ryane_skills <- top_elements %>% 
  filter(o_net_soc_code == '11-9151.00',
         element_name != 'Telephone') %>% 
  pull(element_name)


top_elements %>% 
  mutate(ryane = element_name %in% ryane_skills) %>% 
  group_by(o_net_soc_code) %>% 
  count(ryane) %>% 
  ungroup() %>% 
  filter(ryane == T) %>% 
  arrange(desc(n)) %>% 
  left_join(occupation_data) %>% View()
