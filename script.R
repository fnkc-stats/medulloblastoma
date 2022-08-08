library(tidyverse)
library(REDCapR)


# Загрузка данных ---------------------------------------------------------
data_redcap    <- read_csv('_DATA_2022-08-08_1325.csv')

data <- readxl::read_xlsx('общая база.xlsx')
data <- data %>% 
  select('ФИО пациента',
         sex = 'Пол',
         birthdate = 'Дата.рожд.',
         ds_dt = 'Дата установления первичного диагноза МБ',
         relapse_dt = 'Дата рецидива (не событие)',
         event = '1-е событие',
         event_date = 'Дата события',
         outcome = 'Исход',
         outcome_date = 'Дата исхода') 


# Переименовываем столбцы ---------------------------------------------------------
data <- data %>%
  mutate(name = str_split(`ФИО пациента`, " ", n = 3, simplify = T)) %>%
  mutate(last_name = name[,1], first_name = name[,2], middle_name = name[,3])


# Ищем пациентов, которые есть в базе ---------------------------------------------------------
data_double <- data %>% 
  select(first_name,
         last_name,
         middle_name,
         sex,
         birthdate,
         ds_dt,
         relapse_dt,
         event,
         event_date,
         outcome,
         outcome_date) %>% 
  mutate(pat_fio = paste(last_name,str_sub(`first_name`,1,1),gsub("-", "",birthdate), sep = "_"))
data_one <- data_redcap %>%
  select(record_id,
         first_name,
         last_name,
         birthdate) %>%
  mutate(pat_fio = paste(last_name,str_sub(`first_name`,1,1),gsub("-", "",birthdate), sep = "_")) %>% 
  select(record_id,pat_fio)
data_double <- data_double %>% left_join(data_one, by = 'pat_fio')

# Фильтруем по присутствию в базе ---------------------------------------------------------

data_base <- data_double %>% 
  filter(!is.na(record_id)) %>% 
  select(first_name,
         last_name,
         middle_name,
         sex,
         birthdate,
         ds_dt = relapse_dt,
         event,
         event_date,
         outcome,
         outcome_date) # Данные по пациентам, которые уже есть в базе
data_not_base <- data_double %>% filter(is.na(record_id))
date_first <- data_not_base %>% 
  select(first_name,
         last_name,
         middle_name,
         sex,
         birthdate,
         ds_dt,
         event,
         event_date = relapse_dt,
         outcome,
         outcome_date) 
date_first[,'1-е событие'] = 'рецидив' # Данные по пациентам, которых в базе нет, рассматриваем первичный случай
date_recidive <- data_not_base %>% 
  select(first_name,
         last_name,
         middle_name,
         sex,
         birthdate,
         ds_dt = relapse_dt,
         event,
         event_date,
         outcome,
         outcome_date) # Данные по пациентам, которых в базе нет, рассматриваем рецидив


# Собираем таблицу ---------------------------------------------------------

data <- bind_rows(data_base,
          date_first,
          date_recidive)

write_csv(data, 'data_medull.csv')
