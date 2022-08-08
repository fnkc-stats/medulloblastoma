library(tidyverse)
library(REDCapR)


# Загрузка данных ---------------------------------------------------------
data_redcap    <- read_csv('_DATA_2022-08-08_1325.csv')

data <- readxl::read_xlsx('общая база.xlsx')
data <- data %>% 
  select('ФИО пациента',
         'Пол',
         'Дата.рожд.',
         'Дата установления первичного диагноза МБ',
         'Дата рецидива (не событие)',
         '1-е событие',
         'Дата события',
         'Исход',
         'Дата исхода') 


# Переименовываем столбцы ---------------------------------------------------------
data <- data %>%
  select('ФИО пациента') %>%
  t(.) %>%
  str_split(., " ",simplify = TRUE) %>%
  data_frame(.) %>%
  mutate(middle_name = paste(.[,3],.[,4]), last_name = .[,1], first_name = .[,2]) %>%
  select(first_name,last_name,middle_name) %>%
  bind_cols(.,data)
data['ФИО пациента'] <- NULL


# Ищем пациентов, которые есть в базе ---------------------------------------------------------
data_double <- data %>% 
  select(first_name,last_name,middle_name)
data_one <- data_redcap %>% 
  select(record_id,first_name,last_name,middle_name) %>% 
  left_join(data_double, 'last_name') %>% 
  .[str_sub(.$first_name.x,1,1) == str_sub(.$first_name.y,1,1) & str_sub(.$middle_name.x,1,1) == str_sub(.$middle_name.y,1,1),] %>% 
  .[!is.na(.$first_name.x),]
data <- data %>% left_join(select(data_one, record_id, last_name, first_name = first_name.y, middle_name = middle_name.y))

# Фильтруем по присутствию в базе ---------------------------------------------------------

data_base <- data %>% 
  filter(!is.na(.$record_id)) %>% 
  select(first_name,
         last_name,
         middle_name,
         'Пол',
         'Дата.рожд.',
         'Дата установления первичного диагноза МБ' = 'Дата рецидива (не событие)',
         '1-е событие',
         'Дата события',
         'Исход',
         'Дата исхода') # Данные по пациентам, которые уже есть в базе
data_not_base <- data %>% filter(is.na(.$record_id))
date_first <- data_not_base %>% 
  select(first_name,
         last_name,
         middle_name,
         'Пол',
         'Дата.рожд.',
         'Дата установления первичного диагноза МБ',
         '1-е событие',
         'Дата события' = 'Дата рецидива (не событие)',
         'Исход',
         'Дата исхода') 
date_first[,'1-е событие'] = 'рецидив' # Данные по пациентам, которых в базе нет, рассматриваем первичный случай
date_recidive <- data_not_base %>% 
  select(first_name,
         last_name,
         middle_name,
         'Пол',
         'Дата.рожд.',
         'Дата установления первичного диагноза МБ' = 'Дата рецидива (не событие)',
         '1-е событие',
         'Дата события',
         'Исход',
         'Дата исхода') # Данные по пациентам, которых в базе нет, рассматриваем рецидив


# Собираем таблицу ---------------------------------------------------------

data <- bind_rows(data_base,
          date_first,
          date_recidive)


labels <- c(
  'first_name' = 'first_name',
  'last_name' = 'last_name',
  'middle_name' = 'middle_name',
  'Пол' = 'sex',
  'Дата.рожд.' = 'birthdate',
  'Дата установления первичного диагноза МБ' = 'ds_dt',
  '1-е событие' = 'event',
  'Дата события' = 'event_date',
  'Исход' = 'outcome',
  'Дата исхода' = 'outcome_date'
)

data <- data %>% rename_with(~ labels[.])
write_csv(data, 'data_medull.csv')
