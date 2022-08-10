library(tidyverse)
library(REDCapR)
library(lubridate)


# Загрузка данных ---------------------------------------------------------
data_redcap    <- read_csv('_DATA_2022-08-10_1116.csv')

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
         outcome_date = 'Дата исхода',
         location = 'Локализация опухоли',
         location_r = 'Локализация рецидива (не событие)',
         mgroup = 'Молекулярная группа',
         hist = 'Гистологический вариант') %>% 
  mutate_if(lubridate::is.instant, lubridate::date)


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
         mgroup,
         location,
         location_r,
         hist,
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
  select(record_id,
         first_name,
         last_name,
         middle_name,
         sex,
         birthdate,
         mgroup,
         location,
         hist,
         ds_dt = relapse_dt,
         event,
         event_date,
         outcome,
         outcome_date) # Данные по пациентам, которые уже есть в базе
data_not_base <- data_double %>% filter(is.na(record_id))
data_first <- data_not_base %>% 
  select(first_name,
         last_name,
         middle_name,
         sex,
         birthdate,
         mgroup,
         location,
         hist,
         ds_dt,
         event,
         event_date = relapse_dt,
         outcome,
         outcome_date) 
data_first <- data_first %>%  add_column(record_id = as.character(c((
  max(data_base$record_id)+1):(max(data_base$record_id) + 
                                 length(data_first$first_name)))))
data_first[, 'event'] = 'рецидив' # Данные по пациентам, которых в базе нет, рассматриваем первичный случай
data_recidive <- data_not_base %>% 
  select(first_name,
         last_name,
         middle_name,
         sex,
         birthdate,
         mgroup,
         location = location_r,
         hist,
         ds_dt = relapse_dt,
         event,
         event_date,
         outcome,
         outcome_date)  %>%  
  mutate(record_id = paste(data_first$record_id,'-R1',sep=''))# Данные по пациентам, которых в базе нет, рассматриваем рецидив

data_base <- data_base %>% mutate(record_id = paste(record_id,'-R1',sep=''))


# Собираем таблицу ---------------------------------------------------------

data <- bind_rows(data_base,
                  data_recidive,
                  data_first)

length(t(unique(data[!is.na(data$record_id),"record_id"]))) == nrow(data[!is.na(data$record_id),])
data[duplicated(data),] # смотрим, нет ли дупликатов


# Перекодируем нужные столбцы в формат базы -----------------------------------------------

data$sex <- recode(data$sex, 'М' = 'M', 'Ж' = 'F')
data[str_starts(data$outcome, 'жив'), 'outcome'] = 'NO'
data[str_starts(data$outcome, 'умер'), 'outcome'] = 'DTH'

data <- data %>% 
  add_column(relapse = .$event)
data$relapse <- recode(data$relapse, 'рецидив' = '1', .default = '0')

data$location___h <- as.integer(str_detect(data$location,'ЧМ'))
data$location___4v <- as.integer(str_detect(data$location,'4Ж'))
data$location___v <- as.integer(str_detect(data$location,'ГС'))
data$location <- NULL

data$hist <- recode(data$hist, 'КЛАСС' =	'CLASS',
                        'НОД' =	'NOD',
                        'ЭКСТ НОД' = 'EXTNOD',
                        'АНАПЛ' =	'LCA')
                        
write_csv(data, 'data_medull.csv')
