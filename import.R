library(tidyverse)
library(REDCapR)
library(lubridate)


# Загрузка данных ---------------------------------------------------------
uri     <- "http://redcap.fccho-moscow.ru/api/"
token   <- "9577A86D1421413C586D52B5AB35F5C4"
data_redcap    <- REDCapR::redcap_read(redcap_uri=uri, token=token, raw_or_label = "label")$data


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
data_first[, 'relapse'] = '1' # Данные по пациентам, которых в базе нет, рассматриваем первичный случай
data_recidive <- data_not_base %>% 
  select(first_name,
         last_name,
         middle_name,
         sex,
         birthdate,
         mgroup,
         hist,
         ds_dt = relapse_dt,
         event,
         event_date,
         outcome,
         outcome_date)  %>%  
  mutate(record_id = data_first$record_id)# Данные по пациентам, которых в базе нет, рассматриваем рецидив

data_base_first <- data_base %>% mutate(record_id = paste(record_id,'-R1',sep=''))
data_base <- data_base %>% mutate(record_id = paste(record_id))


# Собираем таблицу ---------------------------------------------------------

data <- data_first

length(t(unique(data[!is.na(data$record_id),"record_id"]))) == nrow(data[!is.na(data$record_id),])
data[duplicated(data),]

data$sex <- recode(data$sex, 'М' = 'M', 'Ж' = 'F')
data[str_starts(data$outcome, 'жив'), 'outcome'] = 'NO'
data[str_starts(data$outcome, 'умер'), 'outcome'] = 'DTH'

data <- data %>% 
  mutate(relapse = coalesce(relapse, event))
data$relapse <- recode(data$relapse, 'рецидив' = '1', '1' = '1' , .default = NA_character_)


data$location___h <- as.integer(str_detect(data$location,'ЧМ'))
data$location___4v <- as.integer(str_detect(data$location,'4Ж'))
data$location___v <- as.integer(str_detect(data$location,'ГС'))
data$location <- NULL

data$hist <- recode(data$hist, 'КЛАСС' =	'CLASS',
                        'НОД' =	'NOD',
                        'ЭКСТ НОД' = 'EXTNOD',
                        'АНАПЛ' =	'LCA')

data <- data %>% 
    select(record_id,	first_name,	last_name,	middle_name,	sex,	birthdate,	mgroup,	hist,	ds_dt,	event,
           event_date,	outcome,	outcome_date,	relapse,	location___h,	location___4v,	location___v)



# для рецидивов --------------------------------------------

data_recidive <- bind_rows(data_recidive,data_base)

data_recidive$event <-  recode(data_recidive$event, 
                               'нет событий' = '0',
                               'смерть от осложнений лечения' = '2',
                               'non-responder/прогрессия' = '5',
                               'рецидив' = '3',
                               'ранняя летальность' = '6')
data_recidive$sex <- recode(data_recidive$sex, 'М' = 'M', 'Ж' = 'F')
data_recidive[str_starts(data_recidive$outcome, 'жив'), 'outcome'] = 'NO'
data_recidive[str_starts(data_recidive$outcome, 'умер'), 'outcome'] = 'DTH'

data_recidive$hist <- recode(data_recidive$hist, 'КЛАСС' =	'CLASS',
                    'НОД' =	'NOD',
                    'ЭКСТ НОД' = 'EXTNOD',
                    'АНАПЛ' =	'LCA')

data_recidive$location___h <- as.integer(str_detect(data_recidive$location,'ЧМ'))
data_recidive$location___4v <- as.integer(str_detect(data_recidive$location,'4Ж'))
data_recidive$location___v <- as.integer(str_detect(data_recidive$location,'ГС'))
data_recidive$location <- NULL



# Записываем файлы -------------------------------------

write_csv(data, 'data_medull.csv')
openxlsx::write.xlsx(data, 'data_medull.xlsx')


write_csv(data_recidive, 'data_medull_rel.csv')
openxlsx::write.xlsx(data_recidive, 'data_medull_rel.xlsx')
