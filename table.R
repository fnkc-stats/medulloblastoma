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
         outcome_date = 'Дата исхода',
         date = 'Дата первичной операции',
         mgroup = 'Молекулярная группа') 


# Переименовываем столбцы ---------------------------------------------------------
data <- data %>%
  mutate(name = str_split(`ФИО пациента`, " ", n = 3, simplify = T)) %>%
  mutate(last_name = name[,1], first_name = name[,2], middle_name = name[,3])



# Ищем пациентов, которые есть в базе ---------------------------------------------------------

data_one_new <- data_redcap %>%
  select(record_id,relapse,relapse_dt,event,event_date) %>% 
  filter(event == 'REL' & is.na(relapse)) # ищем, у кого не проставлен рецидив

data_double_first <- data %>% # Таблица с первичными записями
  select(first_name,
         last_name,
         birthdate,
         ds_dt,
         date,
         relapse_dt,
         mgroup) %>% 
  mutate(pat_fio = paste(last_name,str_sub(`first_name`,1,1),gsub("-", "",birthdate), sep = "_"))

data_double_rel <- data %>% # таблица с записями рецидивов
  select(first_name,
         last_name,
         birthdate,
         ds_dt,
         date,
         outcome,
         outcome_date)

data_double <- data_double_first %>% 
  bind_cols(data_double_rel) %>%
  add_column(table_id = c(1:max(row_number(data$sex))))

data_one <- data_redcap %>%
  select(record_id,
         first_name,
         last_name,
         birthdate,
         ds_dt,
         date,
         relapse,
         relapse_dt,
         mgroup,
         outcome,
         outcome_date) %>%
  mutate(pat_fio = paste(last_name,str_sub(`first_name`,1,1),gsub("-", "",birthdate), sep = "_"))
  data_one[data_one$record_id %in% data_one_new$record_id, 'relapse'] <-  '1'
  data_one[data_one$record_id %in% data_one_new$record_id, 'relapse_dt'] <- data_one_new$event_date
data_double <- data_one %>% 
  full_join(data_double, by = 'pat_fio') %>% 
  filter(!is.na(table_id) | (!is.na(record_id) & relapse == 1)) 

data_time <- data_double %>% 
  filter(!is.na(relapse_dt.x) & !is.na(ds_dt...12) & !is.na(date...13)) %>% 
  add_column(diff_date_1 = difftime(.$relapse_dt.x, .$ds_dt...12, units = "day")) %>% 
  add_column(diff_date_2 = difftime(.$relapse_dt.x, .$date...13, units = "day")) %>% 
  select(pat_fio,diff_date_1,diff_date_2)

data_double <- data_double %>% left_join(data_time) 

# Переименовываем столбцы ------------
labels <- c(record_id = 'record_id',
            first_name = 'Имя (база)',
            last_name = 'Фамилия (база)',
            birthdate = 'Дата рождения (база)',
            ds_dt = 'Дата установления первичного диагноза МБ (база)',
            date = 'Дата операции (база)',
            relapse = 'Рецидив (база)',
            relapse_dt.x = 'Дата рецидива (база)',
            mgroup.x = 'Молекулярная группа (база)',
            outcome.x = 'Исход (база)',
            outcome_date.x = 'Дата исхода (база)',
            pat_fio = 'Ключ',
            first_name...1 = 'Имя (таблица, первычный)',
            last_name...2 = 'Фамилия (таблица, первычный)',
            birthdate...3 = 'Дата рождения (таблица, первычный)',
            ds_dt...4 = 'Дата установления первичного диагноза МБ (таблица, первычный)',
            date...5 = 'Дата операции (таблица, первычный)',
            relapse_dt.y = 'Дата рецидива (таблица, первычный)',
            mgroup.y = 'Молекулярная группа (таблица, первычный)',
            first_name...9 = 'Имя (таблица, рецидив)',
            last_name...10 = 'Фамилия (таблица, рецидив)',
            birthdate...11 = 'Дата рождения (таблица, рецидив)',
            ds_dt...12 = 'Дата установления первичного диагноза МБ (таблица, рецидив)',
            date...13 = 'Дата операции (таблица)',
            outcome.y = 'Исход (таблица)',
            outcome_date.y = 'Дата исхода (таблица)',
            table_id = 'table_id',
            diff_date_1 = 'разница между датой диагноза и рецидива',
            diff_date_2 = 'разница между датой операции и рецидива')

data_double <- data_double %>% 
  select(record_id, table_id, pat_fio,	first_name,	last_name,	birthdate,	ds_dt,	date,	relapse,	relapse_dt.x,	mgroup.x,	
             outcome.x,	outcome_date.x,	first_name...1,	last_name...2,	birthdate...3,	ds_dt...4,	date...5,	relapse_dt.y,	mgroup.y,	
             first_name...9,	last_name...10,	birthdate...11,	ds_dt...12,	diff_date_1, date...13,	diff_date_2, outcome.y,	outcome_date.y)

data_double <- data_double %>% rename_with(~ labels[.])
openxlsx::write.xlsx(data_double, 'data.xlsx')
