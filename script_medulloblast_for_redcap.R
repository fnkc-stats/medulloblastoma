library(tidyverse)
library(REDCapR)
library(lubridate)


# Загрузка данных ---------------------------------------------------------
uri     <- "http://redcap.fccho-moscow.ru/api/"
token   <- "9577A86D1421413C586D52B5AB35F5C4"
data_redcap    <- REDCapR::redcap_read(redcap_uri=uri, token=token, raw_or_label = "label")$data

data <- readxl::read_xlsx('общая база.xlsx', na = c("","?"))

data_rel <- data %>% #для вторых рецидивов
  select('ФИО пациента',
         sex              = 'Пол',
         birthdate        = 'Дата.рожд.',
         outcome_code     = 'Код исхода',	
         event            = '1-е событие',
         outcome          = 'Исход',
         outcome_date     = 'Дата исхода',
         ds_dt            = 'Дата события',
         location         = 'Локализация рецидива (событие)',
         r_stage         	=	'Объем операции при рецидиве (не событие)',
         operation	    	= 'Операция при рецидиве (событие) да/нет',
         date         		=	'Дата операции при рецидиве (событие)',
         drt_is		        =	'ЛТ при рецидиве (событие) да/нет',
         drt			        =	'ЛТ при рецидиве (событие) первичная/повторная',
         drt_type	        =	'Вид ЛТ при рецидиве (событие) протоны/фотоны/РХ',
         drt_dt		        =	'Дата начала ЛТ при рецидиве (событие)',
         drt_vol	        =	'Объем ЛТ при рецидиве (событие)',
         drt_dose       	=	'Доза ЛТ КСО при рецидиве (событие) Гр',
         drt_bust_dose	  =	'Доза ЛТ локально/буста при рецидиве (событие) Гр',
         drt_end_stop	    =	'Дата окончания ЛТ при рецидиве (событие)',
         ct_is	        	=	'ХТ при рецидиве (событие) да/нет',
         ct_start_dt	    =	'Дата начала ХТ при рецидиве (событие)',
         ct_type      		=	'Вид противорецидивной ХТ (событие)',
         ct_num_blok	    =	'Кол-во блоков противорецидивной ХТ (событие)',
         ct_end_dt	      =	'Дата окончания ХТ при рецидиве (событие)',
         ct_tgsk		      =	'ВДХТ с ауто-ТГСК при рецидиве (событие) да/нет',
         ct_notes     		=	'Особенности ВДХТ с ауто-ТГСК при рецидиве (событие)',
         memmat_is	      =	'МЕММАТ при рецидиве (событие) да/нет',
         memmat_start_dt	=	'Дата начала МЕММАТ при рецидиве (событие)',
         memmat_end_dt	  =	'Дата окончания МЕММАТ при рецидиве (событие)',
         memmat_dur		  	=	'Длительность МЕММАТ при рецидиве (событие)',
         memmat_notes	    =	'Особенности МЕММАТ при рецидиве (событие)') %>% 
  mutate_if(lubridate::is.instant, lubridate::date)

data <- data %>% 
  select('ФИО пациента',
         sex              = 'Пол',
         birthdate        = 'Дата.рожд.',
         ds_dt            = 'Дата рецидива (не событие)',
         event            = '1-е событие',
         event_date       = 'Дата события',
         outcome          = 'Исход',
         outcome_date     = 'Дата исхода',
         outcome_code     = 'Код исхода',
         location         = 'Локализация рецидива (не событие)',
         relapse_type     = 'Локализация рецидива (событие)',
         clinic_tr		    =	'Место проведения противорецидивного лечения',
         r_stage     			=	'Объем операции при рецидиве (событие)',
         operation	    	= 'Операция при рецидиве (не событие) да/нет',
         date         		=	'Дата операции при рецидиве (не событие)',
         drt_is		        =	'ЛТ при рецидиве (не событие) да/нет',
         drt			        =	'ЛТ при рецидиве (не событие) первичная/повторная',
         drt_type	        =	'Вид ЛТ при рецидиве (не событие) фотоны/протоны/РХ',
         drt_dt		        =	'Дата начала ЛТ при рецидиве (не событие)',
         drt_vol	        =	'Объем ЛТ при рецидиве (не событие)',
         drt_dose       	=	'Доза ЛТ КСО при рецидиве (не событие) Гр',
         drt_bust_dose	  =	'Доза ЛТ буста при рецидиве (не событие) Гр',
         drt_end_stop	    =	'Дата окончания ЛТ при рецидиве (не событие)',
         ct_is	        	=	'ХТ при рецидиве (не событие) да/нет',
         ct_protocol	    =	'Протокол ХТ при рецидиве (не событие)',
         ct_start_dt	    =	'Дата начала ХТ при рецидиве (не событие)',
         ct_type      		=	'Вид противорецидивной ХТ (не событие)',
         ct_num_blok	    =	'Кол-во блоков противорецидивной ХТ (не событие)',
         ct_end_dt	      =	'Дата окончания ХТ при рецидиве (не событие)',
         ct_tgsk		      =	'ВДХТ с ауто-ТГСК при рецидиве (не событие) да/нет',
         ct_notes     		=	'Особенности ВДХТ с ауто-ТГСК при рецидиве (не событие)',
         memmat_is	      =	'МЕММАТ при рецидиве (не событие) да/нет',
         memmat_start_dt	=	'Дата начала МЕММАТ при рецидиве (не событие)',
         memmat_end_dt	  =	'Дата окончания МЕММАТ при рецидиве (не событие)',
         memmat_dur		  	=	'Длительность МЕММАТ при рецидиве (не событие)',
         memmat_resp    	=	'Ответ на МЕММАТ при рецидиве (не событие)',
         memmat_notes	    =	'Особенности МЕММАТ при рецидиве (не событие)') %>% 
  mutate_if(lubridate::is.instant, lubridate::date)


# Переименовываем столбцы ---------------------------------------------------------
data <- data %>%
  mutate(name = str_split(`ФИО пациента`, " ", n = 3, simplify = T)) %>%
  mutate(last_name = name[,1], first_name = name[,2], middle_name = name[,3])

data_rel <- data_rel %>%
  mutate(name = str_split(`ФИО пациента`, " ", n = 3, simplify = T)) %>%
  mutate(last_name = name[,1], first_name = name[,2], middle_name = name[,3])%>%
  mutate(pat_fio = paste(last_name,str_sub(`first_name`,1,1),gsub("-", "",birthdate), sep = "_"))


# Фильтруем по присутствию в базе ---------------------------------------------------------

data_double <- data %>% 
  mutate(pat_fio = paste(last_name,str_sub(`first_name`,1,1),gsub("-", "",birthdate), sep = "_"))
data_one <- data_redcap %>%
  select(record_id,
         first_name,
         last_name,
         birthdate,
         his_id) %>%
  mutate(pat_fio = paste(last_name,str_sub(`first_name`,1,1),gsub("-", "",birthdate), sep = "_")) %>% 
  select(record_id,pat_fio,his_id)
data_double <- data_double %>% left_join(data_one, by = 'pat_fio')

data_base <- data_double %>% 
  filter(!is.na(record_id)) # Данные по пациентам, которые уже есть в базе

x <- max(as.numeric(data_base$record_id))+1
y <- max(as.numeric(data_base$record_id)) + 
        length(data_first$first_name)

data_not_base <- data_double %>% filter(is.na(record_id))
data_recidive <- data_not_base %>%  
  mutate(record_id = as.character(paste(c(x:y),'-R1',sep='')))# Данные по пациентам, которых в базе нет, рассматриваем рецидив

data_base_first <- data_base %>% mutate(record_id = paste(record_id,'-R1',sep=''))


# Собираем таблицу для рецидивов --------------------------------------------

data_recidive <- bind_rows(data_recidive, data_base_first)
  

# Собираем таблицу для вторых рецидивов --------------------------------------------

data_recidive_imp <- data_recidive %>% 
  select(pat_fio,record_id)
  data_rel <- data_rel %>% right_join(data_recidive_imp, by = 'pat_fio') %>% 
  filter(event == 'рецидив') %>% 
  mutate(name = str_split(record_id, "-", n = 2, simplify = T)) %>% 
  mutate(record_id = paste(name[,1],'-R2', sep=''))
  
  data_recidive$pat_fio <- NULL
  data_recidive$'ФИО пациента' <- NULL
  data_recidive$name <- NULL
  data_rel$pat_fio <- NULL
  data_rel$'ФИО пациента' <- NULL
  data_rel$name <- NULL
  

# делаем данные под файлы -------------------------------------

data_recidive <- bind_rows(data_recidive, data_rel)
  
  data_recidive$event <-  recode(data_recidive$event, 
                                 'нет событий' = '0',
                                 'смерть от осложнений лечения' = '2',
                                 'non-responder/прогрессия' = '5',
                                 'рецидив' = '3',
                                 'ранняя летальность' = '6')
  data_recidive$sex <- recode(data_recidive$sex, 'М' = 'M', 'Ж' = 'F')
  data_recidive[str_starts(data_recidive$outcome, 'жив'), 'outcome'] = 'NO'
  data_recidive[str_starts(data_recidive$outcome, 'умер'), 'outcome'] = 'DTH'
  
  
  data_recidive <- data_recidive %>%
    relocate(record_id,1)
  
  data_recidive$location___loc <- as.integer(str_detect(data_recidive$location,'локальный'))
  data_recidive$location___mts <- as.integer(str_detect(data_recidive$location,'метастатический'))
  data_recidive$location___mix <- as.integer(str_detect(data_recidive$location,'смешанный'))
  data_recidive$location <- NULL
  
  data_recidive$relapse_type <- recode(data_recidive$relapse_type,
                                       'локальный' = 'LOC',
                                       'метастатический' = 'MTS',
                                       'смешанный' = 'MIX',
                                       .default = '',
                                       .missing = '')
  
  recode_is <- function(array_col){
    array_col <- recode(array_col,
                        'нет' = '0',
                        'да' = '1',
                        .default = '0')
  }
  
  data_recidive$operation <- recode_is(data_recidive$operation)
  data_recidive$drt_is <- recode_is(data_recidive$drt_is)
  data_recidive$ct_is <- recode_is(data_recidive$ct_is)
  data_recidive$ct_tgsk <- recode_is(data_recidive$ct_tgsk)
  data_recidive$memmat_is <- recode_is(data_recidive$memmat_is)
  
  data_recidive$drt <- recode(data_recidive$drt, 'первичная' = '1',
                              'повторная' = '2',
                              .missing = 'NI')
  NA_not_data_func <- function(str){
    NA_not_data <- as.character(str_starts(data_recidive$drt_type, str)) 
    NA_not_data <- as.logical(recode(NA_not_data, .missing = 'FALSE',
                                     'TRUE' = 'TRUE', 'FALSE' = 'FALSE'))
  }
  
  data_recidive[NA_not_data_func('фотоны'), 'drt_type'] = '1'
  data_recidive[NA_not_data_func('протоны'), 'drt_type'] = '2'
  data_recidive$drt_type[is.na(data_recidive$drt_type)] <- 'NI'

data_recidive$event_date <- as.character(data_recidive$event_date)
data_recidive$event_date[is.na(data_recidive$event_date)] <- ''

data_recidive[data_recidive$relapse_type != '', "relapse"] = '1'
data_recidive[data_recidive$relapse_type == '', "relapse"] = 'NI'

data_recidive <- data_recidive %>% 
  mutate(memmat_dur = abs(difftime(data_recidive$memmat_end_dt, data_recidive$memmat_start_dt, units = "days")))

data_recidive$ct_start_dt <- as.character(data_recidive$ct_start_dt)
data_recidive$date <- as.character(data_recidive$date)
data_recidive$drt_dt <- as.character(data_recidive$drt_dt)
data_recidive$drt_dose <- as.character(data_recidive$drt_dose)
data_recidive$drt_bust_dose <- as.character(data_recidive$drt_bust_dose)
data_recidive$drt_end_stop <- as.character(data_recidive$drt_end_stop)
data_recidive$ct_num_blok <- as.character(data_recidive$ct_num_blok)
data_recidive$memmat_start_dt <- as.character(data_recidive$memmat_start_dt)
data_recidive$memmat_end_dt <- as.character(data_recidive$memmat_end_dt)
data_recidive$ct_end_dt <- as.character(data_recidive$ct_end_dt)

data_recidive$memmat_dur <- as.character(data_recidive$memmat_dur)
data_recidive$his_id <- as.character(data_recidive$his_id)
data_recidive[is.na(data_recidive)] <- ''


# Записываем файлы -------------------------------------

write_csv(data_recidive, 'data_medull_rel.csv')
openxlsx::write.xlsx(data_recidive, 'data_medull_rel.xlsx')
