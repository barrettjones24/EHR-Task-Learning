######################################################################################
## Newborn Data Processing, Summary Stats, and Event Deviance  (Data not includeed) ##
######################################################################################

library(ggplot2)
library(dplyr)
#### read in data
emp_dept    <- read.csv('data/employee_dept.csv')
logs        <- read.csv('data/infant-logs.csv')
health_cond <- read.csv('data/Inpatient_health conditions.csv')
demo        <- read.csv('data/patient-Demo.csv')
event       <- read.csv('data/unique event.csv')
read        <- read.csv('data/readmissions.csv')
prov        <- read.csv('data/providers.csv')

#### subset data
## join descriptions to logs
logs_desc <- merge(logs, event, by.x = 'COLUMN_', by.y = 'ID')

## create base dataset for newborns
read$LOS       <- as.numeric(as.character(as.Date(read$VISIT_END_DATETIME) - as.Date(read$VISIT_START_DATETIME)))
read$LOS_hours <- difftime(read$VISIT_END_DATETIME, read$VISIT_START_DATETIME, units = 'hours')
nb             <- read[read$NEWBORN_FLAG == 1,]
nb_los         <- nb[nb$LOS <= 3,] ## subset to patients w los <= 3

emp_dept_u <- emp_dept %>% 
  group_by(EMPLOYEE_ID) %>%
  summarise(DEPTID     = paste(DEPTID, collapse = ','),
            DEPT_DESCR = paste(DEPT_DESCR, collapse = ',')
            ) 

logs_nb        <- logs_desc[logs_desc$PERSON_ID %in% nb_los$PERSON_ID,] ## subset logs to nb
logs_nb2       <- merge(logs_nb, emp_dept_u, by = 'EMPLOYEE_ID') ## get emp data
dept_list      <- c('General Pediatrics', 'Pediatrics-Housestaff')
logs_nb3       <- logs_nb2[logs_nb2$DEPT_DESCR %in% dept_list,] ## subset to dept of interest

nb_los2 <- nb_los[nb_los$PERSON_ID %in% unique(logs_nb3$PERSON_ID),]
nb_los3 <- merge(nb_los2, demo, by = 'PERSON_ID')

## summary stats
prop.table(table(nb_los3$GENDER_SOURCE_VALUE))
prop.table(table(as.character(nb_los3$RACE_SOURCE_VALUE)))

## sort the logs and calc time spent on each task

logs_nb3$DATE_fixed  <- as.POSIXct(gsub('\\"', '',as.character(logs_nb3$ACCESS_DATETIME))) ## fix date formatting
log_sort             <- logs_nb3[order(logs_nb3$EMPLOYEE_ID, logs_nb3$PATIENT_ID, logs_nb3$PERSON_ID, logs_nb3$ACCESS_DATETIME),]
log_sort$event_index <- 1:nrow(log_sort)

log_sort$n_minus_1 <- log_sort$event_index - 1
m_times <- merge(log_sort, log_sort, 
                 by.x = c('PATIENT_ID', 'PERSON_ID', 'EMPLOYEE_ID','event_index'),
                 by.y = c('PATIENT_ID', 'PERSON_ID', 'EMPLOYEE_ID','n_minus_1'),
                 all.x = T)
m_times$time_spent <- m_times$DATE_fixed.y - m_times$DATE_fixed.x

# id sessions
session <- NULL
repeats <- NULL
ev      <- NULL
N       <- nrow(m_times)
s_num   <- 1
for(i in 1:N){
  session[i] <- s_num
  repeats[i] <- ifelse(m_times$COLUMN_.x[i] %in% ev, 1, 0)
  ev         <- cbind(ev, m_times$COLUMN_.x[i])
  if(m_times$time_spent[i] >= 300 | is.na(m_times$time_spent[i])){ 
    s_num <- s_num + 1
    ev    <- NULL
  }
  
}

m_times$session <- session
m_times$repeats <- repeats

## create dataset to use in py clustering
df_out <- data.frame(m_times$PATIENT_ID, 
                     m_times$EMPLOYEE_ID,
                     m_times$DEPTID.x, 
                     m_times$COLUMN_.x, 
                     paste(m_times$DEPTID.x, m_times$COLUMN_.x, sep = '_'),
                     m_times$ACCESS_DATETIME.x,
                     m_times$session)
colnames(df_out) <- c('PATIENTID', 'EMPLOYEE_ID', 'DEPTID', 'EVENTID', 'DEPTID_EVENTID', 'TIMESTAMP', 'SESSION')

write.csv(df_out, 'event_codes.csv') ## output csv

## get summary stats
## session level stats

m_times$DATE <- as.Date(m_times$DATE_fixed.x)
# create times column
m_times$time_new <- ifelse(m_times$time_spent >= 300, NA, m_times$time_spent) ## NA if over 5 min event time 

# by dept event stats
by_dept <- m_times %>% 
  group_by(DEPT_DESCR.x) %>%
  summarise(mean_ev  = mean(time_new, na.rm = T),
            sd_ev = sd(time_new, na.rm = T),
            med_ev  = median(time_new, na.rm = T),
            iqr_ev = quantile(time_new, 0.75, na.rm = T) - quantile(time_new, 0.25, na.rm = T)
            ) 

# by_vis_ses
by_vis_ses <- m_times %>% 
  group_by(PERSON_ID, PATIENT_ID, EMPLOYEE_ID, DEPT_DESCR.x, DATE, session) %>%
  summarise(t_ev  = sum(time_new, na.rm = T),
            mean_ev = mean(time_new, na.rm = T),
            n_rep = sum(repeats) / (length(repeats) - 1), ## subtract one to remove the first in a session, bc can't be repeated
            n     = length(COLUMN_.x),
            n_un  = length(unique(COLUMN_.x))) 

## dept session stats



## by dept session stats

by_dept_sess <- by_vis_ses %>%
  group_by(DEPT_DESCR.x) %>%
  summarise(n_ses = length(session),
            n_ev = sum(n),
            ev_ses = mean(n),
            total_time = sum(t_ev),
            mean_time = mean(t_ev),
            sd_time = sd(t_ev),
            median_time = median(t_ev),
            Q1_time = quantile(t_ev, 0.25),
            Q3_time = quantile(t_ev, 0.75))

write.csv(by_dept_sess, 'by_dept_sess.csv')

## test time diff at session level
t.test(t_ev ~ DEPT_DESCR.x, data = by_vis_ses)
wilcox.test(t_ev ~ DEPT_DESCR.x, data = by_vis_ses) 
## test time diff at event level
t.test(time_new ~ DEPT_DESCR.x, data = m_times, na.action = 'na.omit')
wilcox.test(time_new ~ DEPT_DESCR.x, data = m_times, na.action = 'na.omit') 

# logit reg to estimate OR
m_times$ped_house  <- ifelse(m_times$DEPT_DESCR.x == 'Pediatrics-Housestaff', 1, 0)
m_times$event_type <- factor(m_times$COLUMN_.x)
mod                <- glm(ped_house ~ event_type, data = m_times, family = binomial())
mdf                <- data.frame(coef(summary(mod)))
mdf$event          <- (rownames(mdf))
mdf$evshort        <- gsub('event_type', '', mdf$event)
mdf$OR             <- exp(mdf$Estimate)

## merge event descriptions
mdf2 <- merge(mdf, event, 
              by.x = 'evshort',
              by.y = 'ID')

## merge event counts
mdf3 <- merge(mdf2, tdf,
              by.x = 'evshort',
              by.y = 'event')

mdf3$GP_rate <- mdf3$GP_n / sum(mdf3$GP_n)
mdf3$PH_rate <- mdf3$PH_n / sum(mdf3$PH_n)

write.csv(mdf3, 'event_deviance.csv') ## output event deviance results
