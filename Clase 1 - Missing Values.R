# install pacman
if(!require(pacman)) install.packages("pacman") ; require(pacman) #

# require/install packages on this session

p_load(rio, # import/export data
       tidyverse, # tidy-data
       skimr, # summary data
       visdat, # visualizing missing data
       corrplot, # Correlation Plots 
       stargazer) # tables/output to TEX. 

## load data
df <- import("https://github.com/ignaciomsarmiento/datasets/blob/main/GEIH_sample1.Rds?raw=true")

#Cambiar el df a tibble hace más facil su manipulación con tidyverse 
db <- as_tibble(df) ## from dataframe to tibble 

## print data
head(db)
tail(db)

## summary db
skim(db) %>% head() #skimr da información del dataframe #solo nos salen 6 variables porque pusimos head

## summary var
summary(db$y_salary_m) 

des_vars <- c("y_salary_m", "sex", "age")
stargazer(df[des_vars], type="text") #saca tablas mas esteticas

## data + mapping
ggplot(data = db , mapping = aes(x = age , y = y_ingLab_m))

## + geometry
ggplot(data = db , mapping = aes(x = age , y = y_ingLab_m)) +
  geom_point(col = "red" , size = 0.5)

## by group
ggplot(data = db , 
       mapping = aes(x = age , y = y_ingLab_m , group=as.factor(formal) , color=as.factor(formal))) +
  geom_point()

##as.factor convierte todos elementos de la variable en la misma forma

## density: income by sex
p <- ggplot(data=db) + 
  geom_histogram(mapping = aes(x=y_ingLab_m , group=as.factor(sex) , fill=as.factor(sex)))
p

p + scale_fill_manual(values = c("0"="red" , "1"="blue") , label = c("0"="Hombre" , "1"="Mujer") , name = "Sexo")

## box_plot: estrato1 vs totalHoursWorked
box_plot <- ggplot(data=db , mapping = aes(as.factor(estrato1) , totalHoursWorked)) + 
  geom_boxplot() 
box_plot

## add another geometry
box_plot <- box_plot +
  geom_point(aes(colour=as.factor(sex))) +
  scale_color_manual(values = c("0"="red" , "1"="blue") , label = c("0"="Hombre" , "1"="Mujer") , name = "Sexo")
box_plot

## add theme
box_plot + theme_bw()

######Visualizing Missing Values#############

# we can use skim as a dataset. 
db_miss <- skim(db) %>% select( skim_variable, n_missing)

# number of observations
Nobs <- nrow(db) 
Nobs

# percentage of missing
db_miss<- db_miss %>% mutate(p_missing= n_missing/Nobs)
head(db_miss)

# sort in descending order 
db_miss <- db_miss %>% arrange(-n_missing) ## or arrange(desc(n_missing))

### Keep only variables with missing 
db_miss<- db_miss %>% filter(n_missing!= 0)

head(db_miss, 10)

tail(db_miss, 10)

ggplot(db_miss, aes(x = reorder(skim_variable, +p_missing) , y =  p_missing)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  coord_flip() +
  labs(title = "N Missing Per Variable", x = "Var Name", y = "Missings")+ 
  theme(axis.text = element_text(size = 5))  

## visualize the 40 variables with the mayor number of missing values
ggplot(head(db_miss, 40), aes(x = reorder(skim_variable, +p_missing) , y =  p_missing)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  coord_flip() +
  labs(title = "N Missing Per Variable", x = "Var Name", y = "Missings") +
  theme(axis.text = element_text(size = 8))  # Set size for axis labels

## visualize the 40 variables with less missing values
ggplot(tail(db_miss, 40), aes(x = reorder(skim_variable, +p_missing) , y =  p_missing)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  coord_flip() +
  labs(title = "N Missing Per Variable", x = "Var Name", y = "Missings") +
  theme(axis.text = element_text(size = 8))  # Set size for axis labels

## select some variables

db<- db %>% select( directorio, secuencia_p, orden, estrato1, sex, age, ocu, oficio, orden, totalHoursWorked,
                    dsi, ie , formal, informal, sizeFirm , regSalud, maxEducLevel, ingtot,
                    ingtotes,ingtotob, y_salary_m, y_total_m)


## Look at the missing variables by type. 
vis_dat(db)

### Look at the missing variables
vis_miss(db)

# create a dataset with all variables== 1 if missing
db2 <- db %>% mutate_all(~ifelse(!is.na(.), 1, 0))
## drop  variables with not missing or  with all missing.

db2 <-  db2 %>%  select(which(apply(db2, 2, sd) > 0))

M <- cor(db2)
corrplot(M) 

####METODO 1

ggplot(db, aes(maxEducLevel)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  ggtitle("Max Edu  Distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))

# calculating the most commun value of maxEducLevel. 
mode_edu <- as.numeric(names(sort(table(db$maxEducLevel), decreasing = TRUE)[1]))

# Imputing the missing value. 
db <- db  %>%
  mutate(maxEducLevel = ifelse(is.na(maxEducLevel) == TRUE, mode_edu , maxEducLevel))

## get missing values
is.na(db$y_total_m) %>% table()

## replace values:
db %>% select(directorio,y_total_m) %>% tail()

db = db %>% 
  group_by(directorio) %>% 
  mutate(mean_y_total_m = mean(y_total_m,na.rm=T))

db %>% select(directorio,y_total_m,mean_y_total_m) %>% tail()

db = db %>%
  mutate(y_total_m = ifelse(test = is.na(y_total_m)==T,
                            yes = mean_y_total_m,
                            no = y_total_m))

db %>% select(directorio,y_total_m,mean_y_total_m) %>% tail()


#######METODO 2

# transform categorical to factor

db$maxEducLevel<- factor(db$maxEducLevel)

dummy_maxEducLevel <- as.data.frame(model.matrix(~ maxEducLevel - 1, data = db)) 
#db<- db  %>% cbind(dummy_maxEducLevel) 
db <- cbind(db, dummy_maxEducLevel)

db %>% select(maxEducLevel,maxEducLevel1, maxEducLevel2,maxEducLevel3 ,maxEducLevel4 ,maxEducLevel5 ,maxEducLevel6, maxEducLevel7 ) %>% head() 

linear_imput_model <- lm(y_salary_m ~ ingtot + sex   + maxEducLevel3 + maxEducLevel4 + maxEducLevel5 + maxEducLevel6 + maxEducLevel7 , data = db)
summary(linear_imput_model)

db$predicted_y <- predict(linear_imput_model, newdata = db)

db %>% select(y_salary_m, predicted_y, y_salary_m  ) %>% head() 

db<-  db %>%  mutate(y_salary_m = ifelse(is.na(y_salary_m) == TRUE, predicted_y , y_salary_m))

db %>% select(y_salary_m, predicted_y, y_salary_m  ) %>% head() 

## drop recently created variables
db<-  db %>% select(- maxEducLevel1, - maxEducLevel2, - maxEducLevel3, - maxEducLevel4, - maxEducLevel5, - maxEducLevel6, - maxEducLevel7,-predicted_y )
