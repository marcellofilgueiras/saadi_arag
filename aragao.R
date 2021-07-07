#Aragão Casa de Saúde
#Verificando Prescrição em cobranças tributárias

library(tidyverse)



# Importing e tud ---------------------------------------------------------------

library(readxl)
library(lubridate)




processos_aragao_raw <-readxl::read_excel("data_raw/____________.xlsx", 
                            col_types = c("text", "text", "text", 
                                          "text", "text", "date", "text", "date", 
                                          "text","date", "text",  "text", "text"),
                     col_names = c("processo", "tribunal", "digital_ou_fisico",
                     "link", "suspenso", "data_suspensao", "arquivamento", "data_arquivamento",
                     "ultima_movimentacao", "data_ultima_mov", "valor_total", "prescricao", "comentarios"),
                     skip=1) %>%
                    janitor::clean_names()


processos_aragao<-processos_aragao_raw%>%
  mutate(across(.cols = c(suspenso, arquivamento, ultima_movimentacao),
                .fns = str_to_lower))%>%
  mutate(data_prescricao= if_else( condition = str_detect(suspenso, "921|40|suspensao processo civel|decisão judicial"),
                                  true=  data_suspensao + lubridate::dyears(6),
                                  false = NULL),
         data_prescricao = as.character(data_prescricao)%>%
                           str_extract("\\d+-\\d+-\\d+")%>%
                           lubridate::ymd(), 
         prescricao = if_else(condition = Sys.Date() > data_prescricao,
                              true= "Prescrito",
                              false = "Não Prescreveu - Falta prazo",
                              missing = "Não Presceveu - Sem Suspensão por não localizar bens"),
         tempo_ate_prescricao = data_prescricao - Sys.Date())


processos_aragao %>%
  select(suspenso, data_suspensao,
         valor_total, prescricao, data_prescricao, tempo_ate_prescricao) %>%
  view()



# Exporting ---------------------------------------------------------------


           


writexl::write_xlsx(processos_aragao,
                    path = "Aragao Prescrição.xlsx")
           
          
