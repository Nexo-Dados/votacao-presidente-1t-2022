# Bibliotecas -------------------------------------------------------------

library(tidyverse)
library(sf)
library(jsonlite)

# Configuracao dos municipios ---------------------------------------------

urlm = "https://resultados.tse.jus.br/oficial/ele2022/544/config/mun-e000544-cm.json"
muni_tse <- fromJSON(urlm,
                     simplifyDataFrame = TRUE) %>%
  .[['abr']]  %>%
  unnest('mu', names_repair="universal") %>%
  select(-c, -z) %>%
  set_names("uf", "estado", "tse", "ibge7", "nm")


# Dados -------------------------------------------------------------------

url_muni = paste0("https://resultados.tse.jus.br/oficial/ele2022/544/dados/", str_to_lower(muni_tse$uf),
                  "/", str_to_lower(muni_tse$uf), muni_tse$tse, "-c0001-e000544-v.json")

# para evitar quedas do servidor do tse
rate <- rate_backoff(pause_base = 0.1, pause_min = 0.005, max_times = 100)

fs <- function(x,y) {
  x<-fromJSON(x, simplifyDataFrame = T)
  print(y)
  return(x)}
insistent <- insistently(fs,
                         rate, 
                         quiet = FALSE)

municipios <- imap(url_muni, insistent)

# juntar todos os dados
x <- municipios %>% 
  bind_rows() %>% 
  .[["abr"]] %>% 
  unnest(cand, names_repair="universal")

# tratamento dos dados
x %>% 
  filter(tpabr=="MU") %>% 
  left_join(muni_tse, by=c("cdabr"="tse")) %>% 
  select(cdabr, ibge7, nm, uf, e...20, c, a, vv, vn, vb, vap, n) %>% 
  pivot_wider(names_from="n", values_from="vap") %>% 
  select(cdabr:vb,
         `13`, `22`, `15`, `12`, `14`, `16`, `21`, `27`, `30`, `44`, `80`) %>% 
  rename(tse =cdabr, municipio=nm, comparecimento=c, eleitores=e...20, abstencoes=a,
         validos = vv, nulos = vn, brancos=vb)  -> y

# salvar como planilha
googlesheets4::write_sheet(y, ss="https://docs.google.com/spreadsheets/d/1DJMiKEflo2gInPa_2T2BdzWtOdo0O1XNjCj98qJZGgE/edit#gid=0",
                           sheet="presidente-1t-2022")
