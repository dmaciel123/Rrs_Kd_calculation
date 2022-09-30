##Required packages

require(data.table)
require(dplyr)
require(tidyr)
require(triosRead)
require(plyr)
source("R/normaliza.R")
source("R/posicoes.R")
source("R/radiometric_quantities_extraction_and_rrs_calculation.R")
source("R/radiometric_quantities_extraction_and_rrs_calculation_3_sensors.R")
source("R/radiometric_quantities_extraction_kd.R")
source("R/read_trios_data.R")
source("R/rrs_filtering.R")
source("R/K_functions.R")
source("R/kd_funcao.R")

save_path = '/Users/danielmaciel/Downloads/trios_prom/8_15_2022_D01_TriOs'


es =   read.delim("/Users/danielmaciel/Downloads/trios_prom/8_15_2022_D01_TriOs/Es_SAM_8424.txt")
lw =   read.delim("/Users/danielmaciel/Downloads/trios_prom/8_15_2022_D01_TriOs/Lw_SAM_83AD.txt")
lsky = read.delim("/Users/danielmaciel/Downloads/trios_prom/8_15_2022_D01_TriOs/Lsky_SAM_839B.txt")
eu =   read.delim("/Users/danielmaciel/Downloads/trios_prom/8_15_2022_D01_TriOs/Eu_SAMIP_507B.txt")
ed =   read.delim("/Users/danielmaciel/Downloads/trios_prom/8_15_2022_D01_TriOs/Ed_SAMIP_5089.txt")
lu =   read.delim("/Users/danielmaciel/Downloads/trios_prom/8_15_2022_D01_TriOs/Lu_SAM_8404.txt")

res_kd = radiometric_quantities_extraction_kd(es = es, 
                                     lw = lw, 
                                     lsky = lsky,
                                     eu = eu,
                                     ed = ed,
                                     lu = lu, depth_ED = T)

es.split = split(res_kd$es, res_kd$es$estacoes_id)
ed.split = split(res_kd$ed, res_kd$ed$estacoes_id)
eu.split = split(res_kd$eu, res_kd$eu$estacoes_id)


resultados = data.frame(Wave = c(400:900))

#arrumar i = 14,14

df.es <- ldply(es.split[-1], data.frame)
df.ed <- ldply(ed.split[-1], data.frame)

df.es = df.es[-1]
df.ed = df.ed[-1]


NAMES = unique(df.ed$estacoes_id)

for(i in 1:length(names(ed.split))) {
  
  ED = filter(df.ed, estacoes_id == NAMES[i])
  ES = filter(df.es, estacoes_id == NAMES[i])
  
  teste = 'errado'
  
  while(teste == 'errado') {
  
    
  kd = kd_calculation(ed = ED, es = ES)
  
  
  teste = as.character(readline("Está ok?"))
  
  
  
  }
  
  resultados[,c(i+1)] = kd$ponto_all_wave
  
  names(resultados)[i+1]  = names(ed.split)[i]
}


#write.csv(resultados, "E:/OneDrive/OneDrive - inpe.br/Documentos/curuai_bonds_2021_novo.csv")

matplot(x = resultados$Wave, y = resultados[,15], type ='l',lty = 1,  col = 'black', ylim = c(0,40),
        lwd = 2, xlim = c(400,900))

names(resultados) 

res = t(resultados[,-c(1:2)]) %>% data.frame()
names(res) = resultados$Wave

final_kd = data.frame(estacao_nome = names(resultados[-c(1:2)]), res)

write.csv(final_kd, paste(save_path, '/kd.csv', sep = ''))




