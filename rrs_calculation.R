##Required packages

require(data.table)
require(dplyr)
require(tidyr)

source("R/normaliza.R")
source("R/posicoes.R")
source("R/radiometric_quantities_extraction_and_rrs_calculation.R")
source("R/radiometric_quantities_extraction_and_rrs_calculation_3_sensors.R")
source("R/radiometric_quantities_extraction_kd.R")
source("R/read_trios_data.R")
source("R/rrs_filtering.R")


# PATH were results will be saved
save_path = '/Users/danielmaciel/Downloads/trios_prom/8_15_2022_D01_TriOs'


## Loading trios data
es =   read.delim("Example/es_sam_8424.txt")
lw =   read.delim("Example/lw_sam_83ad.txt")
lsky = read.delim("Example/lsky_sam_839b.txt")
eu =   read.delim("Example/eu_sam_83b5.txt")
ed =   read.delim("Example/ed_sam_83fd.txt")
lu =   read.delim("Example/lu_sam_8404.txt")


## If we want to calculate Rrs only, based


## In the following function, we should enter with POINT NAMES, separated by COMMA. 
## Examples are available in the pontos_salvo.R file

## In the case of this example, the names will be entered as:
# Please copy and paste the following line when function ask for (WITHOUT the ###)
#Curuai_Ponto_35, Curuai_Ponto_Teste, Curuai_Ponto_29, Curuai_Ponto_28, Curuai_Ponto_Extra, Curuai_Ponto_17, Curuai_Ponto_16


resultados = radiometric_quantities_extraction_and_rrs_calculation_3_sensors(es = es, 
                                                                   lw = lw, 
                                                                   lsky = lsky, 
                                                                   lu = lu,
                                                                   ed = ed,
                                                                   eu  = eu,  depth_ED = T)



## Filter the results to remote OUTLIERS. 
## In this version, a boxplot is first calculate and we are selecting ONLY
## data between 2 and 4 quartiles. 

rrs_calculada = rrs_filtering(rrs = filter(resultados$rrs, CommentSub1 != 'P01_testlab'))

## Unique points to plot the results
pontos = unique(rrs_calculada$rrs_mediana$estacoes_id)


#Change size of graph
par(mfrow=c(2,3), cex.main = 1)

for(i in 1:length(pontos)) {
  
  rrs_filtro = resultados$rrs  %>% filter(estacoes_id == pontos[[i]])
  rrs_filtro2 = rrs_calculada$rrs_mediana  %>% filter(estacoes_id == pontos[[i]])

   matplot(t(rrs_filtro[,-c(1:6)]), 
         type = 'l', ylim = c(0,0.05), cex.lab = 2, cex.axis = 2,
         main = rrs_filtro$estacoes_id[1],
         cex.main = 5, x = c(400:899), xlab = "Wavelength", ylab = "Rrs", 
         col = 'red', cex.main = 1)
  
  par(new=T)
  
  matplot(t(rrs_filtro2[,-c(1)]), 
          type = 'l', ylim = c(0,0.05), cex.lab = 2, cex.axis = 2,
          main = rrs_filtro2$estacoes_id[1], ylab = "Rrs",
          cex.main = 5, cex.main = 1, col = 'black', x = c(400:899), xlab = "Wavelength")
  
  
}


#Saving the results
write.csv(data.frame(id = 1, resultados$lw),     paste(save_path, sep = '/', "lw.csv"), row.names = F)
write.csv(data.frame(id = 1, resultados$ls),     paste(save_path, sep = '/', "lsky.csv"), row.names = F)
write.csv(data.frame(id = 1, resultados$es),     paste(save_path, sep = '/', "es.csv"), row.names = F)

write.csv(data.frame(id = 1, resultados$ed),     paste(save_path, sep = '/', "ed.csv"), row.names = F)
write.csv(data.frame(id = 1, resultados$eu),     paste(save_path, sep = '/', "eu.csv"), row.names = F)
write.csv(data.frame(id = 1, resultados$lu),     paste(save_path, sep = '/', "lu.csv"), row.names = F)

write.csv(data.frame(id = 1, rrs_calculada$rrs_filtrada), paste(save_path, sep = '/', "rrs_completa.csv"), row.names = F)
write.csv(data.frame(id = 1, rrs_calculada$rrs_mediana), paste(save_path, sep = '/', "rrs_mediana.csv"), row.names = F)
write.csv(data.frame(id = 1, rrs_calculada$rrs_sd), paste(save_path, sep = '/', "rrs_sd.csv"), row.names = F)




