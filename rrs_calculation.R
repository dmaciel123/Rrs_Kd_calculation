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


save_path = '/Users/danielmaciel/Downloads/trios_prom/8_15_2022_D01_TriOs'


es =   read.delim("/Users/danielmaciel/Downloads/trios_prom/8_15_2022_D01_TriOs/Es_SAM_8424.txt")
lw =   read.delim("/Users/danielmaciel/Downloads/trios_prom/8_15_2022_D01_TriOs/Lw_SAM_83AD.txt")
lsky = read.delim("/Users/danielmaciel/Downloads/trios_prom/8_15_2022_D01_TriOs/Lsky_SAM_839B.txt")
eu =   read.delim("/Users/danielmaciel/Downloads/trios_prom/8_15_2022_D01_TriOs/Eu_SAMIP_507B.txt")
ed =   read.delim("/Users/danielmaciel/Downloads/trios_prom/8_15_2022_D01_TriOs/Ed_SAMIP_5089.txt")
lu =   read.delim("/Users/danielmaciel/Downloads/trios_prom/8_15_2022_D01_TriOs/Lu_SAM_8404.txt")

resultados = radiometric_quantities_extraction_and_rrs_calculation(es = es, 
                                                                   lw = lw, 
                                                                   lsky = lsky, 
                                                                   lu = lu,
                                                                   ed = ed,
                                                                   eu  = eu,  depth_ED = T)

rrs_calculada = rrs_filtering(rrs = filter(resultados$rrs, CommentSub1 != 'P01_testlab'))

pontos = unique(rrs_calculada$rrs_mediana$estacoes_id)

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

write.csv(data.frame(id = 1, resultados$lw),     paste(save_path, sep = '/', "lw.csv"), row.names = F)
write.csv(data.frame(id = 1, resultados$ls),     paste(save_path, sep = '/', "lsky.csv"), row.names = F)
write.csv(data.frame(id = 1, resultados$es),     paste(save_path, sep = '/', "es.csv"), row.names = F)

write.csv(data.frame(id = 1, resultados$ed),     paste(save_path, sep = '/', "ed.csv"), row.names = F)
write.csv(data.frame(id = 1, resultados$eu),     paste(save_path, sep = '/', "eu.csv"), row.names = F)
write.csv(data.frame(id = 1, resultados$lu),     paste(save_path, sep = '/', "lu.csv"), row.names = F)

write.csv(data.frame(id = 1, rrs_calculada$rrs_filtrada), paste(save_path, sep = '/', "rrs_completa.csv"), row.names = F)
write.csv(data.frame(id = 1, rrs_calculada$rrs_mediana), paste(save_path, sep = '/', "rrs_mediana.csv"), row.names = F)
write.csv(data.frame(id = 1, rrs_calculada$rrs_sd), paste(save_path, sep = '/', "rrs_sd.csv"), row.names = F)




