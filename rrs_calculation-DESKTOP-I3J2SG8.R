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


save_path = 'F:/promissao abril 2022/'


es =   read.delim("F:/promissao abril 2022/Promissao 2022 Março/trios/es_SAM_83FD.TXT")
lw =   read.delim("F:/promissao abril 2022/Promissao 2022 Março/trios/lw_sam_83AD.txt")
lsky = read.delim("F:/promissao abril 2022/Promissao 2022 Março/trios/lsky_sam_839b.txt")
eu =   read.delim("F:/promissao abril 2022/Promissao 2022 Março/trios/")
ed =   read.delim("F:/promissao abril 2022/Promissao 2022 Março/trios/")
lu =   read.delim("F:/promissao abril 2022/Promissao 2022 Março/trios/")

resultados = radiometric_quantities_extraction_and_rrs_calculation_3_sensors(es = es, 
                                                                   lw = lw, 
                                                                   lsky = lsky, 
                                                                   lu = lw,ed = es,
                                                                   eu  = es,  depth_ED = F)

rrs_calculada = rrs_filtering(rrs = resultados$rrs)


pontos = unique(rrs_calculada$rrs_mediana$estacoes_id)

par(mfrow=c(3,3), cex.main = 1)

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

write.csv(data.frame(id = 1, rrs_calculada$rrs_filtrada), paste(save_path, sep = '/', "rrs_completa.csv"), row.names = F)
write.csv(data.frame(id = 1, rrs_calculada$rrs_mediana), paste(save_path, sep = '/', "rrs_mediana.csv"), row.names = F)
write.csv(data.frame(id = 1, rrs_calculada$rrs_sd), paste(save_path, sep = '/', "rrs_sd.csv"), row.names = F)













