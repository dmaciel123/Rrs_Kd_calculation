##Required packages

require(data.table)
require(dplyr)
require(tidyr)
require(triosRead)
source('R/normaliza.R')
source("R/radiometric_quantities_extraction_and_rrs_calculation.R")
source("R/radiometric_quantities_extraction_and_rrs_calculation_3_sensors.R")
source("R/rrs_filtering.R")
source("R/posicoes.R")
source("R/read_trios_data.R")


dir_padrao = getwd()
dir_salvamento = "E:/OneDrive/OneDrive - inpe.br/Doutorado/Banco de dados/adiciona_dados/Organizacao/Adicionando ao Banco - TEMP/Billings_11_2020"

es =   read.delim("G:/Drives compartilhados/Reuniões labISA/Campanhas/BILLINGS_11_2020/Dados Brutos/dados_brutos/trios/SAM_8424.txt")
lw =   read.delim("G:/Drives compartilhados/Reuniões labISA/Campanhas/BILLINGS_11_2020/Dados Brutos/dados_brutos/trios/SAM_83AD.txt")
lsky = read.delim("G:/Drives compartilhados/Reuniões labISA/Campanhas/BILLINGS_11_2020/Dados Brutos/dados_brutos/trios/SAM_839B.txt")
eu =   read.delim("G:/Drives compartilhados/Reuniões labISA/Campanhas/BILLINGS_11_2020/Dados Brutos/dados_brutos/trios/SAM_8404.txt")
ed =   read.delim("G:/Drives compartilhados/Reuniões labISA/Campanhas/BILLINGS_11_2020/Dados Brutos/dados_brutos/trios/SAM_83FD.txt")
lu =   read.delim("G:/Drives compartilhados/Reuniões labISA/Campanhas/BILLINGS_11_2020/Dados Brutos/dados_brutos/trios/SAM_8404.txt")


  
resultados = radiometric_quantities_extraction_and_rrs_calculation(es = es, lw = lw, lsky = lsky, eu = eu, ed = ed, lu = lu,depth_ED = T)
resultados = radiometric_quantities_extraction_and_rrs_calculation_3_sensors(es = es, lw = lw, lsky = lsky, eu = eu, ed = ed, lu = lu,depth_ED = F)
  
#rrs = read.table("E:/OneDrive/OneDrive - inpe.br/Doutorado/Banco de dados/adiciona_dados/Organizacao/Adicionando ao Banco - TEMP/BILLINGS_08_2021/rrs.csv", header=T, sep =',')

rrs_filtrada = rrs_filtering(rrs = resultados$rrs)


par(mfrow=c(2,2))

pontos = unique(rrs_filtrada$rrs_filtrada$estacoes_id)

for(i in 1:length(unique(rrs_filtrada$rrs_filtrada$estacoes_id))) {
  
  
  #rrs_filtro2 = rrs_filtrada$rrs_filtrada  %>% filter(estacoes_id == pontos[[i]])
  rrs_filtro2 = resultados$rrs  %>% filter(estacoes_id == pontos[[i]])
  rrs_mediana = rrs_filtrada$rrs_mediana %>% filter(estacoes_id == pontos[[i]])
  
  matplot(t(rrs_filtro2[,-c(1:6)]), 
          type = 'l', ylim = c(0,0.04), cex.lab = 2, cex.axis = 3,
          main = rrs_filtro2$estacoes_id[1],
          cex.main = 5, col = 'black', x = c(400:899), xlab = "Wavelength")
  par(new=T)
  matplot(t(rrs_mediana[,-1]), x = c(400:899), type = 'l', lwd = 3, col = 'red', ylim = c(0,0.04))
  
  
}



write.csv(data.frame(id = 1, resultados$rrs), paste(dir_salvamento, "/rrs_completa.csv", sep = ''), row.names = F)
write.csv(resultados$lw,                                 paste(dir_salvamento, "/lw_final.csv", sep = ''))
write.csv(resultados$ls,                                 paste(dir_salvamento, "/ls_final.csv", sep = ''))
write.csv(resultados$es,                                 paste(dir_salvamento, "/es_final.csv", sep = ''))
write.csv(resultados$ed,                                 paste(dir_salvamento, "/ed_final.csv", sep = ''))
write.csv(resultados$eu,                                 paste(dir_salvamento, "/eu_final.csv", sep = ''))
write.csv(resultados$lu,                                 paste(dir_salvamento, "/lu_final.csv", sep = ''))
write.csv(rrs_filtrada$rrs_mediana,                      paste(dir_salvamento, "/rrs_mediana.csv", sep = ''))











## apagar depois de ler as tabelas e unir

ed = rbind(read.table('ed_final_curuai.csv', header= T, sep = ',')[,-1], read.table('ed_final_tap_araipuns.csv', header= T, sep = ',')[,-1])[,-c(2,3,4)]
es = rbind(read.table('es_final_curuai.csv', header= T, sep = ',')[,-1], read.table('es_final_tap_araipuns.csv', header= T, sep = ',')[,-1])[,-c(2,3,4)]
eu = rbind(read.table('eu_final_curuai.csv', header= T, sep = ',')[,-1], read.table('eu_final_tap_araipuns.csv', header= T, sep = ',')[,-1])[,-c(2,3,4)]
lw = rbind(read.table('lw_final_curuai.csv', header= T, sep = ',')[,-1], read.table('lw_final_tap_araipuns.csv', header= T, sep = ',')[,-1])[,-c(2,3,4)]
ls = rbind(read.table('ls_final_curuai.csv', header= T, sep = ',')[,-1], read.table('ls_final_tap_araipuns.csv', header= T, sep = ',')[,-1])[,-c(2,3,4)]
lu = rbind(read.table('lu_final_curuai.csv', header= T, sep = ',')[,-1], read.table('lu_final_tap_araipuns.csv', header= T, sep = ',')[,-1])[,-c(2,3,4)]
rrs_completa = rbind(read.table('rrs_curuai.csv', header= T, sep = ',')[,-1], read.table('rrs_tap_araipuns.csv', header= T, sep = ',')[,-1])[,-c(2,3,4)]
rrs_mna = rbind(read.table('rrs_mediana_curuai.csv', header= T, sep = ',')[,-1], read.table('rrs_mediana_tap_araipuns.csv', header= T, sep = ',')[,-1])


id = 1

ed = data.frame(id = id, ed)
es = data.frame(id = id, es)
eu = data.frame(id = id, eu)
lw = data.frame(id = id, lw)
ls = data.frame(id = id, ls)
lu = data.frame(id = id, lu)
rrs_completa = data.frame('id', rrs_completa)
rrs_mna = data.frame('id', rrs_mna)




write.table(ed, "ed.csv",row.names = F, sep = ',')
write.table(es, "es.csv",row.names = F, sep = ',')
write.table(eu, "eu.csv",row.names = F, sep = ',' )
write.table(lw, "lw.csv",row.names = F, sep = ',' )
write.table(ls, "ls.csv",row.names = F, sep = ',' )
write.table(lu, "lu.csv",row.names = F, sep = ',' )
write.table(rrs_completa, "rrs_completa.csv",row.names = F, sep = ',' )
write.table(rrs_mediana, "rrs_mediana.csv",row.names = F, sep = ',' )



































rrs_lu = ed_final

rrs_lu[,-c(1:6)] = (lu_final[,-c(1:6)])/es_final[-c(1:6)]

rrs_lu$estacoes_id = lw_$CommentSub1


rrs_lw_only = ed_final

rrs_lw_only[,-c(1:6)] = (lw_final[,-c(1:6)])/es_final[-c(1:6)]

rrs_lw_only$estacoes_id = lw_$CommentSub1



jiang_gc = function(spectra) {
  
  names(spectra)[-c(1:6)] = c(400:900)
  
  c.rrs = spectra[,-c(1:6)]
  
  rrs810_ = (c.rrs$`780`+(c.rrs$`840`-c.rrs$`780`)*(810-780)/(840-780))
  
  RHW = c.rrs$`810`  - rrs810_
  
  rrs_810 = 16865.541*RHW^3 - 52.728*RHW^2 +3.361*RHW 
  
  DELTA = c.rrs$`810` - rrs_810
  ## DELTA CALCULATION
  
  rrs.corrected = spectra
  
  rrs.corrected[,-c(1:6)] = rrs.corrected[,-c(1:6)]-DELTA
  
  return(rrs.corrected)
  
}


jiang_gc_cor = jiang_gc(spectra = rrs_lw_only )

pontos = unique(rrs_lu$estacoes_id)

par(mfrow=c(2,2)) 


for(i in 1:length(pontos)) {
  
  
  
  #rrs_filtro = rrs_CORRIGIDA  %>% filter(estacoes_id == pontos[[i]])
  rrs_filtro2 = rrs_lu  %>% filter(estacoes_id == pontos[[i]] & CommentSub2 == "sub")
  rrs_filtro_glint = rrs  %>% filter(estacoes_id == pontos[[i]])
  rrs_filtro_no_glint = rrs_lw_only  %>% filter(estacoes_id == pontos[[i]])
  rrs_filtro_jiang = jiang_gc_cor  %>% filter(estacoes_id == pontos[[i]])
  
  matplot(t(rrs_filtro_glint[,-c(1:6)]), 
          type = 'l', ylim = c(0,0.01), cex.lab = 2, cex.axis = 3,
          main = paste(rrs_filtro_glint$estacoes_id[1]),
          cex.main = 5, x = c(400:899), xlab = "Wavelength", col = 'red')
  
  par(new=T)
  
  matplot(t(rrs_filtro2[,-c(1:6)]), 
          type = 'l', ylim = c(0,0.01), cex.lab = 2, cex.axis = 3,
          main = rrs_filtro2$estacoes_id[1],
          cex.main = 5, col = 'black', x = c(400:899), xlab = "Wavelength")
  par(new=T)
  
  
  matplot(t(rrs_filtro_no_glint[,-c(1:6)]), 
          type = 'l', ylim = c(0,0.01), cex.lab = 2, cex.axis = 3,
          main = rrs_filtro_no_glint$estacoes_id[1],
          cex.main = 5, col = 'blue', x = c(400:899), xlab = "Wavelength")
  
  par(new=T)
  
  
  matplot(t(rrs_filtro_jiang[,-c(1:6)]), 
          type = 'l', ylim = c(0,0.01), cex.lab = 2, cex.axis = 3,
          main = rrs_filtro_jiang$estacoes_id[1],
          cex.main = 5, col = 'brown', x = c(400:899), xlab = "Wavelength")
  
  legend('topleft', legend = c("no correction", 'jiang 2020', 'mobley 2015', 'rrs Lu'),
         col = c('blue', 'brown', 'red', 'black'), lwd = 1, cex = 0.4)
  
}







write.csv(data.frame(id = 1, lw_final), "lw.csv", row.names = F)
write.csv(data.frame(id = 1, ls_final), "lsky.csv", row.names = F)
write.csv(data.frame(id = 1, ls_final), "lsky.csv", row.names = F)
write.csv(data.frame(id = 1, rrs), "rrs.csv", row.names = F)













