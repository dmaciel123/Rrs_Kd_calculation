rrs_filtering = function(rrs) {
  
  require(ggplot2)
  require(plotly)
  
  rrs_filtered = rrs[0,]
  pontos = unique(rrs$estacoes_id)
  
  
  for(i in 1:length(pontos)) {
    
    teste = 'errado'
    
    rrs_filtro2 = rrs %>% filter(estacoes_id == pontos[[i]])
    
    quantis = quantile(rrs_filtro2$X550)
    
    rrs_filtro2 = filter(rrs_filtro2, X550 < 1 & X899 < 1 & X400 < 1)
  
    rrs_filtro2 = filter(rrs_filtro2, X550 < quantis[4])
    rrs_filtro2 = filter(rrs_filtro2, X550 > quantis[2])
    
    
    while(teste == 'errado') {
      
      matplot(t(rrs_filtro2[,-c(1:6)]), 
              type = 'l', cex.lab = 2, cex.axis = 3,
              main = rrs_filtro2$estacoes_id[1],
              cex.main = 5, col = 'black', x = c(400:899), xlab = "Wavelength")
      
      
      text(x = 600, y = rrs_filtro2$X600, labels=c(1:nrow(rrs_filtro2)), cex= 1, pos = 3)
      
      
      excluir = as.numeric(strsplit(x = readline("Entre com os pontos para excluir"), split = ',')[[1]])
      
      
      matplot(t(rrs_filtro2[-excluir,-c(1:6)]), 
              type = 'l', cex.lab = 2, cex.axis = 3,
              main = rrs_filtro2$estacoes_id[1],
              cex.main = 5, col = 'black', x = c(400:899), xlab = "Wavelength")
      
      rrs_filtro2 = rrs_filtro2[-excluir,]
      
      teste = as.character(readline("Está ok?"))
      
    }
    
    
    if(exists('rrs_filtrada') == FALSE) {rrs_filtrada = rrs_filtro2}
    
    else {rrs_filtrada = rbind(rrs_filtrada, rrs_filtro2) }
    

    print(i)
    
    
  }
  
  
  
  rrs.median = rrs_filtrada[,-c(2:6)] %>% group_by(estacoes_id) %>% summarise_each(fun = median)
  rrs.sd = rrs_filtrada[,-c(2:6)] %>% group_by(estacoes_id) %>% summarise_each(fun = sd)
  
  results = list(rrs_filtrada = rrs_filtrada, 
                    rrs_mediana = rrs.median, 
                    rrs_sd = rrs.sd)
  
  return(results)
  
}
