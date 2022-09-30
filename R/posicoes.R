posicoes = function(medida, pressure) {
  
  require(data.table)
  #Campanha
  i = 0
  #Obtem as linhas para cada atributo
  for(i in 1:nrow(medida)) {
    if(medida[i,1] == 'Comment') {
      campanha = i
      break
    }
  }
  for(i in 1:nrow(medida)) {
    if(medida[i,1] == 'CommentSub1') {
      campanha_sub1 = i
      break
    }
  }
  for(i in 1:nrow(medida)) {
    if(medida[i,1] == 'CommentSub2') {
      campanha_sub2 = i
      break
    }
  }
  for(i in 1:nrow(medida)) {
    if(medida[i,1] == 'IDData') {
      datetime = i
      break
    }
  }
  for(i in 1:nrow(medida)) {
    if(medida[i,1] == '[Data]') {
      start_medidas = i+29
      break
    }
  }
  for(i in 1:nrow(medida)) {
    if(medida[i,1] == '[END] of [Spectrum]') {
      fim_medidas = i-76
      break
    }
  }
  
  if(pressure == T) {
    for(i in 1:ncol(medida)) {
      
      if(medida[i,1] == 'Pressure') {
        
        profundidade = i
        break
        
      }
    }
    
  } else(profundidade = 2)
  
  resultados = list(campanha = campanha,
                    campanha_sub1 = campanha_sub1,
                    campanha_sub2 = campanha_sub2,
                    datetime = datetime,
                    start_medidas = start_medidas,
                    fim_medidas = fim_medidas,
                    profundidade = profundidade)
  
  return(resultados)
  
}