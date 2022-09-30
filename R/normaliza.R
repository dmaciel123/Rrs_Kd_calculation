normaliza = function(data) {
  
  radiometric = data[,-c(1:5)] #Retira as primeiras quatro colunas que s�o os atributos. Se usei a fun��o acima, vai estar assim correto
  wavelengths = as.numeric(names(radiometric))
  
  
  transpose.radiometric = data.frame(t(radiometric))
  
  df = data.frame(transpose.radiometric)
  
  normalizado = data.frame(Wave = c(400:900))
  
  for(i in 1:ncol(df)) {
    
    df[,i] = as.numeric(df[,i])
    
    if(is.na(df[,i][1]) == F) {
      normalizado[,(i+1)] = approx(x = wavelengths, y = df[,i], xout = c(400:900),rule = 2)$y
    } else {normalizado[,(i+1)] = 0}
    
    print(i)
  }
  
  normalizado.t = data.frame(t(normalizado))
  names(normalizado.t) = c(400:900)
  normalizado.t = normalizado.t[-1,]
  
  data.normalized = data.frame(data[,1:5], normalizado.t) %>% na.omit()
  
  
  return(data.normalized)
  
}