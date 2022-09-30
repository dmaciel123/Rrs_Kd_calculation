extrai_rrs = function(ed, lsky, lw, es, lu, eu, dept_ED) {
  
  
  ## Ed
  df_es <- data.frame(t(es[posicoes(medida = es, pressure = F)$campanha,]),
                      t(es[posicoes(medida = es, pressure = F)$campanha_sub1,]),
                      t(es[posicoes(medida = es, pressure = F)$campanha_sub2,]),
                      t(es[posicoes(medida = es, pressure = F)$datetime,]),
                      t(es[posicoes(medida = es, pressure = F)$profundidade,]),
                      t(es[posicoes(medida = es, pressure = F)$start_medidas:posicoes(medida = es, pressure = F)$fim_medidas,]))
  
  
  
  nomes = df_es[1,]
  df_es = df_es[-1,]
  names(df_es) <- nomes
  names(df_es)[4] = "DateTime"
  
  dt_df = strsplit(df_es$DateTime, split = '_')
  
  date = data.frame(matrix(unlist(dt_df), nrow=length(dt_df), byrow=TRUE))
  
  date = paste(date$X2, date$X3, sep = ' ')
  
  df_es$DateTime <- as.POSIXct(date, format="%Y-%m-%d %H-%M-%S")
  
  rm(date)
  
  
  #Lsky
  
  df_lsky <- data.frame(t(lsky[posicoes(medida = lsky, pressure = F)$campanha,]),
                        t(lsky[posicoes(medida = lsky, pressure = F)$campanha_sub1,]),
                        t(lsky[posicoes(medida = lsky, pressure = F)$campanha_sub2,]),
                        t(lsky[posicoes(medida = lsky, pressure = F)$datetime,]),
                        t(lsky[posicoes(medida = lsky, pressure = F)$profundidade,]),
                        t(lsky[posicoes(medida = lsky, pressure = F)$start_medidas:posicoes(medida = lsky, pressure = F)$fim_medidas,]))
  
  
  
  
  nomes_lsky = df_lsky[1,]
  df_lsky = df_lsky[-1,]
  names(df_lsky) = nomes_lsky
  names(df_lsky)[4] = "DateTime"
  
  
  dt_df = strsplit(df_lsky$DateTime, split = '_')
  
  date = data.frame(matrix(unlist(dt_df), nrow=length(dt_df), byrow=TRUE))
  
  date = paste(date$X2, date$X3, sep = ' ')
  
  df_lsky$DateTime <-as.POSIXct(date, format="%Y-%m-%d %H-%M-%S")
  
  
  #Lt
  
  df_lw <- data.frame(t(lw[posicoes(medida = lw, pressure = F)$campanha,]),
                      t(lw[posicoes(medida = lw, pressure = F)$campanha_sub1,]),
                      t(lw[posicoes(medida = lw, pressure = F)$campanha_sub2,]),
                      t(lw[posicoes(medida = lw, pressure = F)$datetime,]),
                      t(lw[posicoes(medida = lw, pressure = F)$profundidade,]),
                      t(lw[posicoes(medida = lw, pressure = F)$start_medidas:posicoes(medida = lw, pressure = F)$fim_medidas,]))
  
  nomes_lw = df_lw[1,]
  df_lw = df_lw[-1,]
  names(df_lw) = nomes_lw
  names(df_lw)[4] = "DateTime"
  
  
  dt_df = strsplit(df_lw$DateTime, split = '_')
  
  date = data.frame(matrix(unlist(dt_df), nrow=length(dt_df), byrow=TRUE))
  
  date = paste(date$X2, date$X3, sep = ' ')
  
  df_lw$DateTime <- as.POSIXct(date, format="%Y-%m-%d %H-%M-%S")
  
  
  #Lu
  
  
  df_lu <- data.frame(t(lu[posicoes(medida = lu, pressure = F)$campanha,]),
                      t(lu[posicoes(medida = lu, pressure = F)$campanha_sub1,]),
                      t(lu[posicoes(medida = lu, pressure = F)$campanha_sub2,]),
                      t(lu[posicoes(medida = lu, pressure = F)$datetime,]),
                      t(lu[posicoes(medida = lu, pressure = F)$profundidade,]),
                      t(lu[posicoes(medida = lu, pressure = F)$start_medidas:posicoes(medida = lu, pressure = F)$fim_medidas,]))
  
  
  nomes_lu = df_lu[1,]
  df_lu = df_lu[-1,]
  names(df_lu) = nomes_lu
  names(df_lu)[4] = "DateTime"
  
  
  dt_df = strsplit(df_lu$DateTime, split = '_')
  
  date = data.frame(matrix(unlist(dt_df), nrow=length(dt_df), byrow=TRUE))
  
  date = paste(date$X2, date$X3, sep = ' ')
  
  
  df_lu$DateTime <- as.POSIXct(date, format="%Y-%m-%d %H-%M-%S")
  
  
  
  
  #ed
  
  
  df_ed <- data.frame(t(ed[posicoes(medida = ed, pressure = dept_ED)$campanha,]),
                      t(ed[posicoes(medida = ed, pressure = dept_ED)$campanha_sub1,]),
                      t(ed[posicoes(medida = ed, pressure = dept_ED)$campanha_sub2,]),
                      t(ed[posicoes(medida = ed, pressure = dept_ED)$datetime,]),
                      t(ed[posicoes(medida = ed, pressure = dept_ED)$profundidade,]),
                      t(ed[posicoes(medida = ed, pressure = dept_ED)$start_medidas:posicoes(medida = ed, pressure = dept_ED)$fim_medidas,]))
  
  
  nomes_ed = df_ed[1,]
  df_ed = df_ed[-1,]
  names(df_ed) = nomes_ed
  names(df_ed)[4] = "DateTime"
  
  
  dt_df = strsplit(df_ed$DateTime, split = '_')
  
  date = data.frame(matrix(unlist(dt_df), nrow=length(dt_df), byrow=TRUE))
  
  date = paste(date$X2, date$X3, sep = ' ')
  
  
  df_ed$DateTime <-as.POSIXct(date, format="%Y-%m-%d %H-%M-%S")
  
  #eu
  
  
  df_eu <- data.frame(t(eu[posicoes(medida = eu, pressure = F)$campanha,]),
                      t(eu[posicoes(medida = eu, pressure = F)$campanha_sub1,]),
                      t(eu[posicoes(medida = eu, pressure = F)$campanha_sub2,]),
                      t(eu[posicoes(medida = eu, pressure = F)$datetime,]),
                      t(eu[posicoes(medida = eu, pressure = F)$profundidade,]),
                      t(eu[posicoes(medida = eu, pressure = F)$start_medidas:posicoes(medida = eu, pressure = F)$fim_medidas,]))
  
  
  nomes_eu = df_eu[1,]
  df_eu = df_eu[-1,]
  names(df_eu) = nomes_eu
  names(df_eu)[4] = "DateTime"
  
  
  dt_df = strsplit(df_eu$DateTime, split = '_')
  
  date = data.frame(matrix(unlist(dt_df), nrow=length(dt_df), byrow=TRUE))
  
  date = paste(date$X2, date$X3, sep = ' ')
  
  
  df_eu$DateTime <- as.POSIXct(date, format="%Y-%m-%d %H-%M-%S")
  
  
  
  resultados = list(df_lw, df_lsky, df_es, df_ed, df_eu, df_lu)
  names(resultados) <- c("Lw", "Lsky", "Es", "Ed", "Eu", "Lu")
  return(resultados)
}
