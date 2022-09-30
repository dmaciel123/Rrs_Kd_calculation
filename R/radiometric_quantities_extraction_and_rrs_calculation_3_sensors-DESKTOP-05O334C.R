radiometric_quantities_extraction_and_rrs_calculation_3_sensors = function(es, lw, lsky, depth_ED) {
  
  require(data.table)
  require(dplyr)
  require(tidyr)

  res = extrai_rrs(lsky = lsky,lw = lw, es = es,ed = es, lu = es, eu = es, dept_ED = depth_ED)
  
  
  ## Verifica as dimensões
  
  dim(res$Lw)
  dim(res$Lsky)
  dim(res$Es)

  
  #Tem que primeiro normalizar os dados para 400-900 e calcular a média entre medidas no mesmo tempo. Isso ocorre
  #Caso haja problemas no calculo dos SEGUNDOS dentro do MDB. Daí, tem que calcular a média por ponto no minuto.
  
  
  lw_norm   =   normaliza(data = res$Lw) #%>% group_radiometry()
  lsky_norm =   normaliza(data = res$Lsky) #%>% group_radiometry()
  es_norm   =   normaliza(data = res$Es) #%>% group_radiometry()

  
  ##Depois de normalizar tenho que deixar os dados iguais (MESMO datetime)
  
  multiFull <- merge(merge(
    lw_norm,
    lsky_norm, by = "DateTime"),
    es_norm,by = "DateTime")
  
  #Separado o dado merged de novo para o formato padrão. agora, todos tem as mesmas colunas e linhas.
  
  lw_final   = multiFull[,1:ncol(lw_norm)]
  lsky_final = multiFull[,(ncol(lw_norm)+1):(ncol(lw_norm)*2-1)]
  es_final   = multiFull[,(ncol(lw_norm)*2):(ncol(lw_norm)*3-2)]

  dim(lw_final)
  dim(lsky_final)
  dim(es_final)

  
  
  lw_ =   data.frame(lw_final$Comment.x,     lw_final$CommentSub1.x, lw_final$CommentSub2.x,    lw_final$DateTime, lw_final$IDDevice.x, lw_final[,-c(1:5)])
  lsky_ = data.frame(lsky_final$Comment.y, lsky_final$CommentSub1.y, lsky_final$CommentSub2.y,  lw_final$DateTime, lsky_final$IDDevice.y, lsky_final[,-c(1:4)])
  es_ =   data.frame(es_final$Comment,     es_final$CommentSub1, es_final$CommentSub2,    lw_final$DateTime, es_final$IDDevice, es_final[,-c(1:4)])
  
  dim(lw_)
  dim(lsky_)
  dim(es_)

  names(lw_) = names(es_norm)
  names(lsky_) = names(es_norm)
  names(es_) = names(es_norm)

  nome_original = unique(lw_$CommentSub1)
  
  print(nome_original)
  
  nome_novo = as.character(strsplit(x = readline("Entre com o nome dos pontos"), split = ',')[[1]])
  
  subs_names = function(df, nome_original, nome_novo) {
    
    df$estacao_nome = df$CommentSub1
    
    for(i in 1:length(nome_original)) {
      
      df$estacao_nome = gsub(df$estacao_nome, replacement = nome_novo[i], pattern =  paste('^', nome_original[i], '$', sep  = ''))
      
      print(paste("Alterando nome",nome_novo[i]))
    }
    
    return(df)
    
  }
  
  lw_ = lw_ %>% subs_names(nome_original = nome_original, nome_novo = nome_novo)
  lsky_ = lsky_ %>% subs_names(nome_original = nome_original, nome_novo = nome_novo)
  es_ = es_ %>% subs_names(nome_original = nome_original, nome_novo = nome_novo)

  
  
  
  
  lw_final =   data.frame(estacoes_id = lw_$estacao_nome,       lw_[,-c((ncol(lw_)-1), ncol(lw_))])
  ls_final =   data.frame(estacoes_id = lsky_$estacao_nome,     lsky_[,-c( (ncol(lw_)-1), ncol(lw_))])
  es_final =   data.frame(estacoes_id = es_$estacao_nome,       es_[,-c( (ncol(lw_)-1), ncol(lw_))])

  
  
  names(lw_final)[1] = 'estacoes_id'
  names(ls_final)[1] = 'estacoes_id'
  names(es_final)[1] = 'estacoes_id'
  ##Rrs calculation
  
  rrs1 = es_final
  
  rrs1[,-c(1:6)] = (lw_final[,-c(1:6)]-ls_final[,-c(1:6)]*0.028)/es_final[-c(1:6)]
  
  rrs2 = es_final
  
  rrs2[,-c(1:6)] = (ls_final[,-c(1:6)]-lw_final[,-c(1:6)]*0.028)/es_final[-c(1:6)]
  
  
  resultados1 = list(lw = rbind(filter(ls_final, DateTime < as.Date('2020-11-08')),filter(lw_final, DateTime > as.Date('2020-11-09'))),
                     ls = rbind(filter(lw_final, DateTime < as.Date('2020-11-08')),filter(ls_final, DateTime > as.Date('2020-11-09'))),
                     es = rbind(filter(es_final, DateTime < as.Date('2020-11-08')),filter(es_final, DateTime > as.Date('2020-11-09'))),
                    rrs = rbind(filter(rrs2, DateTime < as.Date('2020-11-08')), filter(rrs1, DateTime > as.Date('2020-11-09'))))
  
  
  resultados2 = list(lw = filter(lw_final, DateTime > as.Date('2020-11-09')),
                     ls = filter(ls_final, DateTime > as.Date('2020-11-09')),
                     es = filter(es_final, DateTime > as.Date('2020-11-09')),
                    rrs = filter(rrs1, DateTime > as.Date('2020-11-09')))
  
  return(resultados)
  
}
