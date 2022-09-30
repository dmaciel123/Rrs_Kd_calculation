radiometric_quantities_extraction_kd = function(es, lw, lsky, eu, ed, lu, depth_ED) {
  
  require(data.table)
  require(dplyr)
  require(tidyr)
  require(triosRead)
  
  res = extrai_rrs(ed = ed, lsky = lsky,lw = lw, lu = lu, es = es, eu = eu,dept_ED = depth_ED)
  
  
  ## Verifica as dimensões
  
  dim(res$Lw)
  dim(res$Lsky)
  dim(res$Es)
  dim(res$Ed)
  dim(res$Eu)
  dim(res$Lu)
  
  #Tem que primeiro normalizar os dados para 400-900 e calcular a média entre medidas no mesmo tempo. Isso ocorre
  #Caso haja problemas no calculo dos SEGUNDOS dentro do MDB. Daí, tem que calcular a média por ponto no minuto.
  
  
  es_norm   =   normaliza(data = res$Es) #%>% group_radiometry()
  ed_norm   =   normaliza(data = res$Ed) #%>% group_radiometry()
  eu_norm   =   normaliza(data = res$Eu) #%>% group_radiometry()

  
  ##Depois de normalizar tenho que deixar os dados iguais (MESMO datetime)
  
  multiFull <- merge(merge(
    es_norm,
    ed_norm, by = "DateTime"),
    eu_norm, by = "DateTime")
  
  #Separado o dado merged de novo para o formato padrão. agora, todos tem as mesmas colunas e linhas.
  
  es_final   = multiFull[,(1:ncol(es_norm))]
  ed_final   = multiFull[,(ncol(es_norm)+1):(ncol(es_norm)*2-1)]
  eu_final   = multiFull[,(ncol(es_norm)*2):(ncol(es_norm)*3-2)]

  dim(es_final)
  dim(ed_final)
  dim(eu_final)

  
  
  es_ =   data.frame(es_final$Comment.x,     es_final$CommentSub1.x, es_final$CommentSub2.x,    es_final$DateTime, es_final$IDDevice.x, es_final[,-c(1:5)])
  ed_ =   data.frame(ed_final$Comment.y,     ed_final$CommentSub1.y, ed_final$CommentSub2.y,    es_final$DateTime, ed_final$Pressure, ed_final[,-c(1:4)])
  eu_ =   data.frame(eu_final$Comment,     eu_final$CommentSub1, eu_final$CommentSub2,    es_final$DateTime, eu_final$IDDevice, eu_final[,-c(1:4)])

  dim(es_)
  dim(ed_)
  dim(eu_)

  names(es_) = names(ed_norm)
  names(ed_) = names(ed_norm)
  names(eu_) = names(ed_norm)

  nome_original = unique(ed_norm$CommentSub1)
  
  print(nome_original)
  
  nome_novo = as.character(strsplit(x = readline("Entre com o nome dos pontos"), split = ',')[[1]])
  
  subs_names = function(df, nome_original, nome_novo) {
    
    df$estacao_nome = df$CommentSub1
    
    for(i in 1:length(nome_original)) {
      
      df$estacao_nome = gsub(df$estacao_nome,replacement = nome_novo[i], pattern = paste('^', nome_original[i], '$', sep  = ''))
      
      print(paste("Alterando nome",nome_novo[i]))
    }
    
    return(df)
    
  }
  
  es_ = es_ %>% subs_names(nome_original = nome_original, nome_novo = nome_novo)
  ed_ = ed_ %>% subs_names(nome_original = nome_original, nome_novo = nome_novo)
  eu_ = eu_ %>% subs_names(nome_original = nome_original, nome_novo = nome_novo)

  
  es_final =   data.frame(estacoes_id = es_$estacao_nome,       es_[,-c( ncol(ed_))])
  ed_final =   data.frame(estacoes_id = ed_$estacao_nome,       ed_[,-c( ncol(ed_))])
  eu_final =   data.frame(estacoes_id = eu_$estacao_nome,       eu_[,-c( ncol(ed_))])

  
  
  names(es_final)[1] = 'estacoes_id'
  names(ed_final)[1] = 'estacoes_id'
  names(eu_final)[1] = 'estacoes_id'

  ##Rrs calculation
  

  resultados = list(es = es_final,
                    ed = ed_final,
                    eu = eu_final)
  
  
  
  
  
  return(resultados)
  
}
