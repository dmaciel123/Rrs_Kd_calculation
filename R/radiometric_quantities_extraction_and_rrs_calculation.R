radiometric_quantities_extraction_and_rrs_calculation = function(es, lw, lsky, eu, ed, lu, depth_ED) {
  
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
  
  
  lw_norm   =   normaliza(data = res$Lw) #%>% group_radiometry()
  lsky_norm =   normaliza(data = res$Lsky) #%>% group_radiometry()
  es_norm   =   normaliza(data = res$Es) #%>% group_radiometry()
  ed_norm   =   normaliza(data = res$Ed) #%>% group_radiometry()
  eu_norm   =   normaliza(data = res$Eu) #%>% group_radiometry()
  lu_norm   =   normaliza(data = res$Lu) #%>% group_radiometry()
  
  
  ##Depois de normalizar tenho que deixar os dados iguais (MESMO datetime)
  
  multiFull <- merge(merge(merge(merge(merge(
    lw_norm,
    lsky_norm, by = "DateTime"),
    es_norm,by = "DateTime"),
    ed_norm, by = "DateTime"),
    eu_norm, by = "DateTime"),
    lu_norm, by = "DateTime")
  
  #Separado o dado merged de novo para o formato padrão. agora, todos tem as mesmas colunas e linhas.
  
  lw_final   = multiFull[,1:ncol(lw_norm)]
  lsky_final = multiFull[,(ncol(lw_norm)+1):(ncol(lw_norm)*2-1)]
  es_final   = multiFull[,(ncol(lw_norm)*2):(ncol(lw_norm)*3-2)]
  ed_final   = multiFull[,(ncol(lw_norm)*3-1):(ncol(lw_norm)*4-3)]
  eu_final   = multiFull[,(ncol(lw_norm)*4-2):(ncol(lw_norm)*5-4)]
  lu_final   = multiFull[,(ncol(lw_norm)*5-3):(ncol(lw_norm)*6-5)]
  
  dim(lw_final)
  dim(lsky_final)
  dim(es_final)
  dim(ed_final)
  dim(eu_final)
  dim(lu_final)
  
  
  
  lw_ =   data.frame(lw_final$Comment.x,     lw_final$CommentSub1.x, lw_final$CommentSub2.x,    lw_final$DateTime, lw_final$IDDevice.x, lw_final[,-c(1:5)])
  lsky_ = data.frame(lsky_final$Comment.y, lsky_final$CommentSub1.y, lsky_final$CommentSub2.y,  lw_final$DateTime, lsky_final$IDDevice.y, lsky_final[,-c(1:4)])
  es_ =   data.frame(es_final$Comment.x,     es_final$CommentSub1.x, es_final$CommentSub2.x,    lw_final$DateTime, es_final$IDDevice.x, es_final[,-c(1:4)])
  ed_ =   data.frame(ed_final$Comment.y,     ed_final$CommentSub1.y, ed_final$CommentSub2.y,    lw_final$DateTime, ed_final$Pressure, ed_final[,-c(1:4)])
  eu_ =   data.frame(eu_final$Comment.x,     eu_final$CommentSub1.x, eu_final$CommentSub2.x,    lw_final$DateTime, eu_final$IDDevice.y, eu_final[,-c(1:4)])
  lu_ =   data.frame(lu_final$Comment.y,     lu_final$CommentSub1.y, lu_final$CommentSub2.y,    lw_final$DateTime, lu_final$IDDevice, lu_final[,-c(1:4)])
  
  dim(lw_)
  dim(lsky_)
  dim(es_)
  dim(ed_)
  dim(eu_)
  dim(lu_)
  
  names(lw_) = names(ed_norm)
  names(lsky_) = names(ed_norm)
  names(es_) = names(ed_norm)
  names(ed_) = names(ed_norm)
  names(eu_) = names(ed_norm)
  names(lu_) = names(ed_norm)
  
  nome_original = unique(lw_$CommentSub1)
  
  print(nome_original)
  
  nome_novo = as.character(strsplit(x = readline("Entre com o nome dos pontos"), split = ',')[[1]])
  
  subs_names = function(df, nome_original, nome_novo) {
    
    df$estacao_nome = df$CommentSub1
    
    for(i in 1:length(nome_original)) {
      
      df$estacao_nome = gsub(df$estacao_nome,replacement = nome_novo[i], pattern = nome_original[i])
      
      print(paste("Alterando nome",nome_novo[i]))
    }
    
    return(df)
    
  }
  
  lw_ = lw_ %>% subs_names(nome_original = nome_original, nome_novo = nome_novo)
  lsky_ = lsky_ %>% subs_names(nome_original = nome_original, nome_novo = nome_novo)
  es_ = es_ %>% subs_names(nome_original = nome_original, nome_novo = nome_novo)
  ed_ = ed_ %>% subs_names(nome_original = nome_original, nome_novo = nome_novo)
  eu_ = eu_ %>% subs_names(nome_original = nome_original, nome_novo = nome_novo)
  lu_ = lu_ %>% subs_names(nome_original = nome_original, nome_novo = nome_novo)
  
  
  
  
  
  
  lw_final =   data.frame(estacoes_id = lw_$estacao_nome,       lw_[,-c((ncol(lw_)-1), ncol(lw_))])
  ls_final =   data.frame(estacoes_id = lsky_$estacao_nome,     lsky_[,-c( (ncol(lw_)-1), ncol(lw_))])
  es_final =   data.frame(estacoes_id = es_$estacao_nome,       es_[,-c( (ncol(lw_)-1), ncol(lw_))])
  ed_final =   data.frame(estacoes_id = ed_$estacao_nome,       ed_[,-c( (ncol(lw_)-1), ncol(lw_))])
  eu_final =   data.frame(estacoes_id = eu_$estacao_nome,       eu_[,-c( (ncol(lw_)-1), ncol(lw_))])
  lu_final =   data.frame(estacoes_id = lu_$estacao_nome,       lu_[,-c((ncol(lw_)-1), ncol(lw_))])
  
  
  
  names(lw_final)[1] = 'estacoes_id'
  names(ls_final)[1] = 'estacoes_id'
  names(es_final)[1] = 'estacoes_id'
  names(ed_final)[1] = 'estacoes_id'
  names(eu_final)[1] = 'estacoes_id'
  names(lu_final)[1] = 'estacoes_id'
  
  ##Rrs calculation
  
  rrs = es_final
  
  rrs[,-c(1:6)] = (lw_final[,-c(1:6)]-ls_final[,-c(1:6)]*0.028)/es_final[-c(1:6)]
  
  
  resultados = list(lw = lw_final,
                    ls = ls_final,
                    es = es_final,
                    ed = ed_final,
                    eu = eu_final,
                    lu = lu_final,
                    rrs = rrs)
  
  return(resultados)
  
}
