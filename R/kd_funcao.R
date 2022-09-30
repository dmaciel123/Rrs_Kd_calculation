

kd_calculation = function(ed, es) {

  if((dim(ed)[1] == dim(es)[1]) == F) {
    
  print("Organizing dimensions by datetime") 
    
  merged = merge(ed, es, by = 'DateTime')
    
  #Date_Time collumn 5
  ed.new = select(merged, contains(".x"))
  es.new = select(merged, contains(".y"))
    
  ed.new = data.frame(ed.new[,1:4],merged$DateTime, ed.new[,-c(1:4)])
  es.new = data.frame(es.new[,1:4],merged$DateTime, es.new[,-c(1:4)])
  
  names(ed.new) = names(ed)
  names(es.new) = names(es)
  
  ed = ed.new
  es = es.new
  
  }
  
  
ed.pt  = ed
es.pt  = es

es.pt$Pressure = ed.pt$Pressure

ed.ordered <- ed.pt[order(ed.pt$Pressure),] %>% filter(Pressure > 0)# & CommentSub2 == 'prof')
es.ordered <- es.pt[order(es.pt$Pressure),] %>% filter(Pressure > 0)# & CommentSub2 == 'prof')

ed.ordered$Pressure   = as.numeric(ed.ordered$Pressure)*10
es.ordered$Pressure.x = as.numeric(es.ordered$Pressure)*10

n = nrow(ed.ordered)

#Cria data frame para os resultados


results.LIN = data.frame("Wave" = c(400:900))
  


#Normalização do Kd

for(i in 7:ncol(ed.ordered)) {
  
  ed.ordered[,i] <- ed_norm(ed = ed.ordered[,i], 
                      es = es.ordered[,i])
  
}


#Calculo KD equação

### Esta parte filtra os dados espúrios (só escolher os pontos para excluir)
teste_kd = 'errado'

while(teste_kd == 'errado') {
  
  ed.ordered = filter(ed.ordered, X650 > 0)
  
  par(mfrow=c(1,1))
  matplot(x = ed.ordered$Pressure, 
          y = log(ed.ordered[,250]/ed.ordered[1,250]),
          pch = 20, main = ed.ordered$estacoes_id[1],
          xlab = "Pressure (m)", ylab = "log(Ed(z)/Ed(Z1)")
  text(ed.ordered$Pressure, log(ed.ordered[,250]/ed.ordered[1,250]), labels=c(1:nrow(ed.ordered[])), cex= 1, pos = 3)
  
  a = as.numeric(strsplit(x = readline("Entre com os pontos para excluir"), split = ',')[[1]])
  
  ed.ordered = ed.ordered[-a,]
  
  
  r2650 = summary(lm(log(ed.ordered$X650/ed.ordered$X650[1])~0+ed.ordered$Pressure))$adj.r.squared %>% round(3)

  
  par(mfrow=c(1,3))
  
  matplot(x = ed.ordered$Pressure, y = log(ed.ordered[,250]/ed.ordered[1,250]), pch = 20, main = r2650, cex = 3)
  text(ed.ordered$Pressure, log(ed.ordered[,250]/ed.ordered[1,250]), labels=c(1:nrow(ed.ordered[])), cex= 1, pos = 3)
  
  matplot(x = ed.ordered$Pressure[], y = log(ed.ordered[,150]/ed.ordered[1,150]), pch = 20, cex = 3)
  text(ed.ordered$Pressure[],log(ed.ordered[,150]/ed.ordered[1,150]), labels=c(1:nrow(ed.ordered[])), cex= 1, pos = 3)
  
  matplot(x = ed.ordered$Pressure[], y = log(ed.ordered[,40]/ed.ordered[1,40]), pch = 20, cex = 3)
  text(ed.ordered$Pressure[], log(ed.ordered[,40]/ed.ordered[1,40]), labels=c(1:nrow(ed.ordered[])), cex= 1, pos = 3)
  
  
  teste_kd = as.character(readline("Está ok?"))
  
}




#write.csv(ed.ordered, file = paste(names(es.split)[j],".csv", sep = ""))

#Calculo Kd Linear (Mishra 2005)

#ed.ordered = ed.ordered %>% na.omit()

results.LIN$ponto_all_wave <- ed_linear.550(ed.ordered = ed.ordered)[,1]
#results.LIN$ponto_all_wave <- ed_linear.wavelength(ed.ordered = ed.ordered)[,1]

par(mfrow=c(1,1))
#max_values = max(max(results.LIN$ponto_all_wave550), max(results.LIN$ponto_all_wave))

matplot(x = results.LIN$Wave, y = results.LIN$ponto_all_wave,
        type = 'l', lwd = 2) #ylim = c(0,max_values))
#
#par(new=T)
#
#
#matplot(x = results.LIN$Wave, y = results.LIN$ponto_all_wave550,
#        type = 'l', lwd = 2, col = 'red', ylim = c(0,max_values))
#
#legend('topright', legend = c("Wavelength Variable", "550 nm"), lty = c(1,1), col = c('black', 'red'))

return(results.LIN)

}
