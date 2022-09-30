kd_calc <- function(z1, z2, edz1, edz2) {
  
  kd = 1/(z2-z1) * log(edz1/edz2)
  
  return(kd)
  
}

kd_calc_nls <- function(pressure, ed_normalized) {
  
  teste <- data.frame(pressure, ed_normalized)
  names(teste) <- c("Prof", 'Ed')
  teste <- filter(teste, Ed > 0)
  
  a = max(teste$Ed)
  
  irrad_min_1_perc <- a*0.01
  teste = filter(teste, Ed > irrad_min_1_perc)
  
  b = min(teste$Ed)
  
  kd_nls = summary(nls(formula = Ed~a*exp(-x*Prof), data = teste))$coefficients[1]
  
  return(kd_nls)
  
}

#Normaliza kd

ed_norm <- function(ed, es) {
  
  norm_factor <- es[1]/es
  
  ed_normalized = ed*norm_factor
  return(ed_normalized)
  
}


ed_linear.wavelength <- function(ed.ordered) {
  
  kd.df  = ed.ordered
  
  profs.min <- min(kd.df$Pressure)
  
  kd.df$Pressure <- kd.df$Pressure - profs.min
  

  intercepts <- data.frame()
  
  for(i in 7:ncol(kd.df)) {
    
    ponto = kd.df[order(kd.df[,i], decreasing = T),]
    
    df_mod <- data.frame('log' = log(ponto[,i]/ponto[1,i]))
    
    df_mod$prof = ponto$Pressure
    df_mod$ratio_1perWV = ponto[,i]/ponto[1,i]
    
    #Selecionara para zona eufotica com: todos os wavelengths (1) ou pelo 560 (2)
    #df_mod <- filter(df_mod, ed.percent550 > 0.01 )
    df_mod <- filter(df_mod, ratio_1perWV > 0.01 )
    
    
    df_mod <- filter(df_mod, log < 0)
    
    df_mod.fil = df_mod[is.infinite(df_mod$log) == F,] 
    
    
    df_mod.fil = df_mod.fil %>% na.omit()
    

    intercepts[(i-6),1] = -lm(log~0+(prof), data = df_mod.fil)$coefficients[[1]]
    
    
  }
  
  return(intercepts)
  
}




ed_linear.550 <- function(ed.ordered) {
  
  profs.min <- min(ed.ordered$Pressure)
  
  ed.ordered$Pressure <- ed.ordered$Pressure - profs.min
  
  ed.percent550 = data.frame(pos = 1:nrow(ed.ordered), ed_550 = ed.ordered[,150]/ed.ordered[1,150], prof = ed.ordered$Pressure)
  
  max_depth = filter(ed.percent550, ed_550 > 0.01) %>% select('prof') %>% max()
  
  intercepts <- data.frame()
  
  for(i in 7:ncol(ed.ordered)) {
    
    
    ponto = ed.ordered  %>% filter(Pressure < max_depth)
    #ponto = ponto[order(ponto[,i], decreasing = T),] %>% filter(Pressure < max_depth)
    
    ponto = ponto[ponto[,i] > 0,]
    df_mod <- data.frame('ratio' = (ponto[,i]/ponto[1,i]))
    
    df_mod$prof = ponto$Pressure
    
    df_mod$log = log(df_mod$ratio)
    
    df_mod <- filter(df_mod, log < 0)
    
    df_mod.fil = df_mod[is.infinite(df_mod$log) == F,] 
    
    if(nrow(df_mod.fil) < 3) { 
      
      intercepts[(i-6),1] = 0
      
      }
    
    else {
    
    intercepts[(i-6),1] = calc_kd(depth = df_mod.fil$prof, ed_log = df_mod.fil$log)
    
    }
    print(i)
  }
  
  return(intercepts)
  
}

calc_kd = function(depth, ed_log) {
  
  df = data.frame(depth = depth, ed_log = ed_log)
  #df$depth = round(df$depth, 1)
  
  modelo1 = lm(ed_log~0+depth, data = df)
  
  dif = data.frame(RESIDUO = abs(modelo1$residuals),ed_log = df$ed_log, depth= df$depth) %>% filter(RESIDUO < sd(abs(modelo1$residuals))*2)
  
  modelo2 = lm(dif$ed_log~0+dif$depth)
  
  
  coef = -modelo2$coefficients[[1]]
  
  return(coef)
  
}
