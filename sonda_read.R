### Extração dos dados da sonda ####


require(data.table)
require(dplyr)
require(tidyr)


##

read_sonda = function(path_sonda) {
  
  dados = read.table(path_sonda, header=T, skip =8, sep = ',')
  
  names(dados) = c("Date", "Phyco", "CDOM", "Chla", "Depth", "Temperature")

  
  phyco.quanti = quantile(dados$Phyco)
  CDOM.quanti = quantile(dados$CDOM)
  Chla.quanti = quantile(dados$Chla)
  Temperature.quanti = quantile(dados$Temperature)
  
  
dados.phyco = filter(dados, Phyco > phyco.quanti[2] & Phyco < phyco.quanti[3]) %>% 
  select(Phyco)  %>% as.matrix() %>% as.numeric() %>% median()

dados.CDOM = filter(dados, CDOM > CDOM.quanti[2] & CDOM < CDOM.quanti[3]) %>% 
  select(CDOM) %>% as.matrix() %>% as.numeric() %>% median()

dados.Chla = filter(dados, Chla > Chla.quanti[2] & Chla < Chla.quanti[3]) %>% 
  select(Chla) %>% as.matrix() %>% as.numeric() %>% median()

dados.Temperature = filter(dados, Temperature > Temperature.quanti[2] & Temperature < Temperature.quanti[3]) %>% 
  select(Temperature)  %>% as.matrix() %>% as.numeric() %>% median()
  
df = data.frame(Date = dados$Date[1], Phyco = dados.phyco, CDOM = dados.CDOM, Chla = dados.Chla, 
                Temperature = dados.Temperature)
  
  
dados.phycosd = filter(dados, Phyco > phyco.quanti[2] & Phyco < phyco.quanti[3]) %>% 
  select(Phyco)  %>% as.matrix() %>% as.numeric() %>% sd()

dados.CDOMsd = filter(dados, CDOM > CDOM.quanti[2] & CDOM < CDOM.quanti[3]) %>% 
  select(CDOM) %>% as.matrix() %>% as.numeric() %>% sd()

dados.Chlasd = filter(dados, Chla > Chla.quanti[2] & Chla < Chla.quanti[3]) %>% 
  select(Chla) %>% as.matrix() %>% as.numeric() %>% sd()

dados.Temperaturesd = filter(dados, Temperature > Temperature.quanti[2] & Temperature < Temperature.quanti[3]) %>% 
  select(Temperature)  %>% as.matrix() %>% as.numeric() %>% sd()
  
dfsd = data.frame(Date = dados$Date[1], Phyco = dados.phycosd, CDOM = dados.CDOMsd, Chla = dados.Chlasd, 
                Temperature = dados.Temperaturesd)

res = list(df, dfsd)

return(res)  

  
}

read_sonda_sem_virgula = function(path_sonda) {
  
  dados.open = read.delim(path_sonda, skip = 9, sep = ',', header=F)
  
  dados = data.frame(Date = dados.open$V1,
                     Phyco = dados.open$V2+dados.open$V3/100, 
                     CDOM = dados.open$V4+dados.open$V5/100,
                     Chla = dados.open$V6+dados.open$V7/100,
                     Depth = dados.open$V8+dados.open$V9/100,
                     Temperature = dados.open$V10 + dados.open$V11/100)
  
  names(dados) = c("Date", "Phyco", "CDOM", "Chla", "Depth", "Temperature")

  
  phyco.quanti = quantile(dados$Phyco)
  CDOM.quanti = quantile(dados$CDOM)
  Chla.quanti = quantile(dados$Chla)
  Temperature.quanti = quantile(dados$Temperature)
  
dados.phyco = filter(dados, Phyco > phyco.quanti[2] & Phyco < phyco.quanti[3]) %>% 
  select(Phyco)  %>% as.matrix() %>% as.numeric() %>% median()

dados.CDOM = filter(dados, CDOM > CDOM.quanti[2] & CDOM < CDOM.quanti[3]) %>% 
  select(CDOM) %>% as.matrix() %>% as.numeric() %>% median()

dados.Chla = filter(dados, Chla > Chla.quanti[2] & Chla < Chla.quanti[3]) %>% 
  select(Chla) %>% as.matrix() %>% as.numeric() %>% median()

dados.Temperature = filter(dados, Temperature > Temperature.quanti[2] & Temperature < Temperature.quanti[3]) %>% 
  select(Temperature)  %>% as.matrix() %>% as.numeric() %>% median()
  
df = data.frame(Date = dados$Date[1], Phyco = dados.phyco, CDOM = dados.CDOM, Chla = dados.Chla, 
                Temperature = dados.Temperature)

return(df)  

  
}

dados = list.files(path = 'F:/BONDS_2022/Sonda/', full.names = T, pattern = '.csv')

sonda = data.frame(Ponto_endereço = dados, 
                   Data =1 , 
                   Phyco=1, 
                   CDOM=1, 
                   Chla=1, 
                   Temp=1)

sondasd = data.frame(Ponto_endereço = dados, 
                   Data =1 , 
                   Phyco=1, 
                   CDOM=1, 
                   Chla=1, 
                   Temp=1)



for(i in 16:length(dados)) {
  
  res = read_sonda(path_sonda = sonda$Ponto_endereço[i])
  
  sonda[i,2:6] = res[[1]]
  sondasd[i,2:6] = res[[2]]
}



sonda$estacoes_id = c("Promissao_04_2022_Ponto_01",
                      "Promissao_04_2022_Ponto_02",
                      "Promissao_04_2022_Ponto_03",
                      "Promissao_04_2022_Ponto_04",
                      "Promissao_04_2022_Ponto_05",
                      "Promissao_04_2022_Ponto_06",
                      "Promissao_04_2022_Ponto_07",
                      "Promissao_04_2022_Ponto_08",
                      "Promissao_04_2022_Ponto_09",
                      "Promissao_04_2022_Ponto_10",
                      "Promissao_04_2022_Ponto_11",
                      "Promissao_04_2022_Ponto_12",
                      "Promissao_04_2022_Ponto_13",
                      "Promissao_04_2022_Ponto_14",
                      "Promissao_04_2022_Ponto_extra01")


sonda$Ponto_endereço

comp = sonda[c(1,4,6,7,8,9,10,11,12),]
compsd = sondasd[c(1,4,6,7,8,9,10,11,12),]

fico = read.table("clipboard", header=F)

SONDA_PC_ESTIMADO = -2E-5*comp$Phyco^2+0.0685*comp$Phyco+2.9096
SONDA_PC_ESTIMADO = 0.0441*comp$Phyco+6.8906
SONDA_PC_ESTIMADO = 16.331*log10(comp$Phyco)-95.974

matplot(x= fico$V2[-1], y = SONDA_PC_ESTIMADO[-1], xlab = "Insitu PC", xlim = c(0,30), ylim = c(0,30), pch = 20)
abline(0,1)


mape(actual = fico$V2[-1], predicted = SONDA_PC_ESTIMADO[-1])


rrs_calculada$rrs_mediana



a = merge(sonda, rrs_calculada$rrs_mediana, by = 'estacoes_id')

a$NDCI = (a$X740 - a$X670)/(a$X740+a$X670)
a$NDPI = (a$X650 - a$X620)/(a$X650+a$X620)



result = ggplot(a) + 
  geom_point(aes(x = NDCI, y = Chla, col = estacoes_id))


ggplotly(result)



















