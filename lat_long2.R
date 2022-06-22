pacotes <- c("osmdata","dplyr")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

# Lendo a base de dados  da AMA
base_ama <- read.csv('base_ama.csv', row.names = NULL, sep = ",")
# base_n_ama <- read.csv('base_n_ama.csv', row.names = NULL, sep = ",")

# Lendo a base de dados com os códigos dos municípios
cod_municipio <- read.csv('cod_municipio.csv',  row.names = NULL, sep = ";")
cod_municipio <- cod_municipio[-2]
names(cod_municipio) <- c('nome_municipio', 'municipio')

base_ama <- left_join(base_ama, cod_municipio, by = 'municipio')  

base_ama$endereco <- paste(base_ama$bairro, base_ama$nome_municipio,
                           sep = ' ')

base_endereco <- base_ama[, c('endereco', 'bairro', 'nome_municipio')]
endereco_long_lat <- unique(base_endereco)

for(i in 1:nrow(endereco_long_lat)){
  
  coord <- getbb(endereco_long_lat[i,1])
  
  if(is.na(coord)){
    coord <- getbb(endereco_long_lat$bairro[i])
    coord_mun <- getbb(endereco_long_lat$nome_municipio[i])
    
    if(sum(is.na(c(coord, coord_mun)))>=1) {
      
      next
      
    } else {
    
      if(coord[1] >= coord_mun[1] & coord[3] <= coord_mun[3] & 
         coord[2] >= coord_mun[2] & coord[4] <= coord_mun[4]) {
        
        endereco_long_lat$latitude[i] <- mean(c(coord[2],coord[4]))
        endereco_long_lat$longitude[i] <- mean(c(coord[1],coord[3]))
        
        next
      }
      
    }
    next
  }
  endereco_long_lat$latitude[i] <- mean(c(coord[2],coord[4]))
  endereco_long_lat$longitude[i] <- mean(c(coord[1],coord[3]))
  print(i)
}

endereco_long_lat <- endereco_long_lat %>% 
  dplyr::select(endereco, bairro, nome_municipio, longitude, latitude)

base_ama <- left_join(base_ama, endereco_long_lat, by = 'endereco') 
save(endereco_long_lat, file = 'endereco_long_lat.RData')
save(base_ama, file = 'base_ama_long_lat.RData')

