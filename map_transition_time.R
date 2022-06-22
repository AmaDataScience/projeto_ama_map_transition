pacotes <- c("tidyverse","sf","tmap","rgdal","rgeos","adehabitatHR","knitr",
             "kableExtra", 'gganimate', 'paletteer', 'ggmap', 'gifski', 
             'ggplot2', 'lubridate')

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

# Carregando a base de dados
load("base_ama_long_lat.RData")
load("endereco_long_lat.RData")

# Importando um objeto do tipo 'sp' do Estado de SP
sao_paulo_rgdal <- readOGR(dsn="shapefile_sp", layer="estado_sp", 
                           verbose=FALSE, stringsAsFactors=FALSE)

# Transformando em um objeto do tipo 'Data frame'
sao_paulo <- fortify(model=sao_paulo_rgdal, region="NM_MUNICIP")

# Delimitando área
min_long <- min(sao_paulo$long)
max_long <- max(sao_paulo$long)
min_lat <- min(sao_paulo$lat)
max_lat <- max(sao_paulo$lat)

# Limpando da base de dados os dados incompatíveis
base_ama <- base_ama[base_ama$latitude > min_lat  & 
                           base_ama$latitude < max_lat & 
                           base_ama$longitude > min_long  & 
                           base_ama$longitude < max_long, ]

# Agregação de valores
credito_localizacao <- aggregate(x = base_ama$credito, 
                                 by = list(base_ama$endereco, 
                                           base_ama$dt_emissao), 
                                 FUN = sum)

# Nomes das variáveis
names(credito_localizacao) <- c('endereco', 'data', 'credito')

# Ajuste do formato data
credito_localizacao$year <- year(credito_localizacao$data)
credito_localizacao$month <- month(credito_localizacao$data)
credito_localizacao$data <- paste0('01', '-', credito_localizacao$month, '-',
  credito_localizacao$year) 

credito_localizacao$data <- as.Date(credito_localizacao$data, 
                                    format = '%d-%m-%Y')

# Join com as geolocalizações
credito_localizacao <- left_join(credito_localizacao, endereco_long_lat, 
                                 by = 'endereco') 

# Gerando o mapa
base_map <- ggplot(data = sao_paulo, mapping = aes(x = long, y = lat, 
                                                   group = id)) +
  geom_polygon(color = "black", fill = "white") +
  coord_quickmap() +
  theme_void() 

# Visualizando
base_map

# Adicionando o layer com as informações de interesse
map_with_data <- base_map +
  geom_point(data = credito_localizacao, aes(x = longitude, y = latitude,
                                             color = credito, size = credito,
                                             group= c(data)))+
  scale_color_viridis_b() +
  coord_quickmap(xlim = c(min_long, max_long),  ylim = c(min_lat, max_lat))

map_with_data

map_with_animation <- map_with_data +
  transition_time(data) +
  ggtitle('Data: {frame_time}',
          subtitle = 'Frame {frame} of {nframes}')
#num_years <- max(credito_localizacao$data) - min(credito_localizacao$data) + 1
unique(credito_localizacao$data)

num_months <- 5*12 # 5 anos

map_with_shadow <- map_with_animation +
  shadow_mark()
animate(map_with_shadow, nframes = num_months, fps = 2)

anim_save("credito_sp.gif", map_with_shadow, nframes = num_months, fps = 3)
