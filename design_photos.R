library(tidyverse)

baseDir <- '~/mpgPostdoc/projects/plantID/gh_data/'

speices <- c('gaiari', #Gaillardia aristata
                   'ratcol', #Ratibita columnifera
                   'achmil', #Achilea millefolium
                   'psespi', #Pseudoroegneria spicata
                   'poasec', #Poa seconda
                   'elytra', #Elymus trachycaulus
                   'linlew', #Linum lewisii
                   'vendub', #Venenata dubia
                   'brotec', #Bromus tectorum
                   'centsto' #Centaurea stoebe
                   )

month <- c('april','may','june','july','august','september','november')

base_grid <- expand.grid(as.character(speices), as.character(month)) %>% as.data.frame()
colnames(base_grid) <- c('species','month')
base_grid$species <- as.character(base_grid$species)
base_grid$month <- as.character(base_grid$month)

n <- 40
full_grid_ordered <- do.call("rbind", replicate(n, base_grid, simplify = FALSE))

plants_in_set <- 4
photos_set_seq <- 1:(nrow(full_grid_ordered)/plants_in_set)

layout <- raster(nrow = 5, ncol =4)
values(layout) <- 1:ncell(layout)
center_cells <- c(6,7,10,11)
corner_cells <- c(1,4,17,20)

photo_set = unlist(lapply(FUN = function(x){rep(x, plants_in_set)}, X = photos_set_seq))
centered_pos = rep(center_cells, length(photos_set_seq))
corner_pos = rep(corner_cells, length(photos_set_seq))
layoutPts <- rasterToPoints(layout) %>% as.data.frame()

set.seed(123)
full_grid <- full_grid_ordered %>% 
  sample_frac(1) %>% 
  mutate(ind = row_number(),
         photo_set = photo_set,
         center_pos = centered_pos,
         corner_pos = corner_pos) %>% 
  group_by(photo_set) %>%
  mutate(random_pos = sample(1:20, size = 4, replace = FALSE)) %>%
  ungroup()

write.csv(full_grid, paste0(baseDir, 'photo_layout.csv'), row.names = FALSE)
write.csv(layoutPts, paste0(baseDir, 'pts_layout.csv'), row.names = FALSE)

  
  
  