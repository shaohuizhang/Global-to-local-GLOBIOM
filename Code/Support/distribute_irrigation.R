
# Prepare input data for distribution
prep_f <- function(crop){
  # Grid
  crop_gridID <- lc$gridID[lc$lc %in% crop & lc$area >0]
  crop_grid <- grid[grid$gridID %in% crop_gridID,]
  crop_grid <- spTransform(crop_grid, CRS(crs_ir)) 

  # Available crop area for irrigation
  crop_area <- filter(lc, lc %in% crop, area >0)

  # input data
  df <- crop_grid@data %>%
    mutate(ID = as.numeric(row.names(.)),
         ir_area = 0) %>%
    left_join(crop_area) %>%
    mutate(total_area = area)
  return(df)
}

# Function to distribute irrigated area (point information) over grid cells using nearest distance
distribute_f <- function(df, crop, i=1){
  
  # Grid
  crop_gridID <- lc$gridID[lc$lc %in% crop & lc$area >0]
  crop_grid <- grid[grid$gridID %in% crop_gridID,]
  crop_grid <- spTransform(crop_grid, CRS(crs_ir)) 
  
  # Irrigated areas
  crop_ir_geo <- ir_geo[ir_geo$lc %in% crop,]
  
  # Extract irrigated area
  ip <- crop_ir_geo[i, ]
  
  # Number of iterations
  n_ir <- nrow(crop_ir_geo@data)
  ia <- ip@data$value
  
  # Calculate nearest grid cells
  dist <- gDistance(ip, crop_grid, byid = T)
  dist <- data.frame(ID = row.names(dist), distance = as.vector(dist)) %>%
    arrange(distance) %>%
    mutate(ID  = as.numeric(ID))
  
  # Check if irrigated area can be allocated
  if(sum(df$area) < ia) {
    stop("Not enough area, allocation stopped")
  }
  
  # Loop to allocate irrigated area over grids cells, starting with nearest  
  for (j in dist$ID){
    if(df$area[df$ID == j]-ia >= 0){
      alloc = ia
      df$ir_area[df$ID == j] <- df$ir_area[df$ID == j] + alloc
      df$area[df$ID == j] <- df$area[df$ID == j] - alloc
      ia = ia - alloc
      break
    } else {
      alloc <- df$area[df$ID == j]
      df$ir_area[df$ID == j] <- df$ir_area[df$ID == j] + alloc
      df$area[df$ID == j] <- df$area[df$ID == j] - alloc
      ia = ia - alloc
    }
  }
  i=1+i
  print(paste("counter", i, sep = " "))
  print(paste("cum ir_area", sum(df$ir_area)), sep = " ")
  if(i <= n_ir) return(distribute_f(df, crop, i = i)) 
  else return(df)
}


