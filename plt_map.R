plot_ssh <- function(fn, reef_dt = rf, view = F, lc, uc) {
  
  file <- nc_open(fn)
  
  dt <- as.data.frame(ncvar_get(file, "SSH")) 
  colnames(dt) <- ncvar_get(file, "latitude")
  dt$lon <- ncvar_get(file, "longitude")
  
  dt2 <- dt %>% 
    pivot_longer(!lon, names_to = "lat", values_to = "value") %>%
    mutate(lon = as.numeric(lon), 
           lat = as.numeric(lat))
  
  nc_close(file)
  
  if (view) {
    p <- ggplot() + 
      geom_raster(data = dt2, aes(x = lon, y = lat, fill = value), interpolate = FALSE) +
      scale_fill_gradient(low = lc,
                          high = uc, 
                          na.value = "black") +
      geom_point(data = reef_dt, alpha = 0.7, size = 2,
                 aes(x = Longitude.Degrees, 
                     y = Latitude.Degrees, 
                     color = bleach, 
                     text = paste0("Reef: ", Reef.Name, "<br>", 
                                   "Ocean:", Ocean, "<br>", 
                                   "Country:", Country))) + 
      theme_void() +  
      labs(x = "", y = "", color = "Bleach status") + 
      theme(legend.position = "none")
    
    ggplotly(p) %>%
      layout(plot_bgcolor = "black")
  } else {
  ggplot() + 
      geom_raster(data = dt2, aes(x = lon, y = lat, fill = value), interpolate = FALSE) +
      scale_fill_gradient(low = lc,
                        high = uc, 
                        na.value = "black") +
      geom_point(data = reef_dt, alpha = 0.7, size = 2,
                 aes(x = Longitude.Degrees, y = Latitude.Degrees, color = bleach)) +
      theme_bw() + 
      labs(x = "longitude", y = "latitude", color = "Bleach status")
  }
}

plot_precipitation <- function(fn, reef_dt = rf, view = F, lc, uc) {
  
  file <- nc_open(fn)
  dt <- as.data.frame(ncvar_get(file, "Grid/precipitation")) 
  colnames(dt) <- ncvar_get(file, varid = "Grid/lon")
  dt$lat <- ncvar_get(file, varid = "Grid/lat")
  nc_close(file)  
  
  dt2 <- dt %>% 
    pivot_longer(!lat, names_to = "lon", values_to = "value") %>%
    mutate(lon = as.numeric(lon), 
           lat = as.numeric(lat))

  if (view) {
    p <- ggplot() + 
      geom_raster(data = dt2, aes(x = lon, y = lat, fill = value), interpolate = FALSE) +
      scale_fill_gradient(low = lc,
                          high = uc, 
                          na.value = "black") +
      geom_point(data = reef_dt, alpha = 0.7, size = 2,
                 aes(x = Longitude.Degrees, 
                     y = Latitude.Degrees, 
                     color = bleach, 
                     text = paste0("Reef: ", Reef.Name, "<br>", 
                                   "Ocean:", Ocean, "<br>", 
                                   "Country:", Country))) +
      theme_void() +  
      labs(x = "", y = "", color = "Bleach status") + 
      theme(legend.position = "none")
    
    ggplotly(p) %>%
      layout(plot_bgcolor = "black")
    
  } else {
  ggplot() +
      geom_raster(data = dt2, aes(x = lon, y = lat, fill = value), interpolate = FALSE) +
      scale_fill_gradient(low = lc,
                        high = uc, 
                        na.value = "black") +
      geom_point(data = reef_dt, alpha = 0.7, size = 2,
                 aes(x = Longitude.Degrees, y = Latitude.Degrees, color = bleach)) +
      theme_bw() + 
      labs(x = "longitude", y = "latitude", color = "Bleach status")
  }
}

plot_uv <- function(fn, reef_dt = rf, view = F, lc, uc) {
  
  file <- nc_open(fn)
  dt <- as.data.frame(ncvar_get(file, "HDFEOS/GRIDS/OMI UVB Product/Data Fields/UVindex")) 
  colnames(dt) <- seq(-89.5, 89.5, 1)
  dt$lon <- seq(-179.5, 179.5, 1)
  nc_close(file)  
  
  dt2 <- dt %>% 
    pivot_longer(!lon, names_to = "lat", values_to = "value") %>%
    mutate(lon = as.numeric(lon), 
           lat = as.numeric(lat))
  
  if (view) {
    p <- ggplot() + 
      geom_raster(data = dt2, aes(x = lon, y = lat, fill = value), interpolate = FALSE) +
      scale_fill_gradient(low = lc,
                          high = uc, 
                          na.value = "black") +
      geom_point(data = reef_dt, alpha = 0.7, size = 2,
                 aes(x = Longitude.Degrees, 
                     y = Latitude.Degrees, 
                     color = bleach, 
                     text = paste0("Reef: ", Reef.Name, "<br>", 
                                   "Ocean:", Ocean, "<br>", 
                                   "Country:", Country))) +
      theme_void() +  
      labs(x = "", y = "", color = "Bleach status") + 
      theme(legend.position = "none")
    
    ggplotly(p) %>%
      layout(plot_bgcolor = "black")
  } else {
  ggplot() + 
      geom_raster(data = dt2, aes(x = lon, y = lat, fill = value), interpolate = FALSE) +
      scale_fill_gradient(low = lc,
                        high = uc, 
                        na.value = "black") +
      geom_point(data = reef_dt, alpha = 0.7, size = 2,
                 aes(x = Longitude.Degrees, y = Latitude.Degrees, color = bleach)) +
      theme_void() + 
      labs(x = "longitude", y = "latitude", color = "Bleach status")
  }
}

plot_st <- function(fn, reef_dt = rf, var = c("SALT", "THETA"), view = F, lc, uc) {
  file <- nc_open(fn)
  dt <- as.data.frame(ncvar_get(file, var)[, , 4]) 
  colnames(dt) <- ncvar_get(file, "latitude")
  dt$lon <- ncvar_get(file, "longitude")
  nc_close(file)
  
  dt2 <- dt %>% 
    pivot_longer(!lon, names_to = "lat", values_to = "value") %>%
    mutate(lon = as.numeric(lon), 
           lat = as.numeric(lat))

  if (view) {
    p <- ggplot() + 
      geom_raster(data = dt2, aes(x = lon, y = lat, fill = value), interpolate = FALSE) +
      scale_fill_gradient(low = lc,
                          high = uc, 
                          na.value = "black") +
      geom_point(data = reef_dt, alpha = 0.7, size = 2,
                 aes(x = Longitude.Degrees, 
                     y = Latitude.Degrees, 
                     color = bleach, 
                     text = paste0("Reef: ", Reef.Name, "<br>", 
                                          "Ocean:", Ocean, "<br>", 
                                          "Country:", Country))) +
      theme_void() +  
      labs(x = "", y = "", color = "Bleach status") + 
      theme(legend.position = "none")
    
    ggplotly(p) %>%
      layout(plot_bgcolor = "black")
  } else {
  ggplot() + 
      geom_raster(data = dt2, aes(x = lon, y = lat, fill = value), interpolate = FALSE) +
      scale_fill_gradient(low = lc,
                        high = uc, 
                        na.value = "black") +
      geom_point(data = reef_dt, alpha = 0.7, size = 2,
                 aes(x = Longitude.Degrees, y = Latitude.Degrees, color = bleach)) +
      theme_bw() + 
      labs(x = "longitude", y = "latitude", color = "Bleach status")
  }
}

pcaCharts <- function(x) {
  x.var <- x$sdev ^ 2
  x.pvar <- x.var/sum(x.var)
  print("proportions of variance:")
  print(x.pvar)
  
  par(mfrow=c(2,2))
  plot(x.pvar,xlab="Principal component", ylab="Proportion of variance explained", ylim=c(0,1), type='b')
  plot(cumsum(x.pvar),xlab="Principal component", ylab="Cumulative Proportion of variance explained", ylim=c(0,1), type='b')
  screeplot(x)
  screeplot(x,type="l")
  par(mfrow=c(1,1))
}































