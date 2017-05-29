# Function to remove values from very large raster where simple raster operations give memory problems
# https://cran.r-project.org/web/packages/raster/vignettes/functions.pdf
# For progress bar:
# https://www.r-bloggers.com/all-in-on-r%e2%81%b4-progress-bars-on-first-post/?utm_source=feedburner&utm_medium=email&utm_campaign=Feed%3A+RBloggers+%28R+bloggers%29

# Working with large rasters
# Progress bar does not work yet 
remove_value_r_f <- function(x, a, filename) {
   out <- raster(x)
   bs <- blockSize(out)
   out <- writeStart(out, filename, overwrite=TRUE)
   #pb <- pbCreate(bs$n, ...)
     for (i in 1:bs$n) {
             v <- getValues(x, row=bs$row[i], nrows=bs$nrows[i] )
             v[v==a] <- NA
             out <- writeValues(out, v, bs$row[i])
             #pbStep(pb, i)
     }
     out <- writeStop(out)
     #pbClose(pb)
     return(out)
}


keep_value_r_f <- function(x, a, filename) {
  out <- raster(x)
  bs <- blockSize(out)
  out <- writeStart(out, filename, overwrite=TRUE)
  #pb <- pbCreate(bs$n, ...)
  for (i in 1:bs$n) {
    v <- getValues(x, row=bs$row[i], nrows=bs$nrows[i] )
    v[!(v %in% a)] <- NA
    out <- writeValues(out, v, bs$row[i])
    #pbStep(pb, i)
  }
  out <- writeStop(out)
  #pbClose(pb)
  return(out)
}
