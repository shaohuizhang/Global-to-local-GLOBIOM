library("gdxrrw")

### LINK GAMS LIBRARIES
GAMSPath <- "C:\\GAMS\\win64\\24.4"
igdx(GAMSPath)



df <- deptots
variables <- c("adm", "short_name")


# Function to create val for parameter prep file
val_gdx <- function(df, variables){
  val <- df[!names(df) %in% variables]
  val <- as.matrix(val) 
  dimnames(val) <- NULL
  val <- unname(val)
  return(val)
}

# Function to create uels for parameter prep file
uels_gdx <- function(df, variables){
  uels <- df[names(df) %in% variables]
  uels <- lapply(uels, factor)
  uels <- lapply(uels,levels)
  return(uels)
}
  
name = "test"

# Function prepare parameter gdx file
para_gdx <- function(df, variables, name, ts = NULL, type = "parameter",  form = "sparse"){
  
  # Prepare input
  val <- val_gdx(df, variables)
  uels <- uels_gdx(df, variables)
  dim <- length(uels)
  
  # Create parameter list
  para <- list()
  para[["val"]] <- val    # Array containing the symbol data
  para[["name"]] <- name  # Symbol name (data item)
  para[["dim"]] <- dim    # Dimension of symbol = levels
  para[["ts"]] <- ts      # Explanatory text for the symbol
  para[["uels"]] <- uels  # Unique Element Labels (UELS) (levels)
  para[["type"]] <- type  # Type of the symbot
  para[["form"]] <- form  # Representation, sparse or full
  return(para)
}


tests_list <- para_gdx(deptots, variables, "deptots", "this is a test")







wgdx("testXX.gdx", para)

### CREATE GAMS INPUT DATA FILES
# CHECK: WILL BE REPLACED BY SCRIPT THAT WRITES GDX

deptotsx <- deptots %>%
  mutate(adm = factor(adm),
         short_name = factor(short_name))


gamsPath <- "C:/GAMS/win64/24.4"
write_gdx_f <- function(df, gamsPath = NULL)
  if(is.null(gamsPath)) 
  {stop("GAMS path not set")}

# Load GAMS library
library("gdxrrw")
igdx(gamsPath)

# Create filename
GDXfile <- outfile

# Extract levels
level_file <- extractLevels_f(deptotsx)
level_file2 <- extractLevels_f(availx)

# Create parameter file
fac_num <- factor2Number_f(deptotsx)
fac_num2 <- factor2Number_f(availx)
parameter_file <- GDXPara_f(fac_num, "demand", "water demand", unname(level_file))
parameter_file2 <- GDXPara_f(fac_num2, "gridID", "lc", unname(level_file2))



# Create GDX file
wgdx.lst("test.gdx", parameter_file, parameter_file2)
wgdx("test.gdx", list = parameter_file, list = parameter_file2)
wgdx.lst("test.gdx", list = c(parameter_file, parameter_file2))
wgdx.lst("test.gdx", b)


# Create set lists
adm <- GDXSet_f("adm", "adm", unname(level_file["adm"]))
short_name <- GDXSet_f("short_name", "short_name", unname(level_file["short_name"]))
