# Loads all data
# Creates one large list named dat

# Get file list
files1 <- list.files('../data')

# Omit temporary Excel files   

files1 <- files1[!grepl('~', files1)]

# Get institutes ()
ids <- gsub('\\.xlsx', '', files1)
names(files1) <- ids

dat <- list()

for(i in ids) {
  
  dat[[i]] <- list() 
  
  # Read in data args
  args <- as.data.frame(read_xlsx(paste0('../data/',files1[i]), sheet = 4, skip = 1))
  
  # Change to vector
  dnames <- args$arg
  args <- as.list(args$val)
  names(args) <- dnames
	
  # Sort out comp argument  
  if(isNumericString(args$comp)) args$comp <- as.numeric(args$comp) 
  if(isNumericString(args$temp)) args$temp <- as.numeric(args$temp)
  if(isNumericString(args$pres)) args$pres <- as.numeric(args$pres)
  
  # And others
  if(isNumericString(args$temp.init))args$temp.init <- as.numeric(args$temp.init)
  if(isNumericString(args$pres.init))args$pres.init <- as.numeric(args$pres.init)
  if(isNumericString(args$pres.resid)) args$pres.resid <- as.numeric(args$pres.resid)
  if(isNumericString(args$pres.amb))args$pres.amb <- as.numeric(args$pres.amb)
  
  args$interval <- as.logical(args$interval)
  args$dry <- as.logical(args$dry)
  args$absolute <- as.logical(args$absolute)
  
  if(isNullOrNa(args$cmethod)) args$cmethod <- 'removed'  ## --> What does the "removed" stand for?
  
  #read in setup and biogas
  setup <- as.data.frame(read_xlsx(paste0('../data/',files1[i]), sheet = 1, skip = 1))
  biogas <- as.data.frame(read_xlsx(paste0('../data/',files1[i]), sheet = 3, skip = 1))
  
  ## Sort out substrate names--use standard substrate names
  #setup$substrate <- NA
  #setup[setup[, args$descrip.name] == args$inoc.name, 'substrate'] <- 'Inoc'
  #setup[grepl('A$', setup[, args$descrip.name]), 'substrate'] <- 'SA'
  #setup[grepl('B$', setup[, args$descrip.name]), 'substrate'] <- 'SB'
  #setup[grepl('C$', setup[, args$descrip.name]), 'substrate'] <- 'SC'
  #setup[grepl('cel', setup[, args$descrip.name], ignore.case = TRUE), 'substrate'] <- 'CEL'
  
  #Set defualt pressure unit if unspecified
  if(is.na(args$unit.pres)) {
    args$unit.pres <- 'atm'   
  }  

  # Add fixed (true, as opposed to the measured ones) VS concentrations   
  fvs <- data.frame(substrate = c('CEL',   'SA', 'SB', 'SC', 'BK'), 
              c.vs2.subf = c(0.948, 0.829, 0.865, 0.810, 0.0))
  
  setup <- merge(setup, fvs, by = 'substrate')
  
  setup$m.sub.vs <- setup$c.vs2.subf*setup$m.sub

  # Get inoc VS
  setup$c.vs2.inoc <- mean(setup[setup$substrate == 'BK', 'c.TS']/100 * setup[setup$substrate == 'BK', 'c.VS']/100, na.rm = TRUE)
  setup$m.inoc.vs <- setup$c.vs2.inoc*setup$m.inoc
  setup$ISR <- setup$m.inoc.vs/setup$m.sub.vs
  
  dat[[i]][['setup']] <- as.data.frame(setup)
  dat[[i]][['biogas']] <- as.data.frame(biogas)
  dat[[i]][['args']] <- args 
  
  # No comp data frame for constant comp
  # Otherwise comp data frame is called comp (from second tab in spreadsheet)
  if(!is.na(args$comp) & is.numeric(args$comp))  {
    dat[[i]][['comp']] <- args$comp
  } else if(args$data.struct == 'longcombo') {
    dat[[i]][['comp']] <- NULL
  } else {
    comp <- as.data.frame(read_xlsx(paste0('../data/',files1[i]), sheet = 2, skip = 1))
    dat[[i]][['comp']] <- comp
  }

  
}


