# Works up data -- calls cumBg() and summBg()

for(i in names(dat)) {
  
  cat('\n', i, i, i, i, '\n')   
  setup <- dat[[i]][['setup']]
  biogas <- dat[[i]][['biogas']]
  comp <- dat[[i]][['comp']]
  args <- dat[[i]][['args']]
  
  
  if(args$dat.type == 'vol') {
    
    cbg <- cumBg(biogas, comp = comp, temp = args$temp, pres = args$pres,
                 interval = args$interval,
                 data.struct = args$data.struct,
                 id.name = args$id.name,
                 time.name = args$time.name, 
                 dat.name = args$dat.name,
                 comp.name = args$comp.name,
                 extrap = TRUE,
                 unit.pres = args$unit.pres,
                 pres.std = biogas:::unitConvert(1, unit = 'atm', to = args$unit.pres),
                 addt0 = TRUE
                 )
    
  } else if(args$dat.type == 'pres') {
    
    cbg <- cumBg(biogas, dat.type = 'pres', comp = comp, temp = args$temp, 
                 interval = args$interval,
                 data.struct = args$data.struct,
                 id.name = args$id.name,
                 time.name = args$time.name, 
                 headspace = setup,
                 vol.hs.name = args$vol.hs.name,
                 pres.resid = args$pres.resid,
                 pres.init = args$pres.init,
                 pres.amb = args$pres.amb,
                 absolute = args$absolute,
                 temp.init = args$temp.init,
                 dat.name = args$dat.name,
                 comp.name = args$comp.name,
                 cmethod = args$cmethod,
                 unit.pres = args$unit.pres,
                 pres.std = biogas:::unitConvert(1, unit = 'atm', to = args$unit.pres),
                 extrap = TRUE,
                 addt0 = TRUE
                 )
    
  } else if(args$dat.type == 'mass') {
    
    cbg <- cumBg(biogas, dat.type = 'mass', temp = args$temp, pres = args$pres, 
                 data.struct = args$data.struct,
                 id.name = args$id.name, time.name = args$time.name, 
                 dat.name = args$dat.name, comp.name = args$comp.name,
                 temp.init = args$temp.init,
                 headspace = setup, vol.hs.name = args$vol.hs, headcomp = 'N2',
                 extrap = TRUE,
                 unit.pres = args$unit.pres,
                 pres.std = biogas:::unitConvert(1, unit = 'atm', to = args$unit.pres),
                 addt0 = TRUE)
 
  }
  

# Get BMP
  bmp1p <- summBg(cbg, setup = setup, id.name = args$id.name, time.name = args$time.name,
                descrip.name = 'substrate', inoc.name = 'BK',
                inoc.m.name = args$inoc.m.name, norm.name = args$norm.name, when = '1p3d', 
                show.more = TRUE, extrap = TRUE, quiet = TRUE)    
  
  bmp1p$end <- '1%'
  bmp1p$rate.met <- attributes(bmp1p)$rate.not.met[1] == ''

  bmp0.5p <- summBg(cbg, setup = setup, id.name = args$id.name, time.name = args$time.name,
                descrip.name = 'substrate', inoc.name = 'BK',
                inoc.m.name = args$inoc.m.name, norm.name = args$norm.name, when = '0.5p3d', 
                show.more = TRUE, extrap = TRUE, quiet = TRUE)    
  
  bmp0.5p$end <- '0.5%'
  bmp0.5p$rate.met <- attributes(bmp0.5p)$rate.not.met[1] == ''

  
  #What is ww?  
  ww <- min(max(cbg[, args$time.name]), 20)
  bmp20 <- summBg(cbg, setup = setup, id.name = args$id.name, time.name = args$time.name,
                descrip.name = 'substrate', inoc.name = 'BK',
                inoc.m.name = args$inoc.m.name, norm.name = args$norm.name, when = ww, 
                show.more = TRUE, extrap = TRUE, quiet = TRUE)    

  bmp20$end <- '20 d'

  ww <- min(max(cbg[, args$time.name]), 30)
  bmp30 <- summBg(cbg, setup = setup, id.name = args$id.name, time.name = args$time.name,
                descrip.name = 'substrate', inoc.name = 'BK',
                inoc.m.name = args$inoc.m.name, norm.name = args$norm.name, when = ww, 
                show.more = TRUE, extrap = TRUE, quiet = TRUE)    

  bmp30$end <- '30 d'

#inoculum yield normalized by inoc vs
    
  inocyld <- summBg(cbg, setup = subset(setup, substrate == 'BK'), id.name = args$id.name, time.name = args$time.name,
                descrip.name = 'substrate', norm.name = 'm.inoc.vs', when = 'meas',
                show.obs = TRUE, extrap = TRUE, quiet = TRUE)

  inocyldmn <- summBg(cbg, setup = subset(setup, substrate == 'BK'), id.name = args$id.name, time.name = args$time.name,
                descrip.name = 'substrate', norm.name = 'm.inoc.vs', when = 'meas',
                show.obs = FALSE, extrap = TRUE, quiet = TRUE)

  inocyldend <- summBg(cbg, setup = subset(setup, substrate == 'BK'), id.name = args$id.name, time.name = args$time.name,
                descrip.name = 'substrate', norm.name = 'm.inoc.vs', when = 'end',
                show.obs = FALSE, extrap = TRUE, quiet = TRUE)

  
 # #inoc rate 1 day mean for 3 replicates    
  inocrate1dmn <- summBg(cbg, setup = subset(setup, substrate == 'BK'), id.name = args$id.name, time.name = args$time.name,
                descrip.name = 'substrate', norm.name = 'm.inoc.vs', when = 1,
                vol.name = 'rvCH4',
                show.obs = FALSE, extrap = TRUE, quiet = TRUE)
  
  cbg <- merge(setup, cbg)

  dat[[i]]$cbg <- cbg
  dat[[i]]$bmp0.5p <- bmp0.5p
  dat[[i]]$bmp1p <- bmp1p
  dat[[i]]$bmp20 <- bmp20
  dat[[i]]$bmp30 <- bmp30
  dat[[i]]$inocyld <- inocyld
  dat[[i]]$inocyldmn <- inocyldmn
  dat[[i]]$inocyldend <- inocyldend

  
}
