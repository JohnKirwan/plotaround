Cplot_dens <- function(x, alpha, ax, rho.col, out.by, kappa.ci, ...){
	if(missing(ax)){ax <- F}#fit mean axis (not direction)
	if(missing(rho.col)){rho.col <- 'deeppink4'}#fit mean axis (not direction)
	if(missing(out.by)){out.by = 0.05} # draw outside (or inside) by
	if(missing(alpha)){alpha = 0.05} #proportion NOT to plot across
		#spacing of stacked points, now automatically stacks towards centre unless otherwise specified
	if(missing(kappa.ci)){kappa.ci = F} #proportion NOT to plot across
		#spacing of stacked points, now automatically stacks towards centre unless otherwise specified
	sp <- 0.04

		if(!(	sum('mycirc'%in% ls())	)){
		mycirc <- function(angles, clock){
			if(missing(clock)){clock <- T}
			if(clock){
			return(		as.circular(angles,units='radians',type='angles',
			modulo='2pi',zero=pi/2,rotation='clock',	template='none')	)
				}else{
				as.circular(angles,units='radians',type='angles',
				 modulo='2pi',zero=pi/2,rotation='counter',template='none')
				}#if(clock)
			}#mycirc <- function(angles, clock)
	}#if(!(	sum('mycirc'%in% ls())	))

	if(ax == F){
			#circular plot settings
	increments <- 5 #radians
	zr <- pi/2 #start at top of screen (pi*	90	/180)
	bn <- 72 #bins
	tcl <- rgb(1,1,1,0)#transparent colour
	pcl <- rgb(.3,.1,.1,.5)#point colour
	#plot characters
	lw <- 0.5 #line width
	pnt <- 2.5 #point size
	arw <- 10 #arrowhead angle
	arl <- 0.1 #arrowhead length
	#	set up input variables
	hd <- as.circular(x,units='radians',type='angles',
			modulo='2pi',zero=pi/2,rotation='clock',	template='none')
	sm <- summary(hd)
	sv <- sd.circular(hd, na.rm=T) # currently redundant
	lbl <- 90*(1:4-1)
	plot(hd, col=tcl, main="", zero=zr, axes=F, shrink=1,tol=0.075)
	axis.circular(1, at = mycirc(lbl*pi/180), labels = paste0(lbl, 'º'))
	par(new=T)
	plot.circular(hd, col=tcl,main="",zero=zr,axes=F,shrink=1.05,tol=0.075)
	points(hd,stack=T,bin=bn,sep=-sp,zero=zr,...)
	par(new=T)
	plot(hd, col=tcl, main="", zero=zr, axes=F, shrink=1,tol=0.075)
	if(kappa.ci){
	lines.circular(mycirc(rep(mean(hd, na.rm =T),2)),
	   y = -(1-A1(mle.vonmises.bootstrap.ci(hd, alpha = 0.05, bias = T,
	  reps = 10^4)$kappa.ci)), col = rgb(1,0,0,0.2), lwd = 7, lend = 'butt')
	}#if(kappa.ci)
	arrows.circular( mean(hd, na.rm =T),zero=zr, col = rho.col,lwd=3,
		 length=arl,angle=arw,shrink = rho.circular(hd,na.rm =T))
	#find the lower and upper confidence interval bounds
	ci <- mle.vonmises.bootstrap.ci(hd, alpha = alpha, bias = T,
          reps = 10^4)$mu.ci
	# if(abs(diff(ci)) > 180){#check if the default angle is "little"
		# #otherwise, plot in the reverse direction
		# sq = seq( from = as.numeric(min(ci)),
					# to = -as.numeric(360 - max(ci)), length.out = 10^3)}else{
	#if it is "little" plot from min to max
		# sq = seq( from = as.numeric(min(ci)),
					# to = as.numeric(max(ci)), length.out = 10^3)
	# }#if(abs(diff(qt)) > pi)

	if(sign(max(ci) - mean(hd, na.rm =T)) == -1 | sign(mean(hd, na.rm =T) - min(ci)) == -1 ){
	  #check if mean is right or left of max CI
		#otherwise, plot in the reverse direction
		sq = seq( from = as.numeric(min(ci)),
					to = -as.numeric(360 - max(ci)), length.out = 10^3)}else{
					  #if it is "little" plot from min to max
		sq = seq( from = as.numeric(min(ci)),
					to = as.numeric(max(ci)), length.out = 10^3)
	}#if(abs(diff(qt)) > pi)

	#draw a line in coordinates of rcos(x) rsin(y)
	#any other line arguments (i.e. col or lwd) input as "..."
	lines.circular(mycirc(sq), rep(out.by,10^3), col = rho.col, lwd = 3)

	circ_dens <- circ_dens_brm(as.numeric(hd),kernel="vonmises",bw=10) # circ density via brms
	lines(circ_dens,zero=pi/2,rotation='clock') # apply lines.density.circular

	}#if(ax == F)
}###################	END OF FUNCTION	###########################

Cplot2 <- function(x, alpha, ax, rho.col, out.by, kappa.ci, ...){
  if(missing(ax)){ax <- F}#fit mean axis (not direction)
  if(missing(rho.col)){rho.col <- 'deeppink4'}#fit mean axis (not direction)
  if(missing(out.by)){out.by = 0.05} # draw outside (or inside) by
  if(missing(alpha)){alpha = 0.05} #proportion NOT to plot across
  #spacing of stacked points, now automatically stacks towards centre unless otherwise specified
  if(missing(kappa.ci)){kappa.ci = F} #proportion NOT to plot across
  #spacing of stacked points, now automatically stacks towards centre unless otherwise specified
  sp <- 0.04

  if(!(	sum('mycirc'%in% ls())	)){
    mycirc <- function(angles, clock){
      if(missing(clock)){clock <- T}
      if(clock){
        return(		as.circular(angles,units='degrees',type='angles',
                             modulo='2pi',zero=pi/2,rotation='clock',	template='none')	)
      }else{
        as.circular(angles,units='degrees',type='angles',
                    modulo='2pi',zero=pi/2,rotation='counter',template='none')
      }#if(clock)
    }#mycirc <- function(angles, clock)
  }#if(!(	sum('mycirc'%in% ls())	))

  if(ax == F){
    #circular plot settings
    increments <- 5 #degrees
    zr <- pi/2 #start at top of screen (pi*	90	/180)
    bn <- 10*10*360/5 #bins
    degrad <- 180/pi #conversion from radians to degrees
    tcl <- rgb(1,1,1,0)#transparent colour
    pcl <- rgb(.3,.1,.1,.5)#point colour
    #plot characters
    lw <- 0.5 #line width
    pnt <- 2.5 #point size
    arw <- 10 #arrowhead angle
    arl <- 0.1 #arrowhead length
    #	set up input variables
    hd <- as.circular(x,units='degrees',type='angles',
                      modulo='2pi',zero=pi/2,rotation='clock',	template='none')
    sm <- summary(hd)
    sv <- degrad*sd.circular(hd, na.rm=T)
    lbl <- 90*(1:4-1)
    plot(hd, col=tcl, main="", zero=zr, axes=F, shrink=1,tol=0.075)
    axis.circular(1, at = mycirc(lbl), labels = paste0(lbl, 'º'))
    par(new=T)
    plot.circular(hd, col=tcl,main="",zero=zr,axes=F,shrink=1.05,tol=0.075)
    points(hd,stack=T,bin=bn,sep=-sp,zero=zr,...)
    par(new=T)
    plot(hd, col=tcl, main="", zero=zr, axes=F, shrink=1,tol=0.075)
    if(kappa.ci){
      lines.circular(mycirc(rep(mean(hd, na.rm =T),2)), y = -(1-A1(mle.vonmises.bootstrap.ci(hd, alpha = 0.05, bias = T, reps = 10^4)$kappa.ci)), col = rgb(1,0,0,0.2), lwd = 7, lend = 'butt')
    }#if(kappa.ci)
    arrows.circular( mean(hd, na.rm =T),zero=zr, col = rho.col,lwd=3,
                     length=arl,angle=arw,shrink = rho.circular(hd,na.rm =T))
    #find the lower and upper confidence interval bounds
    ci <- mle.vonmises.bootstrap.ci(hd, alpha = alpha, bias = T,
                                    reps = 10^4)$mu.ci
    # if(abs(diff(ci)) > 180){#check if the default angle is "little"
    # #otherwise, plot in the reverse direction
    # sq = seq( from = as.numeric(min(ci)),
    # to = -as.numeric(360 - max(ci)), length.out = 10^3)}else{ #if it is "little" plot from min to max
    # sq = seq( from = as.numeric(min(ci)),
    # to = as.numeric(max(ci)), length.out = 10^3)
    # }#if(abs(diff(qt)) > pi)

    if(sign(max(ci) - mean(hd, na.rm =T)) == -1 | sign(mean(hd, na.rm =T) - min(ci)) == -1 ){#check if mean is right or left of max CI
      #otherwise, plot in the reverse direction
      sq = seq( from = as.numeric(min(ci)),
                to = -as.numeric(360 - max(ci)), length.out = 10^3)}else{ #if it is "little" plot from min to max
                  sq = seq( from = as.numeric(min(ci)),
                            to = as.numeric(max(ci)), length.out = 10^3)
                }#if(abs(diff(qt)) > pi)

    #draw a line in coordinates of rcos(x) rsin(y)
    #any other line arguments (i.e. col or lwd) input as "..."
    lines.circular(mycirc(sq), rep(out.by,10^3), col = rho.col, lwd = 3)
  }#if(ax == F)
}
#################################################################

circ_dens_brm <-
  function (x, z = NULL, bw, adjust = 1, type = c("K", "L"),
            kernel = c("vonmises", "wrappednormal"),
            na.rm = FALSE, shrink=2,
            from = circular(0), to = circular(2 * pi), n = 512, K = NULL,
            min.k = 10, control.circular = list(), ...)
  {   name <- deparse(substitute(x))
  data <- x
  if (!is.numeric(from))
    stop("argument 'from' must be numeric")
  if (!is.numeric(to))
    stop("argument 'to' must be numeric")
  if (!is.finite(from))
    stop("non-finite `from'")
  if (!is.finite(to))
    stop("non-finite `to'")
  if (!is.numeric(n))
    stop("argument 'n' must be numeric")
  n <- round(n)
  if (n <= 0)
    stop("argument 'n' must be integer and positive")
  if (!is.numeric(x))
    stop("argument 'x' must be numeric")
  if (!is.null(z) && is.circular(z)) {
    datacircularp <- circularp(z)
  }
  else if (is.circular(x))
    datacircularp <- circularp(x)
  else {
    datacircularp <- list(type = "angles", units = "radians",
                          template = "none", modulo = "asis", zero = 0,
                          rotation = "counter")
  }
  dc <- control.circular
  if (is.null(dc$type))
    dc$type <- datacircularp$type
  if (is.null(dc$units))
    dc$units <- datacircularp$units
  if (is.null(dc$template))
    dc$template <- datacircularp$template
  if (is.null(dc$modulo))
    dc$modulo <- datacircularp$modulo
  if (is.null(dc$zero))
    dc$zero <- datacircularp$zero
  if (is.null(dc$rotation))
    dc$rotation <- datacircularp$rotation
  if (dc$modulo == "pi")
    stop("The function does not work yet for modulo='pi'")
  x <- conversion.circular(x, units = "radians", zero = 0,
                           rotation = "counter")
  attr(x, "class") <- attr(x, "circularp") <- NULL
  from <- conversion.circular(from, units = "radians",
                              zero = 0, rotation = "counter")
  attr(from, "class") <- attr(from, "circularp") <- NULL
  to <- conversion.circular(to, units = "radians", zero = 0,
                            rotation = "counter")
  attr(to, "class") <- attr(to, "circularp") <- NULL
  kernel <- match.arg(kernel)
  x <- as.vector(x)
  x.na <- is.na(x)
  if (any(x.na)) {
    if (na.rm)
      x <- x[!x.na]
    else stop("x contains missing values")
  }
  x.finite <- is.finite(x)
  if (any(!x.finite)) {
    x <- x[x.finite]
  }
  nx <- length(x)
  if (is.null(z)) {
    z <- circular(seq(from = from, to = to, length = n))
  }
  else {
    if (!is.numeric(z))
      stop("argument 'z' must be numeric")
    namez <- deparse(substitute(z))
    z.na <- is.na(z)
    if (any(z.na)) {
      if (na.rm) {
        z <- z[!z.na]
      }
      else {
        stop("z contains missing values")
      }
    }
    z.finite <- is.finite(z)
    if (any(!z.finite)) {
      z <- z[z.finite]
    }
  }
  zz <- conversion.circular(z, dc$units, dc$type, dc$template,
                            dc$modulo, dc$zero, dc$rotation)
  z <- conversion.circular(z, units = "radians", zero = 0,
                           rotation = "counter")
  attr(z, "class") <- attr(z, "circularp") <- NULL
  z <- as.vector(z)
  bw <- adjust * bw
  if (!is.numeric(bw))
    stop("argument 'bw' and 'adjust' must be numeric")
  if (!is.finite(bw))
    stop("non-finite `bw'")
  if (bw <= 0)
    stop("`bw' is not positive.")
  y <- DensityCircularRad2(x = x, z = z, bw = bw, kernel = kernel,
                           K = K, min.k = min.k)
  structure(list(data = data, x = zz, y = y, bw = bw, n = nx,
                 kernel = kernel,
                 call = match.call(), data.name = name,
                 has.na = FALSE), class = "density.circular")
  }


### End of Function ###
