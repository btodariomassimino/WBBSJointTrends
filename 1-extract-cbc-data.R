###############################################################################
# Extract CBC data                                                            #
###############################################################################

#------------------ The following variables must be set ----------------------#

locyear <- 2020	# Current year for the folder

#-----------------------------------------------------------------------------#

# The following variables should not usually be changed
countries <- c("uk","england")
types <- c("farms","woods","specs")

# Input file (more input files are defined within the years loop)
catalfile <- "Z:/cbc/catdata/catpp"
splistfile <- paste0("X:/CensusUnit/Trends",locyear,"/CBC/CBCspecies.csv")
countyfile <- "X:/CensusUnit/cbc/cbccountycodes.csv"

# Read in catalogue of CBC plots
catal <- read.fwf(catalfile, widths=c(5,-3,4), col.names=c("plot","county"))

#catal$square <- paste0(substr(catal$gridref,1,4),substr(catal$gridref,6,7))

# Read in the list of species to process
splist <- read.csv(splistfile, stringsAsFactors = F)

# Read in the county codes
countycodes <- read.csv(countyfile)
	
for (i in 1:length(countries)){
	r <- countries[i]
	print (paste("Processing for",r))
	for (s in 1:nrow(splist)) {
		sp <- splist$code[s]
		sp2 <- splist$sp2[s]
		years <- (splist$firstyr[s]:splist$lastyr[s])
		print (paste0("Processing for ",sp," (",sp2,") - ",r))
		# Output file
		outfile <- paste0("X:/CensusUnit/Trends",locyear,"/cbc/",r,
					"/extract/sp",sp,"(",sp2,")-",years[1],"-",
					years[length(years)],"-ex.csv")
		for (t in 1:length(types)){
			ty <- types[t]
			print(paste0("Processing for ",ty))
			for (y in 1:length(years)){
				yr <- years[y]
				filename <- paste0("z:/cbc/ascii/",ty,substr(yr,3,4))
				dat <- readLines(filename)
				dat1 <- gsub("\t","    ", dat)
				plot <- as.numeric(as.character(substr(dat1, 1, 5)))
				spec <- as.numeric(as.character(substr(dat1, 11, 13)))
				countpre <- substr(dat1, 21, 25)
				region <- substr(dat1, 31, 35)
				plotgual <- as.numeric(as.character(substr(dat1, 36,
											 38)))
				alt <- as.numeric(as.character(substr(dat1, 41, 50)))
				daty <- data.frame(plot, spec, countpre, region,
							 plotgual, alt, stringsAsFactors=F)
				
				# Keep only header row of each plot
				header <- subset(daty, !is.na(plot),
						     select=c(plot,spec,plotgual))
				# In the header, spec is the year
				colnames(header)[colnames(header)=="spec"] <- "year"
				# Relabel all rows with correct plot number
				main <- daty
				main$plot2 <- NA
				for (x in 1:nrow(daty)) {
					p <- ifelse(is.na(daty$plot[x]), p, daty$plot[x])
					main$plot2[x] <- p
				}
				# Remove the header row
				main2 <- subset(main, is.na(plot),
						    select=c(plot2,spec,countpre))
				
				# Merge in the header rows into the main data
				colnames(main2)[colnames(main2)=="plot2"] <- "plot"
				finalpre <- merge(main2, header, by="plot")
				finalpre <- subset(finalpre, spec==sp,
							 select=-plotgual)
				
				# Add in zeros for where plot surveyed in that year but
				# species not recorded					
				final <- merge(finalpre, header, by=c("plot","year"),
						   all=T)
				final$cbc <- ty
				final$count <- ifelse(is.na(final$countpre), 0,
							    final$countpre)
				final$spec <- sp
				final$countpre <- NULL
				allyrs <- if(t==1 & y==1) final else
					rbind(allyrs, final)
				rm(daty, header, main, main2, finalpre, final)
			} # end of years loop
		} # end of types loop
	
	### Recode counts
	allyrs2 <- allyrs
	c1 <- substr(allyrs$count,1,1)
	c2 <- substr(allyrs$count,2,2)
	cnt <- as.numeric(allyrs$count)
	# If holding territory, but not counted, then set to -2
	cnt <- ifelse(c1=="X", -2, cnt)
	# If present, but not holding territory, then set to -1
	cnt <- ifelse(c1=="P", -1, cnt)
	# If nest count then make negative and subtract 2
	cnt <- ifelse(c1=="N", -2-as.numeric(sub("N","",allyrs2$count)), cnt)
	# If species quality is star then remove star and set specqual to star
	cnt <- ifelse(c1=="*",
			  ifelse(c2=="N",
			  	 -2-as.numeric(sub("*N","",allyrs2$count)),
			  	 sub("*","",allyrs2$count,fixed=T)),
			  cnt)
	allyrs2$specqual <- ifelse(c1=="*", "*", "")
	# overwrite count with the new variable cnt
	allyrs2$count <- cnt

	# Include info on county
	allyrs3 <- merge(allyrs2, catal, by="plot", all.x=T)
	allyrs4 <- merge(allyrs3, countycodes, by="county", all.x=T) 
	
	# Keep only plots of the region of interest
	if(r!="uk"){
		selfile <- paste0("z:/cbc/selfiles/",r)
		sel <- read.table(selfile, skip=2)
		colnames(sel) <- "regnum"
		allyrs5 <- merge(allyrs4, sel, by="regnum")
	} else allyrs5 <- allyrs4
	allyrs6 <- allyrs5[,c("cbc","spec","plot","year","plotgual","count",
				   "specqual","county","regnum")]
	allyrs6 <- allyrs6[order(allyrs6$county, allyrs6$plot, allyrs6$year),]
	write.csv(allyrs6, outfile, row.names=F)
	
	} # end of species loop
	
} # end of region loop

