#!/usr/bin/env Rscript
setwd("./Simulation_output")

library(tidyverse)
library(scales)

LD_list <- list.files(pattern="*.vcf.hap.ld")

maxd <- 12.5 		# Maximum distance to use (in Mb)
stopifnot(maxd<=12.5)
min_ee <- 5		# Minimum number of bin entries to be included in plot
max_ee <- 100

rates <- c("1e-05", "1e-06", "1e-07", "1e-08", "1e-09", "1e-10", "1e-11")

for (rate in rates){

	LD_list_GC_subset <- LD_list[grepl(rate, LD_list)]

	print(length(LD_list_GC_subset))

	for(i in 1:10){
		dat <- read.csv(LD_list_GC_subset[i], header=TRUE, sep="\t")
		dat <- dat %>% mutate(DIST=POS2-POS1)
		dat <- dat %>% mutate(LEVEL=cut(dat$DIST,seq(0,25e6,50e4),right=F))
		
		# Can we count how many LD measurements there are? Can we normalise by smallest value?
		# First check what the smallest bin size is within desired range
		
		dat <- dat %>% filter(DIST<=maxd*1e6)
		nbins <- length(unique(dat$LEVEL))
		dim_s <- vector(mode="numeric",length=nbins)
		for(j in 1:nbins){
			dim_s[j] <- dim(subset(dat,LEVEL==unique(dat$LEVEL)[j]))[1]
		}
		
		stopifnot(dim_s[1]>0)	# sanity check - stop if no entries in smallest bin
		min_e <- if_else(min(dim_s[which(dim_s!=0)]) > max_ee, max_ee, min(dim_s[which(dim_s!=0)]))

		if(min_e >= min_ee)
		{
			# Then subsampling entries, first checking they're not zero
			dat2 <- sample_n(subset(dat,LEVEL==unique(dat$LEVEL)[1]),min_e,replace=F)
			for(j in 2:nbins){
				if(dim_s[j] != 0){
					dat2 <- rbind(dat2,sample_n(subset(dat,LEVEL==unique(dat$LEVEL)[j]),min_e,replace=F))
				}
			}
		
			dat2 <- cbind(dat2,i)
			names(dat2)[10] <- c("Rep")
			
		
			if(exists("mainres"))
			{
				mainres <- rbind(mainres,dat2)
			}else{
				mainres <- dat2
			}
		
		}
		
	}

	mainres <- mainres %>% mutate(DIST_MB=DIST/1e6)
	mainres$Rep <- as.factor(mainres$Rep)

	# Plotting LD decay (r^2 and Dprime)

	r2_plot <- ggplot(mainres, aes(x=DIST_MB, y=R.2)) + 
		geom_point(aes(color=Rep),alpha=0.1) + 
		geom_smooth(aes(color=Rep)) + 
		geom_smooth(col='black',size=2,linetype="dashed", se=FALSE) + 
	    scale_color_brewer(palette = "RdBu") +     
	    labs(x="Distance (Mb)",y=expression(paste("Mean LD (",r^2,")"))) +  
		xlim(0,maxd) + 
		ylim(0,1) + 
		#scale_x_continuous(labels=comma) + 
		theme_bw(base_size=36) + 
		theme(plot.title=element_text(hjust=0.5)) + 
		theme(legend.position="none")
		
	Dprime_plot <- ggplot(mainres,aes(x=DIST_MB,y=abs(Dprime))) + 
		geom_point(aes(color=Rep),alpha=0.1) + 
		geom_smooth(aes(color=Rep)) + 
		geom_smooth(col='black',size=2,linetype="dashed", se=FALSE) + 
	    scale_color_brewer(palette = "RdBu") + 
		labs(x="Distance (Mb)",y="Mean LD (D', absolute value)") +  
		xlim(0,maxd) + 
		ylim(0,1) + 
		theme_bw(base_size=20) + 
		theme(plot.title=element_text(hjust=0.5)) + 
		theme(legend.position="none")
		
	setwd("./..")
	ggsave(filename = paste("LD_r2_nonconstant_sh_GC_", gsub(rate, "_", "") ,".png", sep=""),
		plot = r2_plot,
		dpi = 600,
		width=12,
		height=12)
		
	ggsave(filename = paste("LD_Dprime_nonconstant_sh_GC_", gsub(rate, "_", ""),".png", sep=""),
		plot = Dprime_plot,
		dpi = 600,
		width=12,
		height=12)
		
	write.csv(mainres, file=paste("Data_LD__nonconstant_sh_GC_", gsub(rate, "_", ""),".csv", sep=""))
	setwd("./Simulation_output")

	rm(mainres)

	print(warnings())
}

