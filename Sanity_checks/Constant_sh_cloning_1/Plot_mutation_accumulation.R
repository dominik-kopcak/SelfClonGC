library(ggplot2)
library(patchwork)
library(dplyr)

replicate_number=100
colours <- c("Cloning"="red","Cloning+GC_1E-09"="lightblue","Cloning+GC_1E-10"="lightblue","Cloning+GC_1E-11"="lightblue")

Cloning_GC_recessive_load <- read.csv("/home/dvojkvietok/Documents/SlefingCloningGC/2_Cloning_with_gene_conversion/2_constant_sh/lambda_100/test_small_region/v3/Data_GC_recessive_load_test_v3.csv")
Cloning_GC_additive_load <- read.csv("/home/dvojkvietok/Documents/SlefingCloningGC/2_Cloning_with_gene_conversion/2_constant_sh/lambda_100/test_small_region/v3/Data_GC_additive_load_test_v3.csv")

Cloning_GC_recessive_load_09 <- Cloning_GC_recessive_load %>%
	filter(GC_rate == "1e-09")
Cloning_GC_recessive_load_09$GC_rate <- as.factor(Cloning_GC_recessive_load_09$GC_rate)
Cloning_GC_additive_load_09 <- Cloning_GC_additive_load %>%
	filter(GC_rate == "1e-09")
Cloning_GC_additive_load_09$GC_rate <- as.factor(Cloning_GC_additive_load_09$GC_rate)

Cloning_GC_recessive_load_10 <- Cloning_GC_recessive_load %>%
	filter(GC_rate == "1e-10")
Cloning_GC_recessive_load_10$GC_rate <- as.factor(Cloning_GC_recessive_load_10$GC_rate)
Cloning_GC_additive_load_10 <- Cloning_GC_additive_load %>%
	filter(GC_rate == "1e-10")
Cloning_GC_additive_load_10$GC_rate <- as.factor(Cloning_GC_additive_load_10$GC_rate)

Cloning_GC_recessive_load_11 <- Cloning_GC_recessive_load %>%
	filter(GC_rate == "1e-11")
Cloning_GC_recessive_load_11$GC_rate <- as.factor(Cloning_GC_recessive_load_11$GC_rate)
Cloning_GC_additive_load_11 <- Cloning_GC_additive_load %>%
	filter(GC_rate == "1e-11")
Cloning_GC_additive_load_11$GC_rate <- as.factor(Cloning_GC_additive_load_11$GC_rate)


Cloning_recessive_load <- read.csv("/home/dvojkvietok/Documents/SlefingCloningGC/1_Selfing-cloning_comparison/1_constant_sh/full_version_20/Data_cloning_recessive_load.csv")
Cloning_additive_load <- read.csv("/home/dvojkvietok/Documents/SlefingCloningGC/1_Selfing-cloning_comparison/1_constant_sh/full_version_20/Data_cloning_additive_load.csv")

Cloning_recessive_load_all <- Cloning_recessive_load %>%
	filter(Uniparenting_rate > 0.9)
	
Cloning_additive_load_all <- Cloning_additive_load %>%
	filter(Uniparenting_rate > 0.9)


h_coefficient = 0.2

filename_rec_cloning_GC_09 <- Cloning_GC_recessive_load_09 %>% filter(Dominance_coefficient == h_coefficient)
filename_add_cloning_GC_09 <- Cloning_GC_additive_load_09 %>% filter(Dominance_coefficient == h_coefficient)

filename_rec_cloning_GC_10 <- Cloning_GC_recessive_load_10 %>% filter(Dominance_coefficient == h_coefficient)
filename_add_cloning_GC_10 <- Cloning_GC_additive_load_10 %>% filter(Dominance_coefficient == h_coefficient)

filename_rec_cloning_GC_11 <- Cloning_GC_recessive_load_11 %>% filter(Dominance_coefficient == h_coefficient)
filename_add_cloning_GC_11 <- Cloning_GC_additive_load_11 %>% filter(Dominance_coefficient == h_coefficient)


Cloning_GC_09_rec_max <- filename_rec_cloning_GC_09$Expected_recessive_load+1.96*filename_rec_cloning_GC_09$SD_recessive_load/sqrt(replicate_number)
Cloning_GC_09_rec_min <- filename_rec_cloning_GC_09$Expected_recessive_load-1.96*filename_rec_cloning_GC_09$SD_recessive_load/sqrt(replicate_number)

Cloning_GC_09_add_max <- filename_add_cloning_GC_09$Expected_additive_load+1.96*filename_add_cloning_GC_09$SD_additive_load/sqrt(replicate_number)
Cloning_GC_09_add_min <- filename_add_cloning_GC_09$Expected_additive_load-1.96*filename_add_cloning_GC_09$SD_additive_load/sqrt(replicate_number)



Cloning_GC_10_rec_max <- filename_rec_cloning_GC_10$Expected_recessive_load+1.96*filename_rec_cloning_GC_10$SD_recessive_load/sqrt(replicate_number)
Cloning_GC_10_rec_min <- filename_rec_cloning_GC_10$Expected_recessive_load-1.96*filename_rec_cloning_GC_10$SD_recessive_load/sqrt(replicate_number)

Cloning_GC_10_add_max <- filename_add_cloning_GC_10$Expected_additive_load+1.96*filename_add_cloning_GC_10$SD_additive_load/sqrt(replicate_number)
Cloning_GC_10_add_min <- filename_add_cloning_GC_10$Expected_additive_load-1.96*filename_add_cloning_GC_10$SD_additive_load/sqrt(replicate_number)



Cloning_GC_11_rec_max <- filename_rec_cloning_GC_11$Expected_recessive_load+1.96*filename_rec_cloning_GC_11$SD_recessive_load/sqrt(replicate_number)
Cloning_GC_11_rec_min <- filename_rec_cloning_GC_11$Expected_recessive_load-1.96*filename_rec_cloning_GC_11$SD_recessive_load/sqrt(replicate_number)

Cloning_GC_11_add_max <- filename_add_cloning_GC_11$Expected_additive_load+1.96*filename_add_cloning_GC_11$SD_additive_load/sqrt(replicate_number)
Cloning_GC_11_add_min <- filename_add_cloning_GC_11$Expected_additive_load-1.96*filename_add_cloning_GC_11$SD_additive_load/sqrt(replicate_number)



filename_rec_cloning <- Cloning_recessive_load_all %>% filter(Dominance_coefficient == h_coefficient)
filename_rec_cloning$Uniparenting_rate <- as.factor(filename_rec_cloning$Uniparenting_rate)
filename_add_cloning <- Cloning_additive_load_all %>% filter(Dominance_coefficient == h_coefficient)
filename_add_cloning$Uniparenting_rate <- as.factor(filename_add_cloning$Uniparenting_rate)

Cloning_rec_max <- filename_rec_cloning$Expected_recessive_load+1.96*filename_rec_cloning$SD_recessive_load/sqrt(replicate_number)
Cloning_rec_min <- filename_rec_cloning$Expected_recessive_load-1.96*filename_rec_cloning$SD_recessive_load/sqrt(replicate_number)

Cloning_add_max <- filename_add_cloning$Expected_additive_load+1.96*filename_add_cloning$SD_additive_load/sqrt(replicate_number)
Cloning_add_min <- filename_add_cloning$Expected_additive_load-1.96*filename_add_cloning$SD_additive_load/sqrt(replicate_number)

print(Cloning_add_max)
print(Cloning_add_min)

Plot_recessive <- ggplot()+
	geom_point(data = filename_rec_cloning_GC_09, aes(x = GC_rate, y = Expected_recessive_load), color = "lightblue")+
	geom_linerange(data = filename_rec_cloning_GC_09,
			aes(x=GC_rate, 
			    ymax=Cloning_GC_09_rec_max, 
			    ymin=Cloning_GC_09_rec_max), color="lightblue", size=1)+
			    
	geom_point(data = filename_rec_cloning_GC_10, aes(x = GC_rate, y = Expected_recessive_load), color = "lightblue")+
	geom_linerange(data = filename_rec_cloning_GC_10, 
			aes(x=GC_rate, 
			    ymax=Cloning_GC_10_rec_max, 
			    ymin=Cloning_GC_10_rec_max), color="lightblue", size=1)+
	
	geom_point(data = filename_rec_cloning_GC_11, aes(x = GC_rate, y = Expected_recessive_load), color = "lightblue")+
	geom_linerange(data = filename_rec_cloning_GC_11, 
			aes(x=GC_rate, 
			    ymax=Cloning_GC_11_rec_max, 
			    ymin=Cloning_GC_11_rec_max), color="lightblue", size=1)+ 
 
 
 
 	geom_point(data=filename_rec_cloning, aes(x=Uniparenting_rate, y=Expected_recessive_load), color="red")+
	geom_linerange(data=filename_rec_cloning, 
			aes(x=Uniparenting_rate,
			    ymax=Cloning_rec_max, 
			    ymin=Cloning_rec_min), color="red", size=1)+
	
	labs(y="Homozygous genotypes",
	     x="Rate of uniparental reproduction",
	     title="Accumulation of mutations\nin populations with uniparental reproduction")+
	 theme(panel.grid.major=element_blank(),
    	      panel.grid.minor=element_blank(),
    	      panel.background=element_blank(),
    	      axis.line = element_line(color="black"),
    	      
    	      plot.title=element_text(size=20, hjust=0.5, face="bold"),
	      plot.subtitle=element_text(size=15, hjust=1, face="bold"),
	      axis.title.y=element_text(size=15),
	      axis.title.x=element_blank(),
	      
	      axis.text.x=element_text(size=15),
	      axis.text.y=element_text(size=15),
	      
	      legend.position="none")+
	scale_colour_manual(values = colours)
#	scale_x_continuous(breaks=seq(0, 1, 0.2))
		   
Plot_additive <- ggplot()+
			    
	geom_point(data = filename_add_cloning_GC_09, aes(x = GC_rate, y = Expected_additive_load/1000), color = "lightblue")+
	geom_linerange(data = filename_add_cloning_GC_09,
			aes(x=GC_rate, 
			    ymax=Cloning_GC_09_add_max/1000, 
			    ymin=Cloning_GC_09_add_max/1000, color="Cloning+GC_1E-09"), size=1)+
			    
	geom_point(data = filename_add_cloning_GC_10, aes(x = GC_rate, y = Expected_additive_load/1000), color = "lightblue")+
	geom_linerange(data = filename_add_cloning_GC_10, 
			aes(x=GC_rate, 
			    ymax=Cloning_GC_10_add_max/1000, 
			    ymin=Cloning_GC_10_add_max/1000, color="Cloning+GC_1E-10"), size=1)+
	
	geom_point(data = filename_add_cloning_GC_11, aes(x = GC_rate, y = Expected_additive_load/1000), color = "lightblue")+
	geom_linerange(data = filename_add_cloning_GC_11, 
			aes(x=GC_rate, 
			    ymax=Cloning_GC_11_add_max/1000, 
			    ymin=Cloning_GC_11_add_max/1000, color="Cloning+GC_1E-11"), size=1)+ 
			    

	geom_point(data=filename_add_cloning, aes(x=Uniparenting_rate, y=Expected_additive_load/1000), colour="red")+
	geom_linerange(data=filename_add_cloning, 
			aes(x=Uniparenting_rate, 
			    ymax=Cloning_add_max/1000, 
			    ymin=Cloning_add_min/1000, color="Cloning"), size=1)+
	
	labs(x="Rate of uniparental reproduction",
	     y="Total mutation count\n(x 1000)",
	     caption=paste("s = 0.01, h = ", h_coefficient, ", mutation rate = 4E-9, population size = 5 000\ngenome is 1 chromosome with size 25 Mbp, recombination rate = 4E-8\nnumber of replicates = 100, sample size = 50, run for 20 000 generations", sep=""),
	     colour="Reproduction: ")+
	theme(panel.grid.major=element_blank(),
    	      panel.grid.minor=element_blank(),
    	      panel.background=element_blank(),
    	      axis.line = element_line(color="black"),
    	      
	      plot.caption=element_text(size=10, hjust=0.5),
	      plot.subtitle=element_text(size=15, hjust=1, face="bold"), 
	      axis.title.y=element_text(size=15),
	      axis.title.x=element_text(size=15),
	      
	      axis.text.x=element_text(size=15),
	      axis.text.y=element_text(size=15),
	      
	      legend.position="right",
	      legend.background=element_rect(colour="black"))+
	      
	scale_colour_manual(values = colours)
#	scale_x_continuous(breaks=seq(0, 1, 0.2))

Plot <- Plot_recessive/Plot_additive

plot_name <- paste("Figure_Mutation_accumulation_cloning_vs_GC_", h_coefficient, "_test_v3.png",sep="")
ggsave(filename=plot_name,
	plot=Plot,
	dpi=600)






h_coefficient = 0.5


filename_rec_cloning_GC_09 <- Cloning_GC_recessive_load_09 %>% filter(Dominance_coefficient == h_coefficient)
filename_add_cloning_GC_09 <- Cloning_GC_additive_load_09 %>% filter(Dominance_coefficient == h_coefficient)

filename_rec_cloning_GC_10 <- Cloning_GC_recessive_load_10 %>% filter(Dominance_coefficient == h_coefficient)
filename_add_cloning_GC_10 <- Cloning_GC_additive_load_10 %>% filter(Dominance_coefficient == h_coefficient)

filename_rec_cloning_GC_11 <- Cloning_GC_recessive_load_11 %>% filter(Dominance_coefficient == h_coefficient)
filename_add_cloning_GC_11 <- Cloning_GC_additive_load_11 %>% filter(Dominance_coefficient == h_coefficient)

Cloning_GC_09_rec_max <- filename_rec_cloning_GC_09$Expected_recessive_load+1.96*filename_rec_cloning_GC_09$SD_recessive_load/sqrt(replicate_number)
Cloning_GC_09_rec_min <- filename_rec_cloning_GC_09$Expected_recessive_load-1.96*filename_rec_cloning_GC_09$SD_recessive_load/sqrt(replicate_number)

Cloning_GC_09_add_max <- filename_add_cloning_GC_09$Expected_additive_load+1.96*filename_add_cloning_GC_09$SD_additive_load/sqrt(replicate_number)
Cloning_GC_09_add_min <- filename_add_cloning_GC_09$Expected_additive_load-1.96*filename_add_cloning_GC_09$SD_additive_load/sqrt(replicate_number)



Cloning_GC_10_rec_max <- filename_rec_cloning_GC_10$Expected_recessive_load+1.96*filename_rec_cloning_GC_10$SD_recessive_load/sqrt(replicate_number)
Cloning_GC_10_rec_min <- filename_rec_cloning_GC_10$Expected_recessive_load-1.96*filename_rec_cloning_GC_10$SD_recessive_load/sqrt(replicate_number)

Cloning_GC_10_add_max <- filename_add_cloning_GC_10$Expected_additive_load+1.96*filename_add_cloning_GC_10$SD_additive_load/sqrt(replicate_number)
Cloning_GC_10_add_min <- filename_add_cloning_GC_10$Expected_additive_load-1.96*filename_add_cloning_GC_10$SD_additive_load/sqrt(replicate_number)



Cloning_GC_11_rec_max <- filename_rec_cloning_GC_11$Expected_recessive_load+1.96*filename_rec_cloning_GC_11$SD_recessive_load/sqrt(replicate_number)
Cloning_GC_11_rec_min <- filename_rec_cloning_GC_11$Expected_recessive_load-1.96*filename_rec_cloning_GC_11$SD_recessive_load/sqrt(replicate_number)

Cloning_GC_11_add_max <- filename_add_cloning_GC_11$Expected_additive_load+1.96*filename_add_cloning_GC_11$SD_additive_load/sqrt(replicate_number)
Cloning_GC_11_add_min <- filename_add_cloning_GC_11$Expected_additive_load-1.96*filename_add_cloning_GC_11$SD_additive_load/sqrt(replicate_number)



filename_rec_cloning <- Cloning_recessive_load_all %>% filter(Dominance_coefficient == h_coefficient)
filename_rec_cloning$Uniparenting_rate <- as.factor(filename_rec_cloning$Uniparenting_rate)
filename_add_cloning <- Cloning_additive_load_all %>% filter(Dominance_coefficient == h_coefficient)
filename_add_cloning$Uniparenting_rate <- as.factor(filename_add_cloning$Uniparenting_rate)

Cloning_rec_max <- filename_rec_cloning$Expected_recessive_load+1.96*filename_rec_cloning$SD_recessive_load/sqrt(replicate_number)
Cloning_rec_min <- filename_rec_cloning$Expected_recessive_load-1.96*filename_rec_cloning$SD_recessive_load/sqrt(replicate_number)

Cloning_add_max <- filename_add_cloning$Expected_additive_load+1.96*filename_add_cloning$SD_additive_load/sqrt(replicate_number)
Cloning_add_min <- filename_add_cloning$Expected_additive_load-1.96*filename_add_cloning$SD_additive_load/sqrt(replicate_number)

print(Cloning_add_max)
print(Cloning_add_min)

Plot_recessive <- ggplot()+
	geom_point(data = filename_rec_cloning_GC_09, aes(x = GC_rate, y = Expected_recessive_load), color = "lightblue")+
	geom_linerange(data = filename_rec_cloning_GC_09,
			aes(x=GC_rate, 
			    ymax=Cloning_GC_09_rec_max, 
			    ymin=Cloning_GC_09_rec_max), color="lightblue", size=1)+
			    
	geom_point(data = filename_rec_cloning_GC_10, aes(x = GC_rate, y = Expected_recessive_load), color = "lightblue")+
	geom_linerange(data = filename_rec_cloning_GC_10, 
			aes(x=GC_rate, 
			    ymax=Cloning_GC_10_rec_max, 
			    ymin=Cloning_GC_10_rec_max), color="lightblue", size=1)+
	
	geom_point(data = filename_rec_cloning_GC_11, aes(x = GC_rate, y = Expected_recessive_load), color = "lightblue")+
	geom_linerange(data = filename_rec_cloning_GC_11, 
			aes(x=GC_rate, 
			    ymax=Cloning_GC_11_rec_max, 
			    ymin=Cloning_GC_11_rec_max), color="lightblue", size=1)+ 
 
 
 
 	geom_point(data=filename_rec_cloning, aes(x=Uniparenting_rate, y=Expected_recessive_load), color="red")+
	geom_linerange(data=filename_rec_cloning, 
			aes(x=Uniparenting_rate,
			    ymax=Cloning_rec_max, 
			    ymin=Cloning_rec_min), color="red", size=1)+
	
	labs(y="Homozygous genotypes",
	     x="Rate of uniparental reproduction",
	     title="Accumulation of mutations\nin populations with uniparental reproduction")+
	 theme(panel.grid.major=element_blank(),
    	      panel.grid.minor=element_blank(),
    	      panel.background=element_blank(),
    	      axis.line = element_line(color="black"),
    	      
    	      plot.title=element_text(size=20, hjust=0.5, face="bold"),
	      plot.subtitle=element_text(size=15, hjust=1, face="bold"),
	      axis.title.y=element_text(size=15),
	      axis.title.x=element_blank(),
	      
	      axis.text.x=element_text(size=15),
	      axis.text.y=element_text(size=15),
	      
	      legend.position="none")+
	scale_colour_manual(values = colours)
#	scale_x_continuous(breaks=seq(0, 1, 0.2))
		   
Plot_additive <- ggplot()+
			    
	geom_point(data = filename_add_cloning_GC_09, aes(x = GC_rate, y = Expected_additive_load/1000), color = "lightblue")+
	geom_linerange(data = filename_add_cloning_GC_09,
			aes(x=GC_rate, 
			    ymax=Cloning_GC_09_add_max/1000, 
			    ymin=Cloning_GC_09_add_max/1000, color="Cloning+GC_1E-09"), size=1)+
			    
	geom_point(data = filename_add_cloning_GC_10, aes(x = GC_rate, y = Expected_additive_load/1000), color = "lightblue")+
	geom_linerange(data = filename_add_cloning_GC_10, 
			aes(x=GC_rate, 
			    ymax=Cloning_GC_10_add_max/1000, 
			    ymin=Cloning_GC_10_add_max/1000, color="Cloning+GC_1E-10"), size=1)+
	
	geom_point(data = filename_add_cloning_GC_11, aes(x = GC_rate, y = Expected_additive_load/1000), color = "lightblue")+
	geom_linerange(data = filename_add_cloning_GC_11, 
			aes(x=GC_rate, 
			    ymax=Cloning_GC_11_add_max/1000, 
			    ymin=Cloning_GC_11_add_max/1000, color="Cloning+GC_1E-11"), size=1)+ 
			    

	geom_point(data=filename_add_cloning, aes(x=Uniparenting_rate, y=Expected_additive_load/1000), colour="red")+
	geom_linerange(data=filename_add_cloning, 
			aes(x=Uniparenting_rate, 
			    ymax=Cloning_add_max/1000, 
			    ymin=Cloning_add_min/1000, color="Cloning"), size=1)+
	
	labs(x="Rate of uniparental reproduction",
	     y="Total mutation count\n(x 1000)",
	     caption=paste("s = 0.01, h = ", h_coefficient, ", mutation rate = 4E-9, population size = 5 000\ngenome is 1 chromosome with size 25 Mbp, recombination rate = 4E-8\nnumber of replicates = 100, sample size = 50, run for 20 000 generations", sep=""),
	     colour="Reproduction: ")+
	theme(panel.grid.major=element_blank(),
    	      panel.grid.minor=element_blank(),
    	      panel.background=element_blank(),
    	      axis.line = element_line(color="black"),
    	      
	      plot.caption=element_text(size=10, hjust=0.5),
	      plot.subtitle=element_text(size=15, hjust=1, face="bold"), 
	      axis.title.y=element_text(size=15),
	      axis.title.x=element_text(size=15),
	      
	      axis.text.x=element_text(size=15),
	      axis.text.y=element_text(size=15),
	      
	      legend.position="right",
	      legend.background=element_rect(colour="black"))+
	      
	scale_colour_manual(values = colours)
#	scale_x_continuous(breaks=seq(0, 1, 0.2))

Plot <- Plot_recessive/Plot_additive

plot_name <- paste("Figure_Mutation_accumulation_cloning_vs_GC_", h_coefficient, "_test_v3.png",sep="")
ggsave(filename=plot_name,
	plot=Plot,
	dpi=600)
