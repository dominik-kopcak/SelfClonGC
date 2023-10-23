sigma_parameters <- c(seq(0, 1, by = 0.2), 0.95, 0.99) 
replicates <- seq(1, 35, 1)

if (file.exists("parameters_all_2.txt")){
	file.remove("parameters_all_2.txt")
}


		
for (k in sigma_parameters){

	for (r in replicates){
	
		write(c(k, r), file = "parameters_all_2.txt", append = TRUE)
	
	}
}

