sigma_parameters <- c(0.80, 0.95, 0.99, 1) 
replicates <- seq(1, 15, 1)

if (file.exists("parameters_partial_2.txt")){
	file.remove("parameters_partial_2.txt")
}


		
for (k in sigma_parameters){

	for (r in replicates){
	
		write(c(k, r), file = "parameters_partial_2.txt", append = TRUE)
	
	}
}

