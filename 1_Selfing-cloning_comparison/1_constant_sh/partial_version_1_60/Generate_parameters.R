s_parameters <- c(-0.01)
h_parameters <- c(0.2)
sigma_parameters <- c(0.95, 0.99, 1) 
replicates <- seq(1, 20, 1)

if (file.exists("parameters_partial_1.txt")){
	file.remove("parameters_partial_1.txt")
}

for (i in s_parameters){

	for (j in h_parameters){
		
		for (k in sigma_parameters){
		
			for (r in replicates){
			
				write(c(i, j, k, r), file = "parameters_partial_1.txt", append = TRUE)
			
			}
		}
	}
}
