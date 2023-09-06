s_parameters <- c(-0.01)
h_parameters <- c(0.2, 0.5)
sigma_parameters <- c(seq(0, 1, by = 0.2), 0.95, 0.99) 
replicates <- seq(1, 100, 1)

if (file.exists("parameters_all_1.txt")){
	file.remove("parameters_all_1.txt")
}

for (i in s_parameters){

	for (j in h_parameters){
		
		for (k in sigma_parameters){
		
			for (r in replicates){
			
				write(c(i, j, k, r), file = "parameters_all_1.txt", append = TRUE)
			
			}
		}
	}
}
