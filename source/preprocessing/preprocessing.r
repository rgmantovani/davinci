# R Code
# preprocessing File 
# L. P. Garcia, R. G. Mantovani, A. C.P.L.F. Carvalho 2013
# Dataset Preprocessing

#Params:
#	- subdir: subdiretorio das bases que serao preprocessadas
#				podendo receber "orig/", "d1/", "d2/"

preprocessing = function(subdir) {

	#obtem todos os arquivos de bases de dados
	FILES = list.files(paste(getwd(), "/database/", subdir, sep=""))


	for(file in FILES) {
		cat(file,"\n")

		data = read.arff(paste(getwd(), "/database/", subdir, file, sep=""));

		#exclui as bases com menos de 60 exemplos
		if(nrow(data) < 60){
			system(paste("rm ",getwd(), "/database/", subdir, file, sep=""))
		}
		else
		{

			#renomei os atributos, definindo a ultima coluna como "Class"
			colnames(data) = c(paste("V", 1:(ncol(data)-1), sep=""), "Class");
			rownames(data) = NULL;

			#binarização da classe
			aux = length(unique(data$Class));
			if(aux > 2)
				data[which(data$Class != 0),]$Class = 1;

			#Classe como zero e um
			data$Class = factor(data$Class, labels=c(0:1));

			for(i in colnames(data)) {

				#renomear os nivels dos atributos categoricos para numeros
				if(!is.numeric(data[,i])) {

					data[,i] = factor(data[,i]);
					levels(data[,i]) = factor(1:length(levels(data[,i])));
					aux = nlevels(data[,i]);

					if(aux == 1 | aux == nrow(data))
						data[,i] = NULL;
				} 
				else 
				{
					#remove atributos categoricos com um unico nivel
					aux = length(unique(data[,i]))
					if(aux == 1)
						data[,i] = NULL;		
				
					#verificar os atributos binarios e os converter para categoricos
					if(length(unique(data[,i]))==2){
						data[,i] = factor(data[,i], labels=c(0,1))
					}
				}
			}

			#Classe como zero e um
			data$Class = factor(data$Class, labels=c(0:1));

			write.arff(data, paste(getwd(), "/database/", subdir, file, sep=""));
		}
	}
}

