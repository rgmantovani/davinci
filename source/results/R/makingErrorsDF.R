##########################################################################################################
##########################################################################################################

sumError = function(data){

  ALGS = c("J48", "NB", "3NN", "RF", "SVM", "MLP", "FUZZY.W", "FUZZY.CHI");

  # para cada algoritmo, 
  temp = lapply(1:length(ALGS), function(i){
    
    #percorrer a lista e contar os erros para aquele algoritmo
    aux = lapply(1:length(data), function(k){

      df = data[[k]];
      mat = matrix(0, nrow(df), 7)
      
      for(j in 1:nrow(df)){

        # se for igual, recebe 0 (white)
        if (df[j,i] == df[j,9]) {
          mat[j,1] = mat[j,1] + 1 
        }

        # se for 0, mas previu 7
        if (df[j,i] == 7 & df[j,9] == 0) { #(seagreen2)
          mat[j,2] = mat[j,2] + 1 
        }

        #se for 0, mas previu 14            
        if (df[j,i] == 14 & df[j,9] == 0) { # (springgreen4)
          mat[j,3] = mat[j,3] + 1 
        }
        
        #se for 7, mas previu 0
        if (df[j,i] == 0 & df[j,9] == 7) { # (plum3)
          mat[j,4] = mat[j,4] + 1 
        }
        
        #se for 7, mas previu 14
        if (df[j,i] == 14 & df[j,9] == 7) { # (purple4)
          mat[j,5] = mat[j,5] + 1 
        }
        
        #se for 14, mas previu 0
        if (df[j,i] == 0 & df[j,9] == 14) { # (blue1)
          mat[j,6] = mat[j,6] + 1 
        }
        
        #se for 14, mas previu 7
        if (df[j,i] == 7 & df[j,9] == 14) { #(navyblue)
          mat[j,7] = mat[j,7] + 1 
        }
      }
      return(mat)

    })

    total = Reduce("+", aux)
    colnames(total) = c("Corrrect", "0-7", "0-14", "7-0", "7-14", "14-0", "14-7")
    ret = as.data.frame(total)
    ret$classifier = ALGS[i]
    ret$id = 1:250
    return(ret)
  })

  return(temp)  
}

##########################################################################################################
##########################################################################################################

typeOfErrorsPlot = function(ret, prefix = NULL){

  full = do.call("rbind", ret)
  full = full[,-1] # omitindo os acertos
  df1 = melt(full, id.vars=c(8, 7))

  setEPS();
  filename = paste(prefix, "-algorithms-errors.eps", sep="");
  postscript(filename) #, height=10, width=6);
  
  g = NULL
  g = ggplot(df1, aes(x=id, y=value, fill=variable, group=id, width=1))
  g = g + geom_bar(position="dodge",stat="identity")
  g = g + facet_wrap(~classifier, ncol=2)
  colours = c("seagreen2", "springgreen4", "plum3", "purple4", "blue1", "navyblue"); 
  g = g + scale_fill_manual(values=colours, name ="Type of Error",
    labels=c("0 predicted as 7","0 predicted as 14", "7 predicted as 0", "7 predicted as 14",
      "14 predicted as 0", "14 predicted as 7"));
  g = g + ylab("Occurences") + xlab("Sample id");
  g = g +  theme(legend.key.size = grid::unit(0.5, "cm"))
 
  print(g);
  dev.off();

}

##########################################################################################################
##########################################################################################################