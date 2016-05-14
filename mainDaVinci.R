##########################################################################################################
# R Code
# Base experiment 
# R. G.  Mantovani, 2015-2016
# Start the experiment 
##########################################################################################################

setup = function() {

  files = list.files(path = "R", recursive=TRUE, pattern = "*.R$", full.name = TRUE)	
  for(i in files){
    source(i)
  }
}


##########################################################################################################
##########################################################################################################

# Running experiment 
main = function() {

  # loading files
  setup()	

  # create output dir
  subdirs = c("output/")
  for(subd in subdirs ){
    if(!dir.exists(subd)) {
      cat(" - Creating:", subd, "directory.\n")
      dir.create(subd)
    }
  }
		
  data.files = list.files(path = "data/all/")
  #data.files = data.files[1:2] #just for tests

  cat(" ---------------------------------- \n")

  n = length(data.files)
  ret = lapply(1:n, function(k) {

    file = data.files[k]
    filename =  gsub(x = file, pattern = ".arff", replacement = "")
    cat(k, "/", n, " - @File:",filename, "\n")

    if(!dir.exists(paste0("output/", filename))) {
      dir.create(paste0("output/", filename))	
    }

    data = RWeka::read.arff(paste0("data/all/", file))
    ret  = root(data = data, filename = filename)
    cat(" ---------------------------------- \n")
    return(ret$df)
  })

  final = do.call("rbind", ret)
  save(final, file = "output/summary_results.RData")
  write.csv(final, file = "output/summary_results.csv")
  cat(" Done !!! \n")

}

##########################################################################################################
##########################################################################################################

main()

##########################################################################################################
##########################################################################################################
