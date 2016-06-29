Da Vinci
=====

'*Da Vinci*' is a research project that applies Machine Learning (ML) classification algorithms to solve a food quality problem. 
The algorithms investigated are: C4.5 Decision Tree (via 'RWeka' package and through J48 implementation) (DT), Multilayer Perceptron (MLP), 
Naive Bayes (NB), k-Nearest Neighbors (k-NN), Random Forest (RF), Suppor Vector Machines (SVM) and a Fuzzy rule-based system (Fuzzy-RBS).

### Technical Requirements

R version >= 3.1.0

Required packages: 
* [CORElearn](https://cran.r-project.org/web/packages/CORElearn/index.html)
* [e1071](https://cran.r-project.org/web/packages/e1071/index.html)
* [FNN](https://cran.r-project.org/web/packages/FNN/index.html) 
* [infotheo](https://cran.r-project.org/web/packages/infotheo/index.html) 
* [kknn](https://cran.r-project.org/web/packages/kknn/index.html)
* [randomForest](https://cran.r-project.org/web/packages/randomForest/index.html) 
* [R.utils](https://cran.r-project.org/web/packages/R.utils/index.html) 
* [matrixStats](https://cran.r-project.org/web/packages/matrixStats/index.html) 
* [frbs](https://cran.r-project.org/web/packages/frbs/index.html) 
* [PMCMR](https://cran.r-project.org/web/packages/PMCMR/index.html)
* [RWeka](https://cran.r-project.org/web/packages/RWeka/index.html)

To install all of them in R, use the follow command:
```R
install.packages(c("CORElearn", "e1071", "FNN", "infotheo", "kknn", "randomForest", "R.utils", "matrixStats", 
    "frbs", "PMCMR", "RWeka"))
```

### Run the experiments

To run the experiments, clone the repository and execute it by the command:

```
./runDaVinci.sh &
```
It will call the bash file and start the execution creating an output log file called '*outDaVinci.log*'. You can follow 
the execution and errors checking directly the log file.

If you want to run experiments directly through the R script, you can call the command:

````
R CMD BATCH --no-save --no-restore mainDaVinci.R outDaVinci.log &
```

Results will be saved in a directory call '*output*', one per dataset (if exist more than one).


### Contact

Rafael Gomes Mantovani (rgmantovani@gmail.com) University of São Paulo - São Carlos, Brazil.

Sylvio Barbon Junior (sbarbonjr@gmail.com) State University of Londrina - Londrina, Brazil.


### Reference

If you use our code/experiments in your research, please, cite our paper:

Barbon, A.P.A.C.; Barbon Jr, S.; Mantovani, R.G.; Fuzyi, E.M.; Peres, L.M.; Bridi, A.M. 
**Storage time prediction of pork by Computational Intelligence**. 
In: *Computers and Electronics in Agriculture (under revision)* (2016). vol X. number X. p 1 - 18. 
