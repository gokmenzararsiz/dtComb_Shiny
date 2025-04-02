FROM rocker/shiny

# Örnek uygulamaları sil
RUN rm -rf /srv/shiny-server/*

# Uygulama dosyalarını ekle
WORKDIR /srv/shiny-server
COPY . /srv/shiny-server/
RUN chown -R shiny:shiny /srv/shiny-server

# Sistem bağımlılıkları
RUN apt-get update && apt-get upgrade -y
RUN apt-get install -y python3-pip
RUN pip3 install igraph
RUN apt-get install -y libglpk-dev libtk-img libgdal-dev libgda-5.0-4 libproj-dev libudunits2-dev tcl tk tcl-dev tk-dev libglu1 openjdk-11-jdk

# R paketleri (devasa ama eksiksiz 👏)
RUN R -e "install.packages(c('devtools','rgl','glmnet','gam','fastAdaboost','doParallel','adabag','plotrix','Formula','plotmo','TeachingDemos','earth','mda','rprojroot','diffobj','rematch2','brio','callr','desc','pkgload','praise','processx','ps','waldo','testthat','minqa','nloptr','lme4','abind','coda','ada','mvtnorm','stabs','nnls','quadprog','import','libcoin','inum','partykit','mboost','bitops','caTools','rJava','RWekajars','RWeka','party','coin','strucchange','multcomp','sandwich','modeltools','matrixStats','TH.data','kerndwd','xgboost','RSpectra','rARPACK','HDclassif','kknn','klaR','questionr','labelled','styler','haven','R.cache','readr','R.utils','vroom','R.oo','bit64','combinat','rstudioapi','miniUI','forcats','R.methodsS3','clipr','bit','optimx','monmlp','RSNNS','ncvreg','msaenet','naivebayes','pamr','randomForest','pls','dotCall64','gridExtra','backports','statnet.common','maps','SparseM','MatrixModels','permute','carData','network','spam','viridis','broom','vegan','quantreg','sna','fields','pbkrtest','bipartite','car','plsRglm','stepPlr','ordinalNet','RRF','LiblineaR','DEoptimR','pcaPP','robustbase','rrcov','truncnorm','mclust','Rsolnp','robustDA','rotationForest','kohonen','entropy','corpcor','fdrtool','sda','sdwd','lars','elasticnet','sparseLDA','spls','deepnet','gbm','evtree','wsrf','readxl','stringr','dtComb','ggplot2','dplyr','DT','shinyBS','plyr','pROC','epiR','OptimalCutpoints','ROCR','pls','mda','shinyalert','combinat','rapport','arm','Cubist','C50','kernlab','misc3d','plot3D','prim','supervisedPRIM','munsell'))"

# Bioconductor ve özel paketler
RUN R -e "install.packages('BiocManager', repos = 'https://cloud.r-project.org')"
RUN R -e "BiocManager::install(version = '3.16')"
RUN R -e "BiocManager::install('gpls')"

# CRAN dışı paketler
RUN R -e "install.packages('./no_cran_packages/obliqueRF_0.3.tar.gz', repos = NULL, type = 'source')"
RUN R -e "install.packages('./no_cran_packages/nodeHarvest_0.7-3.tar.gz', repos = NULL, type = 'source')"
RUN R -e "install.packages('./no_cran_packages/FCNN4R_0.6.2.tar.gz', repos = NULL, type = 'source')"
RUN R -e "install.packages('./no_cran_packages/extraTrees_1.0.5.tar.gz', repos = NULL, type = 'source')"
RUN R -e "install.packages('./no_cran_packages/kohonen_3.0.12.tar.gz', repos = NULL, type = 'source')"

EXPOSE 3838
