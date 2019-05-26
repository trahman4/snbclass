#setwd("C:/Users/Lam/Desktop/Projects/Hsin_en_classification/Final_simulation_040119/Simulation2_04032019/Disp0.5eff0.25")
library(flux)
loc<-getwd()
param_set<-strsplit(loc,'/')[[1]][grep('Disp',strsplit(loc,'/')[[1]])]

list_result<-list()


prop_folders<-dir()[grep('prop',dir())]




for(prop_ind in 1:length(prop_folders)){
  start<-Sys.time()
  
  setwd(paste(loc,prop_folders[prop_ind],sep='/'))
  
  
  prop<-as.numeric(strsplit(prop_folders[prop_ind],'_')[[1]][2])
  
  ave_splda_mse<-ave_glm_mse<-ave_glmsc_mse<-proportion<-cov_effect<-c()
  
  
  cov_effect<-c()
  pattern_files<-dir()[grep('simu',dir())]
  pattern_files_auc<-pattern_files[grep('_auc',pattern_files)]
  cov_pattern_auc<-unique(matrix(unlist(strsplit(pattern_files_auc,'cov')),ncol=2,byrow = T)[,2])
  
  for(cov_ind in 1:length(cov_pattern_auc)){
    eta<-as.numeric(strsplit(strsplit(cov_pattern_auc[cov_ind],'.RData')[[1]][1],'_auc')[[1]][1])
    mse_glmsc<-mse_glm<-auc_glmc<-mse_splda<-c()
    files<-pattern_files_auc[grep(paste0('cov',cov_pattern_auc[cov_ind]),pattern_files_auc)]
    for(file_ind in 1:length(files)){
      load(files[file_ind])
      
      
      load_file<-files[file_ind]
      load(paste0(strsplit(load_file,'_auc')[[1]][1],'.RData'))
           
      ###glmsc
      
      B_true<-final_result_auc$sim_data$B_true
      
      mse<-unlist(lapply(final_result_auc$result_glmsc,function(x) sum((x$model$beta-B_true)^2)))
      lambda_comb<-do.call(rbind.data.frame,lapply(final_result_auc$result_glmsc,function(x) x$lambda))
      colnames(lambda_comb)<-c('lambda1','lambda2')
      mse_result_glmsc<-data.frame(lambda_comb,mse)
      mse_glmsc[file_ind]<-min(mse)
      #glm
      
      mse_glm[file_ind]<- min(unlist(lapply(final_result_auc$result_glm,function(x) sum((x$model$inibet-B_true)^2))))
      
      #splda
      g_bar<-matrix(rep(apply(final_result_auc$sim_data$train_data$dat,2,mean),3),byrow = T,ncol=1000)
      beta_plda<-lapply(final_result_auc$result_plda,function(x) log(x$ds*g_bar))
      mse_splda[file_ind]<-min(unlist(lapply(beta_plda,function(x) sum((x-B_true)^2))))
      
      
      
       end<-Sys.time()
      print(length(files)-file_ind)
      print(end-start)
    }
    
  
    ave_glm_mse[cov_ind]<-sqrt(sum(mse_glm)/(3000*100))
    ave_splda_mse[cov_ind]<-sqrt(sum(mse_splda)/(3000*100))
    ave_glmsc_mse[cov_ind]<-sqrt(sum(mse_glmsc)/(3000*100))
  
    cov_effect[cov_ind]<-eta 
  }
    effect<-rep(cov_effect,3)
    mse_vec<-c(ave_glm_mse,ave_glmsc_mse,ave_splda_mse)
    Model<-rep(c('NBLDA_glm','NBLDA_glmsc','splda'),each=length(cov_effect))
    prop<-rep(prop,length(effect))
    
    list_result[[prop_ind]]<-data.frame(prop,effect,Model,mse_vec)
  
  }
  
  
  result<-do.call(rbind.data.frame, list_result)
  
  save(result,file=paste(loc,paste0('result_vec',param_set,'.RData'),sep='/'))
  
  