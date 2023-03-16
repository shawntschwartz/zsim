library("ggplot2")

#define
ntrials <- c(10,20,50,100,500,1000)
asplit <- c(0.1,0.2,0.3,0.4,0.5)
nrepeats <- 5000
a_mean <- 5
a_sd <- 1.5
b_mean <- 2
b_sd <- 1

ab_sim_df <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(ab_sim_df) <- c("ntrials","asplit","sim_mean_z","sim_mean")

for (i in ntrials){
  #set random seed
  set.seed(17)
  
  for (j in asplit){#split t into groups by different fractions
    #create two empty lists
    mean_z_list <- vector("list", nrepeats) #stores mean z-scored difference (bgroup-agroup) of each simulation
    mean_list <- vector("list", nrepeats)
    
    for (k in 1:nrepeats){#repeat simulation nrepeats times
      a <- rnorm(i*j, a_mean, a_sd) #generate normal dist a
      b <- rnorm(i*(1-j), b_mean, b_sd)
      
      t <- c(a,b) #combine a and b datasets
      zt <- scale(t)[,1] #z-score datasets together
      
      ab_vec <- c(rep("a",i*j),rep("b",i*(1-j))) #create vector of As and Bs
      
      df <- data.frame(t, zt, ab_vec) #create dataframe of "raw" data
      
      sum_df <- df %>% group_by(ab_vec) %>% summarise(mean_zt  = mean(zt), mean_t = mean(t)) %>% as.data.frame()
      
      mean_z_list[k] <- diff(sum_df$mean_zt) #difference between mean_zt(bgroup) - mean_zt(agroup)
      mean_list[k] <- diff(sum_df$mean_t) 
      
    }
    
    orig_df <- data.frame(cbind(mean_z_list, mean_list)) #creates df of difference (b-a) of means (z-scored and non-z-scored) for each repeat (nrepeats)
    orig_df <- apply(orig_df,2,as.character)
    write.csv (orig_df, paste0("a>b_",i,"_",j,"_orig.csv")) #creates csv of simulation outputs
    
    sim_mean_z <- mean(unlist(mean_z_list)) #average z-scored mean across all nrepeats
    sim_mean <- mean(unlist(mean_list))
    
    ab_sim_df[nrow(ab_sim_df) + 1,] <- c(i,j,sim_mean_z,sim_mean) #output df
  }
}

ab_sim_df$ntrials <- factor(ab_sim_df$ntrials)

#plot using sim_df output
ab_mean_z_plot <- ggplot(ab_sim_df, aes(asplit, sim_mean_z, col = ntrials)) + geom_line() + ggtitle("a>b mean_z")
ab_mean_plot <- ggplot(ab_sim_df, aes(asplit, sim_mean, col = ntrials)) + geom_line() + ggtitle("a>b mean")