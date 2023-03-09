z_sim <- function(ntrials, nrepeats, asplit = 0.5, mean = 4, sd = 1.5){
    
    library(dplyr)
    
    #check condtions
    if (ntrials %% 2 != 0)
        return ("Invalid ntrials")
    if (!between(asplit,0,1))
        return ("Invalid asplit")
    
    #create output list
    mean_z_list <- vector(mode = "list", length = nrepeats)
    mean_list <- vector(mode = "list", length = nrepeats)
    
    #repeat simulation
    for (i in 1:nrepeats){
        #create random dataset t
        t <- rnorm(ntrials, mean, sd) 
        
        #z-scored t
        zt <- scale(t)[,1]
        
        #create and shuffle vector of a and b
        ab_vec <- c(rep("a",ntrials*asplit), rep("b", ntrials*(1-asplit)))
        ab_vec <- sample(ab_vec)
        
        #creates dataframe
        df <- data.frame(t,zt,ab_vec)
        
        #summary df of means of a and b groups z-scored (mean_z_val) and not z-scored (mean_val)
        sum_df <- df %>% group_by(ab_vec) %>% summarise(mean_z_val = mean(zt), mean_val = mean(t)) %>% as.data.frame()
        
        #save difference in means for this simulation
        mean_z_list[i] <- diff(sum_df$mean_z_val) #where diff = b_value - a_value
        mean_list[i] <- diff(sum_df$mean_val)
    }
    
    out_df <- data.frame(cbind(mean_z_list,mean_list))
    
    return(out_df)    
}

z_sim(10,20,0.6)
