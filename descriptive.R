####################################################
## descriptive analysis
####################################################

require(xtable)
require(stargazer)

# Summary
  sum <- as.data.frame(cbind(data$total_deaths_per_million, data$delta_GDP, data$delta_GGD, 
                             data$median_age, data$stringency_index.y, data$retail_and_recreation))
  
  colnames(sum) <- c("y1", "x1", "x2", "z1", "z2", "z3")
  stargazer(sum)

# Unconditional 
  
  # Full Model
    uncond_a <- eff.output.uncond %>% filter(Lambda > 1)
    uncond_b <- eff.output.uncond %>% filter(Lambda == 1)
    uncond_c <- eff.output.uncond %>% filter(Lambda < 1)
  
    stargazer(as.data.frame(cbind(uncond_a$Lambda, uncond_a$y, uncond_a$x_1, uncond_a$x_2)))
    stargazer(as.data.frame(cbind(uncond_b$Lambda, uncond_b$y, uncond_b$x_1, uncond_b$x_2)))
    stargazer(as.data.frame(cbind(uncond_c$Lambda, uncond_c$y, uncond_c$x_1, uncond_c$x_2)))
    
# Ratios - full model
    
    ratios = as.tibble(cbind(effratio))
    stargazer(as.data.frame(ratios))
    
# Statistics by region
    
    regions <- as.tibble(read_excel("regions.xlsx"))
    final_table_r <- inner_join(final_table,regions, by=c("Country"="iso_code")) %>%
                     select(region,Epsilon)
    
    t_final_table_r <- final_table_r %>% group_by(region) %>% summarise(N = n(), 
                                                                        mean = mean(Epsilon),
                                                                        std.dev = sd(Epsilon),
                                                                        min = min(Epsilon),
                                                                        pctl.25 = quantile(Epsilon, .25),
                                                                        pctl.75 = quantile(Epsilon, .75),
                                                                        max = max(Epsilon))
    t_final_table_r <- t_final_table_r[order(t_final_table_r[,3],decreasing = F),]
    
    xtable(t_final_table_r)
