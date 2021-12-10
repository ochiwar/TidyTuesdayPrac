library(latex2exp)
library(tidyverse)

tolerance <- 0.00001
thp <- 200
depth_wh <- 0
depth_bh <- 9700
segments <- 30


depth <- seq.int(from = depth_wh, to = depth_bh, length.out = segments + 1)

n <- length(depth)

fpa <- function(x) 9e-02 + 1e-4 * x + 5e-8 * x^2 - 2e-11 * x^3

depth_top <- depth_wh

dp_dz <- 0.002

p_in <- thp

output <- vector("list")

for (i in seq_len(n)){
    
    depth_prev <- ifelse(i == 1, depth_top, depth[i - 1])
    
    dL = depth[i] - depth_prev
    
    p_out = p_in + dp_dz * dL 
    
    cat(sprintf("i=%2d depth=%8.0f dL=%8.1f segment=%d \n", i, depth[i], dL, i-1))
    
    cat(sprintf("%8s %6s %6s %8s %8s %8s %10s \n", 
                "iter", "p_in", "p_out", "p_avg", "p_calc", "dp/dz", "eps"))
    
    epsilon <- 1
    
    iter <- 1
    
    while(epsilon > tolerance){
        p_avg <- (p_in + p_out) / 2
        dp_dz <- fpa(p_avg)
        p_calc <- p_in - (-dp_dz) *dL
        epsilon <- abs((p_out - p_calc) / p_calc)
        cat(sprintf("%8d %6.1f %6.1f %8.2f %8.2f %8.5f %10.8f \n", 
                    iter, p_in, p_out, p_avg, p_calc, dp_dz, epsilon))
        if(epsilon >= tolerance) p_out = p_calc
        iter <- iter + 1
    }
    
    p_in <-  p_out
    
    output[[i]] <- list(depth = depth[i], p_calc = p_calc, p_avg = p_avg, dp_dz = dp_dz)
}

out_df <- data.table::rbindlist(output)

ggplot(out_df, aes(x=dp_dz, y=p_calc)) +
    scale_y_continuous(limits = c(0, max(out_df$p_calc)),
                       breaks = seq(0, max(out_df$p_calc), 100)) + 
    geom_line() + 
    geom_point() + 
    labs(title = TeX("Pressure vs $\\frac{dp}{dz}$"))


ggplot(out_df, aes(x = dp_dz, y = depth)) +

    geom_line() +
    geom_point() +
    
    scale_y_reverse(limits = c(max(out_df$depth), 0), 
                    breaks = seq(0, max(out_df$depth), 500)) +
    
    labs(title = TeX("Depth vs $\\frac{dp}{dz}$"))


ggplot(out_df, aes(x = p_calc, y = depth)) +
    
    geom_line() +
    geom_point() +
    
    scale_y_reverse(limits = c(max(out_df$depth), 0), 
                    breaks = seq(0, max(out_df$depth), 500)) +
    
    labs(title = "Depth vs Pressure")
                        
                        