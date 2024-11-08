library(tidyverse)
library("readxl")
library(ggpubr)

std <- function(x) sd(x)/sqrt(length(x))

wt_timecourse <- read_excel("~/R/WT_timecourse/MW186_MW193_MW195_combined.xlsx", sheet = "percentage")

mean_table <- wt_timecourse %>%
  group_by(Chase) %>%
  summarise(
    mean_g = mean(Percentage_Golgi, na.rm = TRUE),
    std_g = std(Percentage_Golgi),
    mean_T1 = mean(`TMD1C_T1d-f`, na.rm = TRUE),
    std_T1 = std(`TMD1C_T1d-f`),
    mean_E122 = mean(`E1-22_late`, na.rm = TRUE),
    std_E122 = std(`E1-22_late`),
    mean_TMD1 = mean(mean_T1, mean_E122),
    sd_TMD1 = sd(mean_T1, mean_E122),
    mean_N1 = mean(NBD1, na.rm = TRUE),
    std_N1 = std(NBD1),
    mean_T2 = mean(`TMD2C_late`, na.rm = TRUE),
    std_T2 = std(`TMD2C_late`),
    mean_I4  = mean(`ICL4_late`, na.rm = TRUE),
    std_I4 = std(`ICL4_late`),
    mean_TMD2 = mean(mean_T2, mean_I4),
    sd_TMD2 = sd(mean_T2, mean_I4),
    mean_N2 = mean(NBD2, na.rm = TRUE),
    std_N2 = std(NBD2),
  )

mean_table


Golgi <- ggplot(data = mean_table) +
            #Plot Data points
            geom_point(mapping = aes(x = Chase, y = mean_g), size = 3, color = "black") +
            #Plot Error bars
            geom_errorbar(mapping = aes(x = Chase, ymin = mean_g-std_g, ymax= mean_g+std_g), width=1, color = "black") +
            geom_line(mapping = aes(x = Chase, y =  mean_g), color = "black") +
            #Add labels and set theme to classic
            labs(x =  "Chase time, (min.)",
                 y= "Fraction  (Golgi/Total CFTR), (AU)"
                ) + 
            theme_classic(base_size = 7) +  scale_x_continuous(breaks=c(0, 30, 60, 90, 120), limits = c(-.5, 121)) + 
  scale_y_continuous(breaks=c(0, 0.25, 0.50, 0.75, 1.00, 1.25, 1.5), limits = c(0, 1.25))


TMD1_frag <- ggplot(data = mean_table) +
              #Plot Data points
              geom_point(mapping = aes(x = Chase, y = mean_T1), size = 3, color = "orange") +
              #Plot Error bars
              geom_errorbar(mapping = aes(x = Chase, ymin = mean_T1-std_T1, ymax= mean_T1+std_T1), width=1, color = "orange") +
              geom_line(mapping = aes(x = Chase, y = mean_T1), color = "orange") +
              #Add labels and set theme to classic
              labs(x =  "Chase time, (min.)",
                   y= "Fraction  (T1d-f/Total CFTR), (AU)"
                   ) +
              theme_classic(base_size = 7) +  scale_x_continuous(breaks=c(0, 30, 60, 90, 120), limits = c(-.5, 121)) + 
  scale_y_continuous(breaks=c(0, 0.25, 0.50, 0.75, 1.00, 1.25, 1.5), limits = c(0, 1.25))

E122_frag <- ggplot(data = mean_table) +
              #Plot Data points
              geom_point(mapping = aes(x = Chase, y = mean_E122), size = 3, color = "darkorange2") +
              #Plot Error bars
              geom_errorbar(mapping = aes(x = Chase, ymin = mean_E122-std_E122, ymax= mean_E122+std_E122), width=1, color = "darkorange2") +
              geom_line(mapping = aes(x = Chase, y = mean_E122), color = "darkorange2") +
              #Add labels and set theme to classic
              labs(x =  "Chase time, (min.)",
                   y= "Fraction  (T1d-f/Total CFTR), (AU)"
                   ) +
              theme_classic(base_size = 7) +  scale_x_continuous(breaks=c(0, 30, 60, 90, 120), limits = c(-.5, 121)) + 
  scale_y_continuous(breaks=c(0, 0.25, 0.50, 0.75, 1.00, 1.25, 1.5), limits = c(0, 1.25))

NBD1_frag <- ggplot(data = mean_table) +
              #Plot Data points
              geom_point(mapping = aes(x = Chase, y = mean_N1), size = 3, color = "purple") +
              #Plot Error bars
              geom_errorbar(mapping = aes(x = Chase, ymin = mean_N1-std_N1, ymax= mean_N1+std_N1), width=1, color = "purple") +
              geom_line(mapping = aes(x = Chase, y = mean_N1), color = "Purple") +
              #Add labels and set theme to classic
              labs(x =  "Chase time, (min.)",
                   y= "Fraction  (N1a/Total CFTR), (AU)"
                   ) +
              theme_classic(base_size = 7) +  scale_x_continuous(breaks=c(0, 30, 60, 90, 120), limits = c(-.5, 121)) + 
  scale_y_continuous(breaks=c(0, 0.25, 0.50, 0.75, 1.00, 1.25, 1.5), limits = c(0, 1.25))

TMD2_frag <- ggplot(data = mean_table) +
                #Plot Data points
                geom_point(mapping = aes(x = Chase, y = mean_T2), size = 3, color = "green") +
                #Plot Error bars
                geom_errorbar(mapping = aes(x = Chase, ymin = mean_T2-std_T2, ymax= mean_T2+std_T2), width=1, color = "green") +
                #Plot linear regression model
                geom_line(mapping = aes(x = Chase, y = mean_T2), color = "green") +
                #Add labels and set theme to classic
                labs(x =  "Chase time, (min.)",
                     y= "Fraction  (T2c/Total CFTR), (AU)"
                     ) +
                theme_classic(base_size = 7) +  scale_x_continuous(breaks=c(0, 30, 60, 90, 120), limits = c(-.5, 121)) + 
  scale_y_continuous(breaks=c(0, 0.25, 0.50, 0.75, 1.00, 1.25, 1.5), limits = c(0, 1.25))

ICL4_frag <- ggplot(data = mean_table) +
              #Plot Data points
              geom_point(mapping = aes(x = Chase, y = mean_I4), size = 3, color = "darkgreen") +
              #Plot Error bars
              geom_errorbar(mapping = aes(x = Chase, ymin = mean_I4-std_I4, ymax= mean_I4+std_I4), width=1, color = "darkgreen") +
              #Plot linear regression model
              geom_line(mapping = aes(x = Chase, y = mean_I4), color = "darkgreen") +
              #Add labels and set theme to classic
              labs(x =  "Chase time, (min.)",
                   y= "Fraction  (T2c/Total CFTR), (AU)"
                   ) +
              theme_classic(base_size = 7) +  scale_x_continuous(breaks=c(0, 30, 60, 90, 120), limits = c(-.5, 121)) + 
  scale_y_continuous(breaks=c(0, 0.25, 0.50, 0.75, 1.00, 1.25, 1.5), limits = c(0, 1.25))

NBD2_frag <- ggplot(data = mean_table) +
              #Plot Data points
              geom_point(mapping = aes(x = Chase, y = mean_N2), size = 3, color = "blue") +
              #Plot Error bars
              geom_errorbar(mapping = aes(x = Chase, ymin = mean_N2-std_N2, ymax= mean_N2+std_N2), width=1, color = "blue") +
              #Plot linear regression model
             geom_line(mapping = aes(x = Chase, y = mean_N2), color = "blue") +
              #Add labels and set theme to classic
              labs(x =  "Chase time, (min.)",
                   y= "Fraction  (N2a/Total CFTR), (AU)"
                   ) +
              theme_classic(base_size = 7) +  scale_x_continuous(breaks=c(0, 30, 60, 90, 120), limits = c(-.5, 121)) + 
  scale_y_continuous(breaks=c(0, 0.25, 0.50, 0.75, 1.00, 1.25, 1.5), limits = c(0, 1.25))

seperate_graphs <- ggarrange(Golgi, TMD1_frag, E122_frag, NBD1_frag, TMD2_frag, ICL4_frag, NBD2_frag, 
                              labels = c("A", "B", "C", "D", "E", "F", "G"),
                              ncol = 2, nrow = 4, vjust = 0.5)
seperate_graphs 

all <- ggplot(data = mean_table) +
  #Plot Data points
  geom_point(mapping = aes(x = Chase, y = mean_g, color = "Golgi"), size = 3) +
  #Plot Error bars
  geom_errorbar(mapping = aes(x = Chase, ymin = mean_g-std_g, ymax= mean_g+std_g), width=1, color = "black") +
  #Plot linear regression model
  geom_line(mapping = aes(x = Chase, y =  mean_g), color = "black") +
   #Plot Data points
   geom_point(mapping = aes(x = Chase, y = mean_TMD1, color = "TMD1"), size = 3) +
   #Plot Error bars
   geom_errorbar(mapping = aes(x = Chase, ymin = mean_TMD1-sd_TMD1, ymax= mean_TMD1+sd_TMD1), width=1, color = "orange") +
  geom_line(mapping = aes(x = Chase, y = mean_TMD1), color = "orange") +
 #  #Plot Data points
 #  geom_point(mapping = aes(x = Chase, y = mean_T1, color = "TMD1C: T1d-f"), size = 3) +
 #  #Plot Error bars
 #  geom_errorbar(mapping = aes(x = Chase, ymin = mean_T1-std_T1, ymax= mean_T1+std_T1), width=1, color = "orange") +
 # geom_line(mapping = aes(x = Chase, y = mean_T1), color = "orange") +
 #  #Plot Data points
 #  geom_point(mapping = aes(x = Chase, y = mean_E122, color = "E1-22: T1d-f"), size = 3) +
 #  #Plot Error bars
 #  geom_errorbar(mapping = aes(x = Chase, ymin = mean_E122-std_E122, ymax= mean_E122+std_E122), width=1, color = "darkorange2") +
 #  
 # geom_line(mapping = aes(x = Chase, y = mean_E122), color = "darkorange2") +
  #Plot Data points
  geom_point(mapping = aes(x = Chase, y = mean_N1, color = "N1a"), size = 3) +
  #Plot Error bars
  geom_errorbar(mapping = aes(x = Chase, ymin = mean_N1-std_N1, ymax= mean_N1+std_N1), width=1, color = "purple") +
  #Plot linear regression model
 geom_line(mapping = aes(x = Chase, y = mean_N1), color = "Purple") +
 #  #Plot Data points
 #  geom_point(mapping = aes(x = Chase, y = mean_T2, color = "TMD2C: T2c"), size = 3) +
 #  #Plot Error bars
 #  geom_errorbar(mapping = aes(x = Chase, ymin = mean_T2-std_T2, ymax= mean_T2+std_T2), width=1, color = "green") +
 #  #Plot linear regression model
 # geom_line(mapping = aes(x = Chase, y = mean_T2), color = "green") +
 #  #Plot Data points
 #  geom_point(mapping = aes(x = Chase, y = mean_I4, color = "ICL4: T2c"), size = 3) +
 #  #Plot Error bars
 #  geom_errorbar(mapping = aes(x = Chase, ymin = mean_I4-std_I4, ymax= mean_I4+std_I4), width=1, color = "darkgreen") +
 #  #Plot linear regression model
 # geom_line(mapping = aes(x = Chase, y = mean_I4), color = "darkgreen") +
#Plot Data points
geom_point(mapping = aes(x = Chase, y = mean_TMD2, color = "TMD2"), size = 3) +
  #Plot Error bars
  geom_errorbar(mapping = aes(x = Chase, ymin = mean_TMD2-sd_TMD2, ymax= mean_TMD2+sd_TMD2), width=1, color = "green") +
  #Plot linear regression model
  geom_line(mapping = aes(x = Chase, y = mean_TMD2), color = "green") +
  #Plot Data points
  geom_point(mapping = aes(x = Chase, y = mean_N2, color = "N2a"), size = 3) +
  #Plot Error bars
  geom_errorbar(mapping = aes(x = Chase, ymin = mean_N2-std_N2, ymax= mean_N2+std_N2), width=1, color = "blue") +
  #Plot linear regression model
 geom_line(mapping = aes(x = Chase, y = mean_N2), color = "blue") +
  #Add labels and set theme to classic
  labs(x =  "Chase time, (min.)",
       y= "Fraction  (X/Total CFTR), (AU)"
       ) +
  scale_colour_manual(name="",
                      values=c(Golgi ="black", 'TMD1' ="orange", 'E1-22: T1d-f' = "darkorange2", N1a ="purple",'TMD2' = "green", "ICL4: T2c" = "darkgreen", N2a ="blue")) +
  theme_classic(base_size = 10)

all +  scale_x_continuous(breaks=c(0, 30, 60, 90, 120), limits = c(-.5, 121)) + 
  scale_y_continuous(breaks=c(0, 0.25, 0.50, 0.75, 1.00, 1.25, 1.5), limits = c(0, 1.25))