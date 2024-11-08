##Import libraries required
library(tidyverse)
library("readxl")
library(ggpubr)

##Read in file containing binary (0 or 1) folding classification
cftr2_org <- read_excel("C:/Users/marce/ownCloud/CFTR/Databases and Schemes/CFTR2/CFTR2_Barcode.xlsx", sheet = 'Binary 4 par')

##Filter out wild-type
cftr2_org <- cftr2_org %>% filter(cftr2_org$Domain != "wild-type")

##Read excel (March 2019 version) file obtained from https://cftr2.org/mutations_history
cftr2_0319 <- read_excel("../CFTR2_11March2019.xlsx", sheet = "Sheet3")

##Pre-defined domain order for sorting
domain_order <- c("TMD1", "NBD1", "TMD2", "NBD2")

##Summarise the number of mutations in each domain for the binary scoring model
cftr2_org_labels <- cftr2_org %>% 
  group_by(Domain) %>% 
  summarise( 
    count = n())

##Summarise the number of mutations in each domain in the CFTR2 database
cftr2_0319_labels <- cftr2_0319 %>%
  group_by(Domain) %>% 
  summarise(
    count = n())


##Plot the distribution 
Distribution <- ggplot() +
                  geom_bar(data = cftr2_org, mapping = aes(x = 0, fill = factor(Domain, levels = c("TMD1", "NBD1", "R-region", "TMD2", "NBD2"))), position = "fill") +
                  geom_bar(data = cftr2_0319, mapping = aes(x = 1, fill = factor(Domain, levels = c("TMD1", "NBD1", "R-region", "TMD2", "NBD2"))), position = "fill") +
                  theme_classic() + labs(x = '') + theme( axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
                  scale_fill_manual(values = c("#FF8000", "#991999", "gray", "#00FF00", "#3380CC"))

##Uncomment to covert the plot to plotly to make it interactive.
# Distribution <- plotly::ggplotly(Distribution)

##Create plot
Distribution
