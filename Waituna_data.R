library(tidyverse)
library("reshape2")
library("grid")
library("gridExtra")
library(xlsx)
library(patchwork)

#read csv files
df_noc <- read.csv("GAMS_UPDATE_manual_solve_nitrogen_optimisation_catchment.csv", header = TRUE, sep = ";")
df_nof <- read.csv("GAMS_UPDATE_manual_solve_nitrogen_optimisation_farm.csv", header = TRUE, sep = ";")
df_non <- read.csv("GAMS_nitrogen_optimisation_nutrientcap_3Nov2020.csv", header = TRUE)
df_poc <- read.csv("GAMS_UPDATE_manual_solve_phosphorus_optimisation_catchment.csv", header = TRUE, sep = ";")
df_pof <- read.csv("GAMS_UPDATE_manual_solve_phosphorus_optimisation_farm.csv", header = TRUE, sep = ";")
df_pon <- read.csv("GAMS_phosphorus_optimisation_nutrientcap_3Nov2020.csv", header = TRUE)

#Add factors for optimisation approach
df_noc$group <- as.factor("noc")
df_nof$group <- as.factor("nof")
df_non$group <- as.factor("non")
df_poc$group <- as.factor("poc")
df_pof$group <- as.factor("pof")
df_pon$group <- as.factor("pon")

#link the csv files together and make a tibble data frame
df <- rbind(df_noc,df_nof,df_non,df_poc,df_pof,df_pon)
head(df)
str(df)
dft <- as_tibble(df)

#changes column names that are wrong
dft <- dft %>% rename(Ebit_dollar = Ebit_K, TNLoad_kg = TNLoad_ton, TNLoad_red_FSM_kg = TNLoad_red_FSM_ton,
              TNLoad_red_EoFM_kg = TNLoad_red_EoFM_ton, TPLoad_kg = TPLoad_ton, TPLoad_red_FSM_kg = TPLoad_red_FSM_ton,
              TPLoad_red_EoFM_kg = TPLoad_red_EoFM_ton)

str(dft)

# Extract the target % reduction level from the overview variable
dft$overview <- as.character(dft$overview)
dft$red_target <- str_extract(dft$overview, pattern = "[0-9]+")
dft$red_target <- as.factor(dft$red_target)

# split dft to change red_target values for pon group
group_pon <- filter(dft, group %in% c("pon"))
group_not_pon <- filter(dft, group != c("pon"))
group_pon$red_target <- c("0.6","0.8","1.0","1.2","1.4")
dft <- rbind(group_not_pon,group_pon)

#reshape the table to long skinny
dft_melt <- melt(dft, id.vars = c("group","red_target","Ebit_red"), measure.vars = c("TN_red","TP_red"))

# ggplot code for marginal abatement curves
MAC_catch_n <- ggplot(subset(dft_melt,group %in% c("noc")), aes(value, Ebit_red)) +
  geom_line(aes(shape = variable)) +
  geom_point(aes(shape = variable), size=2) +
  labs(y = "Profit reduction %", x = NULL) +
  ylim(0,50) +
  theme_classic() +
  theme_linedraw() +
  theme_light() +
  theme(axis.title.x = element_text(face="bold", size=12),axis.text.x  = element_text(size=12)) +
  theme(axis.title.y = element_text(face="bold", size=12),axis.text.y  = element_text(size=12)) +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
  theme(legend.position = "none")
  #theme(legend.position = "right") +
  #theme(legend.position = c(0.85,0.2)) +
  #labs(shape = "Nutrient") +
  #scale_shape_discrete(labels = c("N","P"))

MAC_catch_n

MAC_catch_p <- ggplot(subset(dft_melt,group %in% c("poc")), aes(value, Ebit_red)) +
  geom_line(aes(shape = variable)) +
  geom_point(aes(shape = variable), size=2) +
  labs(y = NULL, x = NULL) +
  theme_classic() +
  ylim(0,50) +
  theme_linedraw() +
  theme_light() +
  theme(axis.title.x = element_text(face="bold", size=12),axis.text.x  = element_text(size=12)) +
  theme(axis.title.y = element_text(face="bold", size=12),axis.text.y  = element_text(size=12)) +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
  theme(legend.position = "none")
  #theme(legend.position = "right") +
  #theme(legend.position = c(0.85,0.2)) +
  #labs(shape = "Nutrient") +
  #scale_shape_discrete(labels = c("N","P"))

MAC_catch_p

MAC_farm_n <- ggplot(subset(dft_melt,group %in% c("nof")), aes(value, Ebit_red)) +
  geom_line(aes(shape = variable)) +
  geom_point(aes(shape = variable), size=2) +
  labs(x = "Nutrient reduction %", y = "Profit reduction %") +
  xlim(0,50) +
  ylim(0,50) +
  theme_classic() +
  theme_linedraw() +
  theme_light() +
  theme(axis.title.x = element_text(face="bold", size=12),axis.text.x  = element_text(size=12)) +
  theme(axis.title.y = element_text(face="bold", size=12),axis.text.y  = element_text(size=12)) +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
  theme(legend.position = "none")
  #theme(legend.position = "right") +
  #theme(legend.position = c(0.85,0.2)) +
  #labs(shape = "Nutrient") +
  #scale_shape_discrete(labels = c("N","P"))

MAC_farm_n

MAC_farm_p <- ggplot(subset(dft_melt,group %in% c("pof")), aes(value, Ebit_red)) +
  geom_line(aes(shape = variable)) +
  geom_point(aes(shape = variable), size=2) +
  labs(x = "Nutrient reduction %", y = NULL) +
  xlim(0,50) +
  ylim(0,50) +
  theme_classic() +
  theme_linedraw() +
  theme_light() +
  theme(axis.title.x = element_text(face="bold", size=12),axis.text.x  = element_text(size=12)) +
  theme(axis.title.y = element_text(face="bold", size=12),axis.text.y  = element_text(size=12)) +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
  theme(legend.position = "none")
  #theme(legend.position = "right") +
  #theme(legend.position = c(0.85,0.2)) +
  #labs(shape = "Nutrient") +
  #scale_shape_discrete(labels = c("N","P"))

MAC_farm_p

mac_catch_and_farm <- MAC_catch_n + MAC_catch_p + MAC_farm_n + MAC_farm_p + plot_layout(widths = c(1, 1)) +
  plot_annotation(tag_levels = list(c("Catchment N focus", "Catchment P focus", "Farm N focus", "Farm P focus"))) &
  theme(plot.tag.position = c(0.4, 1),
        plot.tag = element_text(size = 10, hjust = 0, vjust = -0.2))

mac_catch_and_farm

# MAC_cap is no longer relevant. These graphs will be generated further down in the script
#MAC_cap <- ggplot(subset(dft_melt,group %in% c("non","pon")), aes(value, Ebit_red)) +
#  geom_line(aes(lty = group,shape = variable)) +
#  geom_point(aes(shape = variable), size=2) +
#  labs(x = "Nutrient reduction %", y = "Profit reduction %") +
#  theme_classic() +
#  theme_linedraw() +
#  theme_light() +
#  theme(axis.title.x = element_text(face="bold", size=12),axis.text.x  = element_text(size=12)) +
#  theme(axis.title.y = element_text(face="bold", size=12),axis.text.y  = element_text(size=12)) +
#  theme(axis.line.x = element_line(color="black", size = 0.5),
#        axis.line.y = element_line(color="black", size = 0.5)) +
#  theme(legend.position = "right") +
#  theme(legend.position = c(0.85,0.2)) +
#  labs(shape = "Nutrient", lty = "Mitigation focus") +
#  scale_linetype_discrete(labels = c("N","P")) +
#  scale_shape_discrete(labels = c("N","P"))

#MAC_cap

#generate new melt table for proportion FSM and EoFM used for mitigations with N as the focus
dft_melt2 <- melt(dft, id.vars = c("group","red_target"), measure.vars = c("TN_FSM_red","TN_EoFM_red"))
dft_melt2 <- dft_melt2 %>% filter(red_target != "0")
group.labs.n <- c("Catchment N","Farm N", "Nutrient cap N")
names(group.labs.n) <- c("noc", "nof", "non")
group.labs.p <- c("Catchment P","Farm P", "Nutrient cap P")
names(group.labs.p) <- c("poc", "pof", "pon")

# construct bar graphs with proportion contribution of FSM and EoFM to mitigations
BAR_graph_noc <- ggplot(subset(dft_melt2, group %in% c("noc")), aes(x = factor(red_target,levels = c("5","10","15","20","25","30","35","40","45","50")), y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_grey(start = 0.4, end = 0.6,labels = c("FSM", "EoFM")) +
  theme_bw() +
  theme(legend.position = "none") +
  facet_wrap(.~group, labeller = labeller(group = group.labs.n)) +
  labs(x = NULL, y = "Proportion") +
  theme(axis.title.x = element_text(face="bold", size=10),axis.text.x  = element_text(size=8)) +
  theme(axis.title.y = element_text(face="bold", size=10),axis.text.y  = element_text(size=8)) +
  theme(axis.line.x = element_line(color="black", size = 0.5),axis.line.y = element_line(color="black", size = 0.5)) 
  #theme(legend.position = "right") +
  #labs(fill = "Mitigation option")
  
BAR_graph_noc

BAR_graph_nof <- ggplot(subset(dft_melt2, group %in% c("nof")), aes(x = factor(red_target,levels = c("5","10","15","20","25","30","35","40","45","50")), y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_grey(start = 0.4, end = 0.6,labels = c("FSM", "EoFM")) +
  theme_bw() +
  theme(legend.position = "none") +
  facet_wrap(.~group, labeller = labeller(group = group.labs.n)) +
  labs(x = NULL, y = NULL) +
  theme(axis.title.x = element_text(face="bold", size=10),axis.text.x  = element_text(size=8)) +
  theme(axis.title.y = element_text(face="bold", size=10),axis.text.y  = element_text(size=8)) +
  theme(axis.line.x = element_line(color="black", size = 0.5),axis.line.y = element_line(color="black", size = 0.5)) 
  #theme(legend.position = "right") +
  #labs(fill = "Mitigation option")

BAR_graph_nof

BAR_graph_non <- ggplot(subset(dft_melt2, group %in% c("non")), aes(x = factor(red_target,levels = c("30","35","40","45","50")), y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_grey(start = 0.4, end = 0.6,labels = c("FSM", "EoFM")) +
  theme_bw() +
  theme(legend.position = "none") +
  facet_wrap(.~group, labeller = labeller(group = group.labs.n)) +
  labs(x = NULL, y = NULL) +
  theme(axis.title.x = element_text(face="bold", size=10),axis.text.x  = element_text(size=8)) +
  theme(axis.title.y = element_text(face="bold", size=10),axis.text.y  = element_text(size=8)) +
  theme(axis.line.x = element_line(color="black", size = 0.5),axis.line.y = element_line(color="black", size = 0.5))
  #theme(legend.position = "right") +
  #labs(fill = "Mitigation option")

BAR_graph_non

#generate new melt table for proportion FSM and EoFM used for mitigations with P as the focus
dft_melt3 <- melt(dft, id.vars = c("group","red_target"), measure.vars = c("TP_FSM_red","TP_EoFM_red"))
dft_melt3 <- dft_melt3 %>% filter(red_target != "0")
group.labs.n <- c("Catchment N","Farm N", "Nutrient cap N")
names(group.labs.n) <- c("noc", "nof", "non")
group.labs.p <- c("Catchment P","Farm P", "Nutrient cap P")
names(group.labs.p) <- c("poc", "pof", "pon")

BAR_graph_poc <- ggplot(subset(dft_melt3, group %in% c("poc")), aes(x = factor(red_target,levels = c("5","10","15","20","25","30","35","40","45","50")), y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_grey(start = 0.4, end = 0.6,labels = c("FSM", "EoFM")) +
  theme_bw() +
  theme(legend.position = "none") +
  facet_wrap(.~group, labeller = labeller(group = group.labs.p)) +
  labs(x = "Reduction target %", y = "Proportion") +
  theme(axis.title.x = element_text(face="bold", size=10),axis.text.x  = element_text(size=8)) +
  theme(axis.title.y = element_text(face="bold", size=10),axis.text.y  = element_text(size=8)) +
  theme(axis.line.x = element_line(color="black", size = 0.5),axis.line.y = element_line(color="black", size = 0.5)) 
  #theme(legend.position = "right") +
  #labs(fill = "Mitigation option")

BAR_graph_poc

BAR_graph_pof <- ggplot(subset(dft_melt3, group %in% c("pof")), aes(x = factor(red_target,levels = c("5","10","15","20","25","30","35","40","45","50")), y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_grey(start = 0.4, end = 0.6,labels = c("FSM", "EoFM")) +
  theme_bw() +
  theme(legend.position = "none") +
  facet_wrap(.~group, labeller = labeller(group = group.labs.p)) +
  labs(x = "Reduction target %", y = NULL) +
  theme(axis.title.x = element_text(face="bold", size=10),axis.text.x  = element_text(size=8)) +
  theme(axis.title.y = element_text(face="bold", size=10),axis.text.y  = element_text(size=8)) +
  theme(axis.line.x = element_line(color="black", size = 0.5),axis.line.y = element_line(color="black", size = 0.5)) 
  #theme(legend.position = "right") +
  #labs(fill = "Mitigation option")

BAR_graph_pof

BAR_graph_pon <- ggplot(subset(dft_melt3, group %in% c("pon")), aes(x = factor(red_target,levels = c("0.6","0.8","1.0","1.2","1.4")), y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_grey(start = 0.4, end = 0.6,labels = c("FSM", "EoFM")) +
  theme_bw() +
  theme(legend.position = "none") +
  facet_wrap(.~group, labeller = labeller(group = group.labs.p)) +
  labs(x = "Reduction target kg/ha", y = NULL) +
  theme(axis.title.x = element_text(face="bold", size=10),axis.text.x  = element_text(size=8)) +
  theme(axis.title.y = element_text(face="bold", size=10),axis.text.y  = element_text(size=8)) +
  theme(axis.line.x = element_line(color="black", size = 0.5),axis.line.y = element_line(color="black", size = 0.5)) 
  #theme(legend.position = "right") +
  #labs(fill = "Mitigation option")

BAR_graph_pon

bar_graphs <- BAR_graph_noc + BAR_graph_nof + BAR_graph_non + BAR_graph_poc + BAR_graph_pof + BAR_graph_pon +
  plot_layout(ncol = 3, heights = c(1,1)) 
  #plot_annotation(tag_levels = list(c("Catchment N focus", "Farm N focus", "Nutrient cap N focus", "Catchment P focus", "Farm P focus", "Nutrient cap P focus"))) &
  #theme(plot.tag.position = c(0.5, 1),
  #      plot.tag = element_text(size = 8, hjust = 0, vjust = -0.2))

bar_graphs

# extract cost-efficiency comparison
dft_min_NLoad <- dft %>% 
                  filter(group == "noc"| group =="nof" | group == "non") %>%
                    group_by(group) %>%
                      summarize(min_NLoad_kg = min(TNLoad_kg))
                          
dft_min_ebit <- dft %>%
                  filter(group == "noc"| group =="nof" | group == "non") %>%
                    group_by(group) %>%
                      summarize(min_ebit = min(Ebit_dollar))

# export dft table to excel for extracting cost-efficiencies manually 
write.xlsx(dft,"overview_data.xlsx")

# load data for frequency distribution histograms
# N focus
df_noc_zero <- read.csv("overview_nitrogen_0red.csv", header = TRUE)
df_areaprof <- read.csv("AreaProf.csv", header = TRUE)
df_noc_50 <- read.csv("noc_overview_nitrogen_50red.csv", header = TRUE)
df_nof_30 <- read.csv("nof_overview_nitrogen_30red.csv", header = TRUE, sep = ";") #the 30% farm target gives approx 50% reduction at catchment scale
df_non_N40_P0.6 <- read.csv("non_overview__n40_p0.6KgHa.csv", header = TRUE, sep = ";")

# join data frames by farm and make as tibble. Calcualte new profit and N loss by ha
df_zero <- left_join(df_areaprof,df_noc_zero)
df_noc_50 <- left_join(df_areaprof,df_noc_50)
df_nof_30 <- left_join(df_areaprof,df_nof_30)
df_non_N40_P0.6 <- left_join(df_areaprof,df_non_N40_P0.6)
df_nof_30 <- rename(df_nof_30, z = EBIT)
df_non_N40_P0.6 <- rename(df_non_N40_P0.6, z = EBIT)
df_zero <- mutate(df_zero, Profit_ha = z/FarmArea_ha, Nleach_ha = farmN/FarmArea_ha, catch_target = "noc_0" )
df_noc_50 <- mutate(df_noc_50, Profit_ha = z/FarmArea_ha, Nleach_ha = farmN/FarmArea_ha, catch_target = "noc_50" )
df_nof_30 <- mutate(df_nof_30, Profit_ha = z/FarmArea_ha, Nleach_ha = farmN/FarmArea_ha, catch_target = "nof_30" )
df_nof_30 <- select(df_nof_30, farm,LandUse,FarmType,FarmArea_ha,z,farmN,farmP,Profit_ha,Nleach_ha,catch_target)
df_non_N40_P0.6 <- mutate(df_non_N40_P0.6, Profit_ha = z/FarmArea_ha, Nleach_ha = farmN/FarmArea_ha, catch_target = "non_N40_P0.6" )
df_non_N40_P0.6 <- select(df_non_N40_P0.6, farm,LandUse,FarmType,FarmArea_ha,z,farmN,farmP,Profit_ha,Nleach_ha,catch_target)
df_n <- rbind(df_zero,df_noc_50,df_nof_30,df_non_N40_P0.6)

dft_n <- as_tibble(df_n)
dft_n$LandUse <- as.factor(dft_n$LandUse)
landuse <- c("1"="Dairy", "2"="Dairy support", "3"="Sheep & Beef")
target <- c(noc_0 = "0% N", noc_50 = "Catch 50% N", nof_30 = "Farm 50% N", non_N40_P0.6 = "Cap N40 P0.6") #the 30% farm target gives approx 50% reduction at catchment scale

# construct histogram by Landuse
HIST_n_nleach <- ggplot() +
  geom_histogram(data=dft_n, mapping=aes(Nleach_ha, fill = LandUse), binwidth = 10) +
  geom_vline(data = dft_n, mapping=aes(xintercept=mean(df_zero$Nleach_ha)), colour="magenta", size=1) +
  scale_fill_grey(start = 0.2, end = 0.6) +
  theme_bw() +
  facet_grid(catch_target ~ LandUse, labeller = labeller(catch_target = target, LandUse = landuse)) +
  labs(x = "N leaching kg/ha", y = "Frequency") +
  theme(axis.title.x = element_text(face="bold", size=12),axis.text.x  = element_text(size=8)) +
  theme(axis.title.y = element_text(face="bold", size=12),axis.text.y  = element_text(size=8)) +
  theme(axis.line.x = element_line(color="black", size = 0.5),axis.line.y = element_line(color="black", size = 0.5)) +
  theme(legend.position = "none")

HIST_n_nleach

HIST_n_profit <- ggplot() +
  geom_histogram(data=dft_n, mapping=aes(Profit_ha, fill = LandUse), binwidth = 500) +
  geom_vline(data = dft_n, mapping=aes(xintercept=mean(df_zero$Profit_ha)), colour="magenta", size=1) +
  scale_fill_grey(start = 0.2, end = 0.6) +
  theme_bw() +
  facet_grid(catch_target ~ LandUse, labeller = labeller(catch_target = target, LandUse = landuse)) +
  labs(x = "Profit $/ha", y = "Frequency") +
  theme(axis.title.x = element_text(face="bold", size=12),axis.text.x  = element_text(size=8)) +
  theme(axis.title.y = element_text(face="bold", size=12),axis.text.y  = element_text(size=8)) +
  theme(axis.line.x = element_line(color="black", size = 0.5),axis.line.y = element_line(color="black", size = 0.5)) +
  theme(legend.position = "none")

HIST_n_profit

# load data for histograms
# P focus
df_poc_zero <- read.csv("overview_phosphorus_0red.csv", header = TRUE)
df_poc_50 <- read.csv("poc_overview_phosphorus_50red.csv", header = TRUE)
df_pof_30 <- read.csv("pof_overview_phosphorus_30red.csv", header = TRUE, sep = ";") # the farm target of 30% gives approx 50% P reduction at catchment scale
df_non_N40_P0.6 <- read.csv("non_overview__n40_p0.6KgHa.csv", header = TRUE, sep = ";")

# join data frames by farm and make as tibble. Calcualte new profit and P loss by ha
df_zero_p <- left_join(df_areaprof,df_poc_zero)
df_poc_50 <- left_join(df_areaprof,df_poc_50)
df_pof_30 <- left_join(df_areaprof,df_pof_30)
df_non_N40_P0.6 <- left_join(df_areaprof,df_non_N40_P0.6)
df_pof_30 <- rename(df_pof_30, z = EBIT)
df_non_N40_P0.6 <- rename(df_non_N40_P0.6, z = EBIT)
df_zero_p <- mutate(df_zero_p, Profit_ha = z/FarmArea_ha, Ploss_ha = farmP/FarmArea_ha, catch_target = "poc_0" )
df_poc_50 <- mutate(df_poc_50, Profit_ha = z/FarmArea_ha, Ploss_ha = farmP/FarmArea_ha, catch_target = "poc_50" )
df_pof_30 <- mutate(df_pof_30, Profit_ha = z/FarmArea_ha, Ploss_ha = farmP/FarmArea_ha, catch_target = "pof_30" )
df_non_N40_P0.6 <- mutate(df_non_N40_P0.6, Profit_ha = z/FarmArea_ha, Ploss_ha = farmP/FarmArea_ha, catch_target = "pon_N40_P0.6" )
df_pof_30 <- select(df_pof_30, farm,LandUse,FarmType,FarmArea_ha,z,farmN,farmP,Profit_ha,Ploss_ha,catch_target)
df_non_N40_P0.6 <- select(df_non_N40_P0.6, farm,LandUse,FarmType,FarmArea_ha,z,farmN,farmP,Profit_ha,Ploss_ha,catch_target)
df_p <- rbind(df_zero_p,df_poc_50,df_pof_30,df_non_N40_P0.6)

dft_p <- as_tibble(df_p)
dft_p$LandUse <- as.factor(dft_p$LandUse)
landuse <- c("1"="Dairy", "2"="Dairy support", "3"="Sheep & Beef")
target_p <- c(poc_0 = "0% P", poc_50 = "Catch 50% P", pof_30 = "Farm 50% P", pon_N40_P0.6 = "Cap N40 P0.6") #the 30% farm target gives approx 50% reduction at catchment scale

# construct histogram by Landuse
HIST_p_ploss <- ggplot() +
  geom_histogram(data=dft_p, mapping=aes(Ploss_ha, fill = LandUse), binwidth = 0.2) +
  geom_vline(data = dft_p, mapping=aes(xintercept=mean(df_zero_p$Ploss_ha)), colour="magenta", size=1) +
  scale_fill_grey(start = 0.2, end = 0.6) +
  theme_bw() +
  xlim(NA, 2.0) +
  facet_grid(catch_target ~ LandUse, labeller = labeller(catch_target = target_p, LandUse = landuse)) +
  labs(x = "P loss kg/ha", y = "Frequency") +
  theme(axis.title.x = element_text(face="bold", size=12),axis.text.x  = element_text(size=8)) +
  theme(axis.title.y = element_text(face="bold", size=12),axis.text.y  = element_text(size=8)) +
  theme(axis.line.x = element_line(color="black", size = 0.5),axis.line.y = element_line(color="black", size = 0.5)) +
  theme(legend.position = "none")

HIST_p_ploss

HIST_p_profit <- ggplot() +
  geom_histogram(data=dft_p, mapping=aes(Profit_ha, fill = LandUse), binwidth = 500) +
  geom_vline(data = dft_p, mapping=aes(xintercept=mean(df_zero$Profit_ha)), colour="magenta", size=1) +
  scale_fill_grey(start = 0.2, end = 0.6) +
  theme_bw() +
  facet_grid(catch_target ~ LandUse, labeller = labeller(catch_target = target_p, LandUse = landuse)) +
  labs(x = "Profit $/ha", y = "Frequency") +
  theme(axis.title.x = element_text(face="bold", size=12),axis.text.x  = element_text(size=8)) +
  theme(axis.title.y = element_text(face="bold", size=12),axis.text.y  = element_text(size=8)) +
  theme(axis.line.x = element_line(color="black", size = 0.5),axis.line.y = element_line(color="black", size = 0.5)) +
  theme(legend.position = "none")

HIST_p_profit

# try another version of the histograms with a curve that smooths the histogram in this case a density histogram
# as it is below it is not working properly
# construct histogram by Landuse
#HIST_n_nleach_v2 <- ggplot(dft_n, aes(x=Nleach_ha)) +
#  geom_histogram(aes(y= ..density.., fill =LandUse), binwidth = 10) +
#  geom_vline(data = dft_n, mapping=aes(xintercept=mean(df_zero$Nleach_ha)), colour="magenta", size=1) +
#  scale_fill_grey(start = 0.2, end = 0.6) +
#  stat_function(fun=dnorm, color="red",args=list(mean=mean(dft_n$Nleach_ha),sd=sd(dft_n$Nleach_ha))) +
#  facet_grid(catch_target ~ LandUse, labeller = labeller(catch_target = target, LandUse = landuse)) +
#  labs(x = "N leaching kg/ha", y = "Density") +
#  theme(axis.title.x = element_text(face="bold", size=12),axis.text.x  = element_text(size=8)) +
#  theme(axis.title.y = element_text(face="bold", size=12),axis.text.y  = element_text(size=8)) +
#  theme(axis.line.x = element_line(color="black", size = 0.5),axis.line.y = element_line(color="black", size = 0.5)) +
#  theme(legend.position = "none")

#HIST_n_nleach_v2

# load the overview file Marc generated for the nutrientcap optimization
df_new_noc <- read.csv("nutrientcap_overview_dataset_usethis_3Nov2020.csv", header = TRUE, sep = ",")

# create new variables for N and P % reduction
df_new_noc <- mutate(df_new_noc, N_red_perc = N_reduction*100, P_red_perc = P_reduction*100)

# add categorical variable based on N or P or Both focus
N <- c("N:30","N:35","N:40","N:45","N:50")
P <- c("P:0.6","P:0.8","P:1.0","P:1.2","P:1.4")
N_Pchange <- c("N:40 P:0.6","N:40 P:0.8","N:40 P:1.0","N:40 P:1.2","N:40 P:1.4")
P_Nchange <- c("N:30 P:1.0","N:35 P:1.0","N:40 P:1.0","N:45 P:1.0","N:50 P:1.0")

temp1 <- filter(df_new_noc, scenario %in% N)
temp2 <- filter(df_new_noc, scenario %in% P)
temp3 <- filter(df_new_noc, scenario %in% N_Pchange)
temp4 <- filter(df_new_noc, scenario %in% P_Nchange)

temp <- rbind(temp1 %>% mutate(Cap ="N"),temp2 %>% mutate(Cap = "P"), temp3 %>% mutate(Cap = "N_Pchange"),
              temp4 %>% mutate(Cap = "P_Nchange"))
temp$Cap <- as.factor(temp$Cap)

# construct plots using temp data frame
df_new_noc_nfocus <- melt(temp, id.vars = c("Cap","scenario","Ebit_red"), measure.vars = c("N_red_perc","P_red_perc"))
MAC_cap_nfocus <- ggplot(subset(df_new_noc_nfocus, Cap %in% c("N")), aes(value, Ebit_red)) +
  geom_line(aes(lty = variable)) +
  guides(colour = "legend", lty = "none") +
  geom_point(aes(shape = scenario), size=2) +
  labs(x = NULL, y = "Profit reduction %") +
  xlim(0,50) +
  theme_classic() +
  theme_linedraw() +
  theme_light() +
  theme(axis.title.x = element_text(face="bold", size=10),axis.text.x  = element_text(size=8)) +
  theme(axis.title.y = element_text(face="bold", size=10),axis.text.y  = element_text(size=8)) +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
  theme(legend.position = "right", legend.title = NULL, legend.text = element_text(size = 8)) +
#  theme(legend.position = c(0.85,0.2)) +
  labs(shape = NULL, lty = NULL) +
  #scale_linetype_discrete(labels = c("N","P")) +
  scale_shape_discrete(labels = c("N30","N35","N40","N45","N50"))
  
MAC_cap_nfocus

MAC_cap_pfocus <- ggplot(subset(df_new_noc_nfocus, Cap %in% c("P")), aes(value, Ebit_red)) +
  geom_line(aes(lty = variable)) +
  guides(colour = "legend", lty = "none") +
  geom_point(aes(shape = scenario), size=2) +
  labs(x = NULL, y = NULL) +
  xlim(0,50) +
  theme_classic() +
  theme_linedraw() +
  theme_light() +
  theme(axis.title.x = element_text(face="bold", size=10),axis.text.x  = element_text(size=8)) +
  theme(axis.title.y = element_text(face="bold", size=10),axis.text.y  = element_text(size=8)) +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
  theme(legend.position = "right", legend.title = NULL, legend.text = element_text(size = 8)) +
  #  theme(legend.position = c(0.85,0.2)) +
  labs(shape = NULL, lty = NULL) +
  #scale_linetype_discrete(labels = c("N","P")) +
  scale_shape_discrete(labels = c("P0.6","P0.8","P1.0","P1.2","P1.4"))

MAC_cap_pfocus

MAC_cap_N_Pchange <- ggplot(subset(df_new_noc_nfocus, Cap %in% c("N_Pchange")), aes(value, Ebit_red)) +
  geom_line(aes(lty = variable)) +
  guides(colour = "legend", lty = "none") +
  geom_point(aes(colour = scenario), size=2) +
  labs(x = "Nutrient reduction %", y = NULL) +
  xlim(0,50) +
  theme_classic() +
  theme_linedraw() +
  theme_light() +
  theme(axis.title.x = element_text(face="bold", size=10),axis.text.x  = element_text(size=8)) +
  theme(axis.title.y = element_text(face="bold", size=10),axis.text.y  = element_text(size=8)) +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
  theme(legend.position = "right", legend.title = NULL, legend.text = element_text(size = 8)) +
  #  theme(legend.position = c(0.85,0.2)) +
  labs(colour = NULL, lty = NULL) 
  #scale_linetype_discrete(labels = c("N","P")) 
  #scale_colour_discrete(labels = c("N:30 P:1.0","N:35 P:1.0","N:40 P:1.0","N:45 P:1.0","N:50 P:1.0","N:40 P:0.6","N:40 P:0.8","N:40 P:1.2","N:40 P:1.4"))

MAC_cap_N_Pchange

MAC_cap_P_Nchange <- ggplot(subset(df_new_noc_nfocus, Cap %in% c("P_Nchange")), aes(value, Ebit_red)) +
  geom_line(aes(lty = variable)) +
  guides(colour = "legend", lty = "none") +
  geom_point(aes(colour = scenario), size=2) +
  labs(x = "Nutrient reduction %", y = "Profit reduction %") +
  xlim(0,50) +
  theme_classic() +
  theme_linedraw() +
  theme_light() +
  theme(axis.title.x = element_text(face="bold", size=10),axis.text.x  = element_text(size=8)) +
  theme(axis.title.y = element_text(face="bold", size=10),axis.text.y  = element_text(size=8)) +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
  theme(legend.position = "right", legend.title = NULL, legend.text = element_text(size = 8)) +
  #  theme(legend.position = c(0.85,0.2)) +
  labs(colour = NULL, lty = NULL) 
  #scale_linetype_discrete(labels = c("N","P")) 
#scale_colour_discrete(labels = c("N:30 P:1.0","N:35 P:1.0","N:40 P:1.0","N:45 P:1.0","N:50 P:1.0","N:40 P:0.6","N:40 P:0.8","N:40 P:1.2","N:40 P:1.4"))

MAC_cap_P_Nchange

mac_cap <- MAC_cap_nfocus + MAC_cap_pfocus + MAC_cap_P_Nchange + MAC_cap_N_Pchange + plot_layout(widths = c(1, 1)) +
  plot_annotation(tag_levels = list(c("Cap N focus", "Cap P focus","Cap N sensitivity","Cap P sensitivity"))) &
  theme(plot.tag.position = c(0.2, 1),
        plot.tag = element_text(size = 10, hjust = 0, vjust = -0.2))

mac_cap

# reconstruct the farm-scale marginal abatement curves used as input to the Optimiser Module i.e. Fig 2 in 
# the manuscript
df_farm_macs <- read.csv("dairy_macs.csv", header = TRUE)
df_farm_macs <- mutate(df_farm_macs, n_red_ = n_red_perc * 100, op_red_ = op_red_perc * 100)

#too many lines on one graph. This is meant to give the reader a feel only so remove dairy_5 and dairy_6
df_farm_macs <- filter(df_farm_macs, type %in% c("dairy_1", "dairy_2", "dairy_3", "dairy_4"))

farm_macs <- ggplot(df_farm_macs, aes(n_red_, op_red_)) +
  geom_line(aes(lty = type)) +
  geom_point(aes(shape = type), size=2) +
  guides(shape = "legend", lty = "none") +
  labs(x = "% reduction in farm N loss", y = "%reduction in farm OP") +
  theme_classic() +
  theme_linedraw() +
  theme_light() +
  theme(axis.title.x = element_text(face="bold", size=10),axis.text.x  = element_text(size=8)) +
  theme(axis.title.y = element_text(face="bold", size=10),axis.text.y  = element_text(size=8)) +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
  theme(legend.position = "right", legend.title = NULL, legend.text = element_text(size = 8)) +
  #  theme(legend.position = c(0.85,0.2)) +
  labs(shape = "Farm typology", lty = NULL) 
#scale_linetype_discrete(labels = c("N","P")) 
#scale_colour_discrete(labels = c("N:30 P:1.0","N:35 P:1.0","N:40 P:1.0","N:45 P:1.0","N:50 P:1.0","N:40 P:0.6","N:40 P:0.8","N:40 P:1.2","N:40 P:1.4"))

farm_macs
