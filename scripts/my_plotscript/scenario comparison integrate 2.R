
# ---
# title: "R Notebook"
# output:
#   html_document: default
#   html_notebook: default
#   pdf_document: default
# ---
# 
# 
# # Data analysis and plotting using quitte
# 
# Contact: Chris Gong
# 
# ## Load necessary libraries
# ---
# title: "R Notebook"
# output:
#   html_document: default
#   html_notebook: default
#   pdf_document: default
# ---


require(dplyr)
require(tidyr)
require(ggplot2)
require(quitte)
require(mip)
require(purrr)

require(plotly) #install.packages("plotly")
library(magick) #install.packages("magick")
library(magrittr) #install.packages("magrittr")
require(stringr)
# library(ggpattern)

# library(cowplot)  # Useful for themes and for arranging plots
# library(metR)  # Useful for contour_fill
# library(scales)

###################### Plot styles######################################
options(warn=-1)
# demand
tt = plotstyle.add(c("Emi|CO2|Energy|Demand|CDR")   ,c("CDR"),  c("#00cc99"), replace = T)
tt = plotstyle.add(c("Emi|CO2|Energy|Demand|Industry")   ,c("Industry"),  c("#7777ff"),replace=T)
tt = plotstyle.add(c("Emi|CO2|Energy|Demand|Transport")   ,c("Transport"),  c("#440077"), replace = T)
tt = plotstyle.add(c("Emi|CO2|Energy|Demand|Buildings")   ,c("Buildings"),  c("#4444bb"), replace = T)
# supply
tt = plotstyle.add(c("Emi|CO2|Energy|Supply|Electricity w/ couple prod")   ,c("Elec. Supply"),  c("#ffaa00"),replace=T)
tt = plotstyle.add(c("Emi|CO2|Gross|Energy|Supply|Electricity")   ,c("Elec. Supply"),  c("#ffaa00"),replace=T)

tt = plotstyle.add(c("Emi|CO2|Energy|Supply|Non-electric")   ,c("Non Elec. Supply"),  c("#aa0000"),replace=T)

#misc
tt = plotstyle.add(c("Emi|CO2|non-BECCS CDR")   ,c("non BECCS CDR"),  c("#116611"),replace=T)

# tt = plotstyle.add(c("Emi|CO2|CDR|BECCS")   ,c("BECCS"),  c("#FF337F"), replace = T)
tt = plotstyle.add(c("Emi|CO2|Land-Use Change")   ,c("Land-use change"),  c("#919100"),replace=T)
tt = plotstyle.add(c("Emi|CO2|Industrial Processes")   ,c("Industrial Processes"),  c("#10bfcc"),replace=T)


tt = plotstyle.add(c("FE|Transport|Electricity")   ,"Transport|Elec" ,  c("#ffb200"),replace=T)
tt = plotstyle.add(c("FE|Buildings|Electricity")   ,"Buildings|Elec",  c("#ffcc55"),replace=T)
tt = plotstyle.add(c("FE|Industry|Electricity")    ,"Industry|Elec",  c("#fa8c02"),replace=T)

tt = plotstyle.add(c("FE|Transport|Hydrogen")   ,"Transport|Hydrogen" ,  c("#46fc02"),replace=T)
tt = plotstyle.add(c("FE|Buildings|Hydrogen")   ,"Buildings|Hydrogen",  c("#94ff6c"),replace=T)
tt = plotstyle.add(c("FE|Industry|Hydrogen")    ,"Industry|Hydrogen",  c("#2b7c0d"),replace=T)

tt = plotstyle.add(c("FE|Buildings|Heat")   ,"Buildings|Heat",  c("#ff7a59"),replace=T)
tt = plotstyle.add(c("FE|Industry|Heat")    ,"Industry|Heat",  c("#b92400"),replace=T)

tt = plotstyle.add(c("FE|Transport|Gases")   ,"Transport|Gases" ,  c("#b200ff"),replace=T)
tt = plotstyle.add(c("FE|Buildings|Gases")   ,"Buildings|Gases",  c("#c875ec"),replace=T)
tt = plotstyle.add(c("FE|Industry|Gases")    ,"Industry|Gases",  c("#610d85"),replace=T)

tt = plotstyle.add(c("FE|Transport|Liquids")   ,"Transport|Liquids" ,  c("#0f30ec"),replace=T)
tt = plotstyle.add(c("FE|Buildings|Liquids")   ,"Buildings|Liquids",  c("#7e8fef"),replace=T)
tt = plotstyle.add(c("FE|Industry|Liquids")    ,"Industry|Liquids",  c("#04167c"),replace=T)

tt = plotstyle.add(c("FE|Buildings|Solids")   ,"Buildings|Solids", c("#a5a5a5"),replace=T)
tt = plotstyle.add(c("FE|Industry|Solids")    ,"Industry|Solids",   c("#1b1b1b"),replace=T)

tt = plotstyle.add(c("Electricity")    ,"Electricity",   c("#990000"),replace=T)
tt = plotstyle.add(c("Heat")    ,"Heat",   c("#dd7700"),replace=T)

tt = plotstyle.add(c("SE|Hydrogen|Electricity")    ,"H2|Electrolysis",   c("#ffdd44"),replace=T)
tt = plotstyle.add(c("SE|Hydrogen|Biomass")    ,"H2|Bio",   c("#33AACC"),replace=T)
tt = plotstyle.add(c("SE|Hydrogen|Fossil")    ,"H2|Fossil",   c("#1133EE"),replace=T)
tt = plotstyle.add(c("SE|Heat|Geothermal")    ,"Heat|Heat Pump",   c("#e51900"),replace=T)
tt = plotstyle.add(c("SE|Heat|Biomass")    ,"Heat|Bio",   c("#cc5555"),replace=T)
tt = plotstyle.add(c("SE|Heat|Fossil")    ,"Heat|Fossil",   c("#cc0000"),replace=T)


tt = plotstyle.add(c("SE|Heat|Biomass")    ,"Heat|Bio",   c("#66cc11"),replace=T)
tt = plotstyle.add(c("SE|Hydrogen|Biomass")    ,"H2|Bio",   c("#55EEEE"),replace=T)
tt = plotstyle.add(c("SE|Gases|Biomass")    ,"Gases|Bio",   c("#33ff00"),replace=T)
tt = plotstyle.add(c("SE|Liquids|Biomass")    ,"Liquids|Bio",   c("#11AA22"),replace=T)
tt = plotstyle.add(c("SE|Solids|Biomass")    ,"Solids|Bio",   c("#005900"),replace=T)

tt = plotstyle.add(c("SE|Heat|Fossil")    ,"Heat|Fossil",   c("#996666"),replace=T)
tt = plotstyle.add(c("SE|Hydrogen|Fossil")    ,"H2|Fossil",   c("#666699"),replace=T)
tt = plotstyle.add(c("SE|Gases|Fossil")    ,"Gases|Fossil",   c("#444444"),replace=T)
tt = plotstyle.add(c("SE|Liquids|Fossil")    ,"Liquids|Fossil",   c("#553022"),replace=T)
tt = plotstyle.add(c("SE|Solids|Coal")    ,"Solids|Coal",   c("#0c0c0c"),replace=T)

options(warn=0)
###################### Scenario names #####################################

setwd('~/source/REMIND_integrate/remind/output')
run_number = "standalone_v22"

data.dir = paste0("./",run_number)

plots.path = paste0("./plots/", run_number,"/")
dir.create(plots.path, showWarnings = FALSE)

regs = c("World", "CHA", "EUR")
t.barplot <- c(2015, 2020, 2030, 2040, 2050, 2060)

scenarios = c(
    "REMIND_generic_elec_INT_1150_po_plateau30",
    "REMIND_generic_elec_INT_1150_po_plateau25",
    "REMIND_generic_elec_INT_1150_po_slope_slo",
    "REMIND_generic_elec_INT_1150_po_slope_med",
    "REMIND_generic_elec_INT_1150_po_slope_fast",
    # "REMIND_generic_elec_INT_1150_po_plateau30_adj",
    # "REMIND_generic_elec_INT_1150_po_slope_fast_adj",
    NULL)

scens_short =
  c("2C plateau 30",
    "2C plateau 25",
    "2C slow",
    "2C medium",
    "2C fast",
    # "2C plateau 30 adj",
    # "2C fast adj",
    NULL)

default_scen = "2C plateau 30"

data.files = lapply(scenarios, function(x) paste0( data.dir, "/", x, "_withoutPlus.mif")) %>% unlist
# data.files = lapply(scenarios, function(x) paste0( data.dir, "/", x, ".mif")) %>% unlist

run_indices = seq(from = 1, to = length(data.files), by = 1)
df.raw = NULL

# trim df to chosen scenarios and regions
for (i in run_indices){
  print(i)
  # REMIND.data = read.quitte(data.files[[1]])
  REMIND.data = read.quitte(data.files[[i]])
  
  df.raw <- rbind(df.raw, REMIND.data)
}

# scens1 = stringr::str_replace(scenarios, "_withoutPlus", "")
scens0 = stringr::str_replace(scenarios, "REMIND_generic_", "")
scens = stringr::str_replace(scens0, "-rem-5", "")

df =  filter(df.raw, scenario %in% scens, region %in% regs ) %>%
  order.levels(scenario = scens) %>%
  factor.data.frame()

rm(df.raw)

######## Tsinghua mif #########################

data.TH = read.quitte(paste0("C-GEM_CHA_NZ.mif"))

###################### Historical mif ######################################

tmax = 2060

# also read in historical data
df.hist0 = read.quitte(paste0("./",run_number, "/historical.mif"))

df.hist = filter(df.hist0, model =="CEDS", region %in% regs ) %>%
  factor.data.frame() 

df.hist_FE = filter(df.hist0, model =="IEA", region %in% regs ) %>%
  factor.data.frame() %>% 
  filter(variable %in% c("FE", "FE|Industry", "FE|Building", "FE|Transport"))

df.hist.emi = df.hist0 %>%
  filter(model =="CEDS", region %in% c("CHA") ) %>% 
  filter(variable == "Emi|CO2|Energy and Industrial Processes") %>%
  mutate(value = value/1e3) %>% 
  select(-scenario)

rm(df.hist0)

###################### Rescale price ######################
# rescale price and cost data to 2020 using 2005->2020 deflator of 1.34 (https://stats.oecd.org/Index.aspx?DataSetCode=PRICES_CPI)

indices = grepl("US\\$2005",df$unit)
df[indices, "value"] = df[indices, "value"] * 1.52
levels(df$unit) = gsub("US\\$2005", "US\\$2020", levels(df$unit))

df$scenario <- plyr::mapvalues(df$scenario, from = scens, to = scens_short)

###################### Color ######################################
# reg = "World"
# limy = c(-10,45)
reg = "CHA"
limy = c(-2,13)

# scens = scens_short

cbbPalette <- c( "#56B4E9", "#009E73", "#D55E00", "#0072B2", "#F0E442","#E69F00", "#CC79A7", "#FF9999", "#9966FF", "#999900")
# mycolors = c(scens[[1]] = cbbPalette[[1]])

library(RColorBrewer)
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(7)[c(1,3,4)]
######################################################################################################
######################################################################################################

###################### emission plot ##############################
var.emi.mapping <- c("Emi|CO2|Industrial Processes" = "Industrial Processes",
                     "Emi|CO2|Gross|Energy|Supply|Non-electric" = "Non-elec. Supply",
                     "Emi|CO2|Energy|Demand|Industry" = "Industry",
                     "Emi|CO2|Energy|Demand|Buildings" = "Buildings",
                     "Emi|CO2|Energy|Demand|Transport" = "Transport",
                     "Emi|CO2|Energy|Supply|Electricity w/ couple prod" = "Elec. Supply",
                     "Emi|CO2|CDR|BECCS" = "BECCS",
                     "Emi|CO2|CDR|DACCS" = "DAC",
                     "Emi|CO2|CDR|EW" = "EW",
                     "Emi|CO2|Land-Use Change" = "Land-Use Change",
                     NULL)

color.emi.mapping <- c("Industrial Processes" = "palevioletred",
                       "Elec. Supply" = "darkgoldenrod",
                       "Non-elec. Supply" = "darkorchid",
                       "Land-Use Change" = "darkgreen",
                       "Carbon Sink" = "#8bb07f",
                       "Buildings" = "darkred",
                       "Transport" = "darkblue",
                       "Industry" = "grey",
                       "DAC" = "cyan",
                       "BECCS" = "lightgreen",
                       "EW" = "sandybrown",
                       NULL)

plot.period <- seq(2015,2060,5)

df.emi.grossneg <- df %>%
  filter(variable %in% names(var.emi.mapping), period %in% plot.period, region == "CHA") %>%
  order.levels(variable = names(var.emi.mapping)) %>% 
  revalue.levels(variable = var.emi.mapping) 
  
df.emiNet.tot <- df %>%
  filter(variable == "Emi|CO2", period %in% plot.period, region == "CHA") %>%
  mutate(variable = "Total")

p.emi.grossneg <- ggplot() +
  geom_col(data=df.emi.grossneg, aes(period, value, fill=variable), alpha=0.8) +
  geom_line(data=df.emiNet.tot, aes(period, value, linetype=variable), size=1.2, alpha=0.7) +
  geom_point(data=df.emiNet.tot, aes(period, value, shape=variable), size=1.5, alpha=0.7) +
  facet_wrap(~scenario, ncol = length(getScenarios(df))) + 
  scale_y_continuous("CO2 Emissions (Mt CO2/yr)") + 
  scale_fill_manual(name = "", values = color.emi.mapping) +
  geom_hline(yintercept=0, size=0.8, linetype="dashed", alpha=0.7) + 
  theme_bw() +
  theme( axis.text.x = element_text(angle=90, vjust = 0.5),
         legend.position = "bottom",strip.background = element_blank(),
         legend.title = element_blank(),strip.text.y = element_text(angle = 0, hjust = 0))

  p.emi.grossneg <- p.emi.grossneg +
    geom_line(data=df.hist.emi %>% 
                filter(period > 2000) %>% 
                filter(period < 2060) %>% 
                mutate(value = value *1e3), aes(period, value, color=model), size=1.2, alpha=0.7)+
  guides(linetype=guide_legend(nrow=3,byrow=TRUE),fill=guide_legend(nrow=3,byrow=TRUE)) 
    
ggsave(plot = p.emi.grossneg, filename = paste0(plots.path,  "/", run_number, "Emi_CO2GrossNeg.png"), width=20, height=10, units = "cm")

#### with Tsinghua
var.emi.mapping.TH <- c("Emi|CO2|Industrial Processes" = "Industrial Processes",
                     "Emi|CO2|DAC" = "DAC",
                     "Emi|CO2|Electricity" = "Elec. Supply",
                     "Emi|CO2|Carbon sink" = "Carbon Sink",
                     "Emi|CO2|Building" = "Buildings",
                     "Emi|CO2|Transport" = "Transport",
                     "Emi|CO2|Industry (energy activity)" = "Industry",
                     "Emi|CO2|Others" = "Others",
                     NULL)

plot.period <- seq(2020,2060,5)

df.emi.grossneg_TH <- data.TH %>%
  filter(variable %in% names(var.emi.mapping.TH)) %>%
  order.levels(variable = names(var.emi.mapping.TH)) %>% 
  revalue.levels(variable = var.emi.mapping.TH) 

df.emiNet.tot_TH <- data.TH %>%
  filter(variable == "Emi|GHG|CO2 (energy activity + industrial process)") %>%
  mutate(variable = "Total")

df.emi.grossneg_allmodels =  list(df.emi.grossneg, df.emi.grossneg_TH) %>%
  reduce(full_join) 

p.emi.grossneg_allmodels <- ggplot() +
  geom_col(data=df.emi.grossneg_allmodels, aes(period, value, fill=variable), alpha=0.8) +
  geom_line(data=df.emiNet.tot_TH, aes(period, value, linetype=variable), size=1.2, alpha=0.7) +
  geom_point(data=df.emiNet.tot_TH, aes(period, value, shape=variable), size=1.5, alpha=0.7) +
  geom_line(data=df.emiNet.tot, aes(period, value, linetype=variable), size=1.2, alpha=0.7) +
  geom_point(data=df.emiNet.tot, aes(period, value, shape=variable), size=1.5, alpha=0.7) +
  facet_wrap(~scenario, ncol = length(getScenarios(df))) +
  scale_y_continuous("CO2 Emissions (Mt CO2/yr)") +
  scale_fill_manual(name = "", values = color.emi.mapping) +
  geom_hline(yintercept=0, size=0.8, linetype="dashed", alpha=0.7) +
  theme_bw() +
  theme( axis.text.x = element_text(angle=90, vjust = 0.5),
         legend.position = "bottom",strip.background = element_blank(),
         legend.title = element_blank(),strip.text.y = element_text(angle = 0, hjust = 0))+ 
  theme(axis.text=element_text(size=10), axis.title=element_text(size=10, face="bold"), strip.text = element_text(size = 10)) +
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(),legend.text = element_text(size=10)) 

p.emi.grossneg_allmodels <- p.emi.grossneg_allmodels +
  geom_line(data=df.hist.emi %>% 
              filter(period > 2000) %>% 
              filter(period < 2060) %>% 
              mutate(value = value *1e3), aes(period, value, color=model), size=1.2, alpha=0.7)+
  guides(linetype=guide_legend(nrow=3,byrow=TRUE),fill=guide_legend(nrow=3,byrow=TRUE)) 


ggsave(plot = p.emi.grossneg_allmodels, filename = paste0(plots.path,  "/", run_number, "Emi_CO2GrossNeg_allmodels.png"), width=23, height=16, units = "cm")

###################### Emi CO2 sector ######################################

  vars = c(
  # energy|supply:
    "Emi|CO2|Energy|Supply|Non-electric",
    "Emi|CO2|Energy|Supply|Electricity w/ couple prod" ,
    # energy|demand: 
    "Emi|CO2|Energy|Demand|Industry" ,
    "Emi|CO2|Energy|Demand|Buildings",
    "Emi|CO2|Energy|Demand|Transport",
    "Emi|CO2|Energy|Demand|CDR",
    
    "Emi|CO2|Industrial Processes",
    "Emi|CO2|Land-Use Change",
    "Emi|CO2|non-BECCS CDR"
    # ,
    # "Emi|CO2|CDR|BECCS"
    )

vars = c(     "Emi|CO2|Gross|Energy|Supply|Electricity",
              "Emi|CO2|Gross|Energy|Supply|Non-electric",
              
              "Emi|CO2|Energy|Demand|Buildings",
              "Emi|CO2|Energy|Demand|Transport",
              "Emi|CO2|Industrial Processes",
              "Emi|CO2|Energy|Demand|Industry",
              "Emi|CO2|Land-Use Change",
              "Emi|CO2|CDR|BECCS",
              "Emi|CO2|CDR|DACCS",
              "Emi|CO2|CDR|EW")

df.tot.hist = filter(df.hist, variable == "Emi|CO2",model == "CEDS", region == reg) %>%
  mutate(value = value/1000) %>%
  select(-scenario)

df.tot.hist.regcheck = filter(df.hist, variable == "Emi|CO2",model == "CEDS", region == "CHA") %>%
  mutate(value = value/1000) %>%
  select(-scenario)

df.tot.hist.regs = filter(df.hist, variable == "Emi|CO2",model == "CEDS") %>%
  mutate(value = value/1000) %>%
  select(-scenario)

df.tot.histcheck = filter(df.hist, grepl("Emi|CO2", variable, fixed=TRUE), period > 2005, model == "CEDS", region == reg) %>%
  mutate(value = value/1000) %>%
  select(-scenario)

df.plot = filter(df, variable %in% vars,scenario %in% scens_short) %>%
    order.levels(scenario = scens_short, variable = vars) %>%
  filter(period >= 2010, period <= 2080) %>%
  mutate(value = value/1000) %>%
  factor.data.frame()

df.sector_emi_check <- df.plot %>% 
  filter(scenario == default_scen,region == "CHA")
  
df.BECCS.check = filter(df.plot, grepl("Emi|CO2", variable, fixed=TRUE), period > 2005, period < 2055, scenario == "1.5C netzero BECCS", region == reg) %>% 
  dplyr::summarise( value = sum(value) )

df.1.5.check = filter(df.plot, grepl("Emi|CO2", variable, fixed=TRUE), period > 2005, period < 2065, scenario == default_scen, region == reg)  

df.2.check = filter(df.plot, grepl("Emi|CO2", variable, fixed=TRUE), period > 2005, period < 2055, scenario == "2C netzero bal", region == reg)  

df.emiCO2gross.plot =  filter(df, period <= 2080, scenario %in% scens_short) %>%
  # filter(variable == "Emi|CO2") %>% 
  filter(variable == "Emi|CO2|Gross|Energy and Industrial Processes") %>% 
  mutate(value = value/1000) %>%
  factor.data.frame()

df.linecomparison = df.emiCO2gross.plot %>% filter(period <= 2080, scenario %in% scens_short, region == reg)

levels(df.linecomparison$scenario) <- scens_short

if (reg == "CHA"){
  df.tsinhua_CGEM <- data.frame(period=seq(from = 2020, to = 2070, by = 5), 
                                value=c(11.2,11.5,11.0,9.5,7.2,4.9,3.1,1.6,0.73,0.73,0.73), scenario = "C-GEM 2060NZ") 
  
  df.linecomparison2 = list(df.linecomparison, df.tsinhua_CGEM) %>%
    reduce(full_join) 
  
  # levels(df.linecomparison2$scenario) <- c(scens_short, "2060CN")

  p<-ggplot() + geom_line(data= df.linecomparison2, aes(x=period, y = value, color = scenario)) +
  ylab("Gross CO2 Emissions [GtCO2/yr]") +
  theme_bw() +
  theme(strip.background = element_blank()) +
  geom_hline(yintercept = 0) +
    coord_cartesian(xlim = c(2010,2080),ylim = limy)+
  geom_line(data= df.hist.emi, aes(x=period, y = value), color = "black", size =1 )+
    scale_color_manual(values = cbbPalette)

ggsave(filename = paste0(plots.path,"/", run_number,"_",reg, "_", "GrossEmiCO2_lines_TsingHua.png"),width=18, height=10, units = "cm")
}
############### net co2 emi
df.tot =  filter(df, variable ==  "Emi|CO2", period <= 2080, scenario %in% scens_short) %>%
  mutate(value = value/1000) %>%
  factor.data.frame()

# peak budget
# between years
df.tot_sum1 <- df.tot %>% 
  filter(period > 2020, period < 2060) %>%
  select(scenario, value, period, region) %>% 
  filter(region == "CHA") %>% 
  dplyr::group_by(scenario, region) %>%
  dplyr::summarise( value = sum(value) *5)%>% 
  dplyr::ungroup()

# end year
df.tot_sum2 <- df.tot %>% 
  filter(period %in% c(2060)) %>% 
  select(scenario, value, period, region) %>% 
  filter(region == "CHA") %>% 
  mutate(value2 = value * 7.5)%>% 
  select(scenario, value2, region)

# start year
df.tot_sum3 <- df.tot %>% 
  filter(period %in% c(2020)) %>% 
  select(scenario, value, period, region) %>% 
  filter(region == "CHA") %>% 
  mutate(value3 = value * 2.5)%>% 
  select(scenario, value3, region)

df.tot_sum = list(df.tot_sum1, df.tot_sum2,df.tot_sum3) %>%
  reduce(full_join) %>% 
  mutate(budget = value + value2 +value3)

df.linecomparison.net = df.tot %>% filter(period <= 2080, scenario %in% scens_short, region == reg)

levels(df.linecomparison.net$scenario) <- scens_short

if (reg == "CHA"){
  df.tsinhua_CGEM <- data.frame(period=seq(from = 2020, to = 2070, by = 5), 
                                value=c(11.2,11.5,11.0,9.5,7.2,4.9,3.1,1.6,0,0,0), scenario = "C-GEM 2060NZ") 
  df.linecomparison.net2 = list(df.linecomparison.net, df.tsinhua_CGEM) %>%
    reduce(full_join) 
  
  # levels(df.linecomparison2$scenario) <- c(scens_short, "2060CN")
  
  p<-ggplot() + geom_line(data= df.linecomparison.net2, aes(x=period, y = value, color = scenario)) +
    ylab("Net CO2 Emissions [GtCO2/yr]") +
    theme_bw() +
    theme(strip.background = element_blank()) +
    geom_hline(yintercept = 0) +
    coord_cartesian(xlim = c(2010,2080),ylim = limy)+
    geom_line(data= df.hist.emi, aes(x=period, y = value), color = "black", size =1 )+
    scale_color_manual(values = cbbPalette)
  
  ggsave(filename = paste0(plots.path,"/", run_number,"_",reg, "_", "NetEmiCO2_lines_TsingHua.png"),width=18, height=10, units = "cm")
}

#####################################################################################################

data.files = lapply(scenarios, function(x) paste0( data.dir, "/", x, ".mif")) %>% unlist

run_indices = seq(from = 1, to = length(data.files), by = 1)
df.raw = NULL

# trim df to chosen scenarios and regions
for (i in run_indices){
  print(i)
  # REMIND.data = read.quitte(data.files[[9]])
  REMIND.data = read.quitte(data.files[[i]])
  
  df.raw <- rbind(df.raw, REMIND.data)
}

scens0 = stringr::str_replace(scenarios, "REMIND_generic_", "")
scens = stringr::str_replace(scens0, "-rem-5", "")

df.wp =  filter(df.raw, scenario %in% scens, region %in% regs ) %>%
  order.levels(scenario = scens) %>%
  factor.data.frame()

rm(df.raw)

indices = grepl("US\\$2005",df.wp$unit)
df.wp[indices, "value"] = df.wp[indices, "value"] * 1.52
levels(df.wp$unit) = gsub("US\\$2005", "US\\$2020", levels(df.wp$unit))

df.wp$scenario <- plyr::mapvalues(df.wp$scenario, from = scens, to = scens_short)

# calculate other fossil (coal + oil) and other renewables categories for electricity generation shown in some plots
df.wp <- df.wp %>%
  calc_addVariable("`SE|Electricity|+|Other Renewables`" = "`SE|Electricity|+|Biomass`+`SE|Electricity|Solar|+|CSP`+`SE|Electricity|+|Hydro`+`SE|Electricity|+|Geothermal`",
                   "`SE|Electricity|+|Other Fossil`" =  "`SE|Electricity|Coal|+|w/ CC` + `SE|Electricity|Coal|+|w/o CC` + `SE|Electricity|+|Oil`",
                   units = "EJ/yr")

#####################################################################################################
#####################################################################################################

df_test = filter(df, grepl("SE|Electricity|", variable, fixed=TRUE))

# SE Electricity Supply and Demand Mix
### plot with positive supply and negative demand bars
elsup.focus.vars <- c("SE|Electricity|+|Wind",
                      "SE|Electricity|+|Solar",
                      "SE|Electricity|+|Hydrogen",
                      "SE|Electricity|+|Gas",
                      "SE|Electricity|+|Other Fossil",
                      "SE|Electricity|+|Other Renewables",
                      "SE|Electricity|+|Nuclear",
                      NULL)

eldem.focus.vars <- c("SE|Input|Electricity|Self Consumption Energy System",
                      "SE|Input|Electricity|CDR",
                      "SE|Input|Electricity|Buildings",
                      "SE|Input|Electricity|Industry",
                      "SE|Input|Electricity|Transport",
                      "SE|Input|Electricity|Hydrogen|+|Standard Electrolysis",
                      "SE|Input|Electricity|Hydrogen|+|VRE Storage",
                      "SE|Input|Electricity|Hydrogen|Synthetic Fuels|+|Liquids",
                      "SE|Input|Electricity|Hydrogen|Synthetic Fuels|+|Gases")

el.sink.source.label <- c("SE|Electricity|+|Wind" = "Wind",
                          "SE|Electricity|+|Solar" = "PV",
                          "SE|Electricity|+|Hydrogen" = "Hydrogen",
                          "SE|Electricity|+|Gas" = "Gas",
                          "SE|Electricity|+|Other Fossil" = "Other Fossil",
                          "SE|Electricity|+|Other Renewables" = "Other Renewables",
                          "SE|Electricity|+|Nuclear" = "Nuclear",
                          "SE|Input|Electricity|Self Consumption Energy System" = "for own consumption",
                          "SE|Input|Electricity|Hydrogen|+|Standard Electrolysis" = "for direct H2",
                          "SE|Input|Electricity|Hydrogen|+|VRE Storage" = "for H2 storage",
                          "SE|Input|Electricity|CDR" = "for CDR",
                          "SE|Input|Electricity|Buildings" = "for Buildings",
                          "SE|Input|Electricity|Industry" = "for Industry",
                          "SE|Input|Electricity|Transport" = "for Transport",
                          "SE|Input|Electricity|Hydrogen|Synthetic Fuels|+|Liquids" = "for synthetic liquids", 
                          "SE|Input|Electricity|Hydrogen|Synthetic Fuels|+|Gases" = "for synthetic gases")

el.sink.source.color <- c("Wind" = "deepskyblue1",
                          "PV" = "yellow",
                          "Hydrogen" = "cyan",
                          "Gas" = "grey80",
                          "Other Fossil" = "grey20",
                          "Other Renewables" = "darkgreen",
                          "Nuclear" = "darkorchid",
                          "for own consumption" = "darkgoldenrod",
                          "for direct H2" = "darkcyan",
                          "for H2 storage" = "mediumpurple1",
                          "for CDR" = "lightgreen",
                          "for Buildings" = "red",
                          "for Industry" = "grey50",
                          "for Transport" = "blue",
                          "for synthetic liquids" = "lightyellow3", 
                          "for synthetic gases" = "navajowhite1")

vars.order <- c("PV",
                "Wind",
                "Hydrogen",
                "Nuclear",
                "Other Renewables",
                "Gas",
                "Other Fossil",
                "for synthetic gases",
                "for synthetic liquids",
                "for H2 storage",
                "for direct H2",
                "for CDR",
                "for Buildings",
                "for Industry",
                "for Transport",
                "for own consumption")

df.el.sink.source <- df.wp %>% 
  filter(variable %in% elsup.focus.vars,
         scenario %in% scens_short) %>% 
  # bind demand variables with negative sign
  rbind( df.wp %>% 
           filter(variable %in% eldem.focus.vars,
                  scenario %in% scens_short) %>%
           # change sign an remove "SE|Electricity|used for"
           mutate( value = - value)) %>% 
  revalue.levels(variable = el.sink.source.label) %>% 
  order.levels(variable = vars.order, 
               scenario = scens_short) %>% 
  filter(period %in% plot.period)

for(reg in regs){
  scen = default_scen
  
  p.el.sink.source <- ggplot() +
    geom_col(data=df.el.sink.source%>% filter(region == reg)
             %>% filter(scenario == scen ), 
             aes(period, value, fill=variable), 
             alpha=.6, width = 3) +
    facet_wrap(~scenario, ncol=length(scens_short)) +
    scale_y_continuous("Electricity (EJ/yr)") +
    scale_fill_manual(values = el.sink.source.color) +
    theme_bw() +
    theme( axis.text.x = element_text(angle=90, vjust = 0.5), 
           legend.position = "bottom",
           strip.background = element_blank(),
           legend.title = element_blank(),
           strip.text.y = element_text(angle = 0, hjust = 0)) +
    guides(fill=guide_legend(ncol=4, direction = "horizontal"),color = guide_legend(nrow = 4, byrow = TRUE))
  
  ggsave(plot = p.el.sink.source,
         filename =  paste0(plots.path, "/", run_number,"_",reg, "_", scen, "_El_SinkSource.png"),
         width=12, height=16, units="cm")
}

# p.el.sink.source

for(reg in regs){
  scen = default_scen
  
  df.el.sink.source2 <-df.el.sink.source%>% mutate(value = value * 277.77)
  
  p.el.sink.source <- ggplot() +
    geom_col(data=df.el.sink.source%>% filter(region == reg)%>% mutate(value = value * 277.77)
             %>% filter(scenario == scen )
             , 
             aes(period, value, fill=variable), 
             alpha=.6, width = 3) +
    facet_wrap(~scenario, ncol=length(scens_short)) +
    scale_y_continuous("Electricity (TWh)") +
    scale_fill_manual(values = el.sink.source.color) +
    theme_bw() +
    theme( axis.text.x = element_text(angle=90, vjust = 0.5), 
           legend.position = "bottom",
           strip.background = element_blank(),
           legend.title = element_blank(),
           strip.text.y = element_text(angle = 0, hjust = 0)) +
    guides(fill=guide_legend(ncol=4, direction = "horizontal"),color = guide_legend(nrow = 4, byrow = TRUE))
  
  ggsave(plot = p.el.sink.source,
         filename =  paste0(plots.path, "/", run_number,"_",reg, "_", scen, "_El_SinkSource_TWh.png"),
         width=16, height=18, units="cm"
  )
}


##### compare to Tsinghua  
#### remind supply-side data

el.supp.label <- c("SE|Electricity|+|Wind" = "Wind",
                          "SE|Electricity|+|Solar" = "PV",
                          "SE|Electricity|+|Hydrogen" = "Hydrogen",
                          "SE|Electricity|+|Gas" = "Gas",
                          "SE|Electricity|Coal|+|w/ CC" = "Coal CCS",
                          "SE|Electricity|Coal|+|w/o CC" = "Coal",
                          "SE|Electricity|+|Oil" = "Oil",
                          "SE|Electricity|Biomass|+|w/ CC" = "BECCS",
                          "SE|Electricity|Biomass|+|w/o CC" = "Biomass",
                          "SE|Electricity|Solar|+|CSP" = "CSP",
                          "SE|Electricity|+|Hydro" = "Hydro",
                          "SE|Electricity|+|Geothermal" = "Geothermal",
                          "SE|Electricity|+|Nuclear" = "Nuclear",
                          # "SE|Electricity|Net Imports" = "Net Imports",
                          NULL)

vars.supp.order <- c("PV",
                "Wind",
                "Hydrogen",
                "Biomass",
                "BECCS",
                "CSP",
                "Hydro",
                "Geothermal",
                "Nuclear",
                "Coal CCS",
                "Coal",
                "Oil",
                "Gas",
                "Gas CCS",
                # "Net Imports",
                NULL)

el.supp.color <- c("Wind" = "deepskyblue1",
                          "PV" = "yellow",
                          "Hydrogen" = "cyan",
                          "Gas" = "grey80",
                          "Biomass" = "darkgreen",
                          "BECCS" = "green",
                          "CSP" = "#A05729",
                          "Hydro" = "blue",
                          "Geothermal" = "brown",
                          "Nuclear" = "darkorchid",
                          "Coal CCS" = "#fc6400",
                          "Coal" = "black",
                          "Oil" = "grey40",
                          "Gas"= "grey10",
                          "Gas CCS"= "#f2ba49",
                          # "Net Imports" = "darkslateblue",
                   NULL
)

df.el.remind.twh <- df.wp %>% 
  filter(region == "CHA") %>% 
  filter(variable %in% names(el.supp.label),
         scenario %in% scens_short) %>% 
  revalue.levels(variable = el.supp.label) %>% 
  order.levels(variable = vars.supp.order, 
               scenario = scens_short) %>% 
  filter(period %in% plot.period)%>%
  mutate(value = value * 277.77) %>% 
  select(-model,-region,-unit)

elsup.tsinghua.vars <- c(
                         "SE|Electricity|Coal" = "Coal",
                         "SE|Electricity|Gas" = "Gas",
                         "SE|Electricity|Hydro" = "Hydro",
                         "SE|Electricity|Nuclear" = "Nuclear",
                         "SE|Electricity|Wind" = "Wind",
                         "SE|Electricity|Solar" = "PV",
                         "SE|Electricity|Biomass" = "Biomass",
                         "SE|Electricity|Gas+CCS"= "Gas CCS",
                         "SE|Electricity|Coal+CCS" = "Coal CCS",
                         "SE|Electricity|BECCS" = "BECCS",
                         NULL)

df.el.TH.twh <- data.TH %>%
  filter(variable %in% names(elsup.tsinghua.vars)) %>%
  order.levels(variable = names(elsup.tsinghua.vars)) %>% 
  revalue.levels(variable = elsup.tsinghua.vars) %>% 
  mutate(value = 1e3*value) %>% 
  select(-model,-region,-unit)

plot.Diff <- list(df.el.TH.twh, df.el.remind.twh) %>%
  reduce(full_join)
  
scens_short_TH <- c(scens_short, "C-GEM 2060NZ")

plot.Diff <- plot.Diff %>%
  mutate(variable = factor(variable, levels=rev(unique(vars.supp.order)))) %>% 
  mutate(scenario = factor(scenario, levels=rev(unique(scens_short_TH)))) 

p.compare <- ggplot() +
  geom_bar(data = plot.Diff %>% 
             filter(scenario== scens_short_TH[[1]]) %>% 
             mutate(period = period -1), 
           aes(x=period, y=value, fill=variable, linetype=scenario), colour = "black", stat="identity", position="stack", width=1) +
  geom_bar(data = plot.Diff %>% 
             filter(scenario== scens_short_TH[[2]]) %>% 
             mutate(period = period), 
           aes(x=period, y=value, fill=variable, linetype=scenario), colour = "black", stat="identity", position="stack", width=1) +
  geom_bar(data = plot.Diff %>% 
             filter(scenario== scens_short_TH[[3]]) %>% 
             mutate(period = period +1), 
           aes(x=period, y=value, fill=variable, linetype=scenario), colour = "black", stat="identity", position="stack", width=1) +
  geom_col(position = position_stack(reverse = TRUE)) +
  scale_fill_manual(name = "Technology", values = el.supp.color) + 
  theme(legend.title = element_blank()) +
  theme(legend.position="bottom", legend.direction="horizontal", legend.text = element_text(size=14)) +
  guides(fill=guide_legend(nrow=4,byrow=TRUE), linetype=guide_legend(nrow=4,byrow=TRUE))+
  scale_linetype_discrete(breaks=scens_short_TH) +
  theme(axis.text = element_text(size=14), axis.title = element_text(size= 14, face="bold")) +
  xlab("period") + ylab(paste0("Generation (TWh)")) 

ggsave(plot = p.compare,
       filename =  paste0(plots.path, "/", run_number, "_CHA_ElSupp_TWh_compare.png"),
       width=25, height=16, units="cm")


###############################################################################

vars <- c(
  # liquids
  "FE|Buildings|Liquids|+|Fossil",
  "FE|Buildings|Liquids|+|Biomass",
  "FE|Buildings|Liquids|+|Hydrogen",
  "FE|Industry|Liquids|+|Fossil",
  "FE|Industry|Liquids|+|Biomass",
  "FE|Industry|Liquids|+|Hydrogen",
  "FE|Transport|Liquids|+|Fossil",
  "FE|Transport|Liquids|+|Biomass",
  "FE|Transport|Liquids|+|Hydrogen",
  # gases
  "FE|Buildings|Gases|+|Fossil",
  "FE|Buildings|Gases|+|Biomass",
  "FE|Buildings|Gases|+|Hydrogen",
  "FE|Industry|Gases|+|Fossil",
  "FE|Industry|Gases|+|Biomass",
  "FE|Industry|Gases|+|Hydrogen",
  "FE|Transport|Gases|+|Fossil",
  "FE|Transport|Gases|+|Biomass",
  "FE|Transport|Gases|+|Hydrogen",
  # solids
  "FE|Buildings|+|Solids", "FE|Industry|+|Solids",
  # heat
  "FE|Buildings|+|Heat", "FE|Industry|+|Heat",
  # hydrogen
  "FE|Buildings|+|Hydrogen", 
  "FE|Industry|+|Hydrogen",
  "FE|Transport|+|Hydrogen",
  # electricity
  "FE|Buildings|+|Electricity", 
  "FE|Industry|+|Electricity",
  "FE|Transport|+|Electricity"
)

plot.vars.order <- c("Others","Synthetic Liquids","Biomass Liquids","Fossil Liquids","Hydrogen" ,
                     "Solids","Heat","Synthetic Gases","Biomass Gases","Fossil Gases",
                     "Electricity")

plot.vars.color <- c("Others" = "blue",
                     "Synthetic Liquids" = "mediumpurple4",
                     "Biomass Liquids" = "olivedrab",
                     "Fossil Liquids" = "gray40",
                     "Hydrogen" = "#66cccc", 
                     "Solids" = "#191919",
                     "Heat" = "#cc0000",
                     "Synthetic Gases" = "plum2",
                     "Biomass Gases" = "olivedrab2",
                     "Fossil Gases" = "gray90",
                     "Electricity" = "#ffb200", 
                     NULL)

df.FE.sec <- df.wp %>% 
  filter(variable %in% vars, period %in% plot.period) %>%
  # add bar to be able to retrieve for dimensions of variables in the next step
  mutate( variable = paste0(variable,"| "))

# spread variables label across dimensions (to obtain sector dimensions and energy carrier dimension)
df.FE.sec$sector <- strsplit(as.character(df.FE.sec$variable), "\\|")  
df.FE.sec$sector <- sapply(df.FE.sec$sector, "[[", 2)

df.FE.sec$encar <- strsplit(as.character(df.FE.sec$variable), "\\|")  
df.FE.sec$encar <- sapply(df.FE.sec$encar, "[[", 3)

df.FE.sec$origin <- strsplit(as.character(df.FE.sec$variable), "\\|")  
df.FE.sec$origin <- sapply(df.FE.sec$origin, "[[", 4)

df.FE.sec$origin2 <- strsplit(as.character(df.FE.sec$variable), "\\|")  
df.FE.sec$origin2 <- sapply(df.FE.sec$origin2, "[[", 5)

df.FE.sec.plot <- df.FE.sec %>% 
  mutate(encar = ifelse( origin == "+",
                         paste(origin2, encar), encar)) %>% 
  mutate( encar = ifelse( encar == "+", origin, encar)) %>% 
  revalue.levels(encar = c("Hydrogen Liquids" = "Synthetic Liquids",
                           "Hydrogen Gases" = "Synthetic Gases")) %>% 
  order.levels(encar = plot.vars.order, scenario = scens_short) %>% 
  select(-variable,-unit)

vars.indu.TH <- c("FE|Industry|Others"="Others",
                  "FE|Industry|Coal"="Solids",
                  "FE|Industry|Oil"="Fossil Liquids",
                  "FE|Industry|Gas" = "Fossil Gases",
                  "FE|Industry|Electricity"="Electricity",
                  "FE|Industry|Heat"="Heat",
                  NULL) 

df.ind.TH <- data.TH %>%
  filter(variable %in% names(vars.indu.TH), unit == "EJ/yr") %>%
  revalue.levels(variable = vars.indu.TH) %>% 
  # order.levels(variable = names(vars.indu.TH)) %>% 
  mutate(sector = "Industry") %>% 
  select(scenario, encar = variable, period, sector, value) 

vars.build.TH <- c(
  "FE|Building|Others"="Others",
  "FE|Building|Coal"="Solids",
  "FE|Building|Oil"="Fossil Liquids",
  "FE|Building|Gas" = "Fossil Gases",
  "FE|Building|Electricity"="Electricity",
  "FE|Building|Heat"="Heat",
  NULL)

df.build.TH <- data.TH %>%
  filter(variable %in% names(vars.build.TH), unit == "EJ/yr") %>%
  revalue.levels(variable = vars.build.TH) %>% 
  order.levels(variable = vars.build.TH) %>% 
  mutate(sector = "Buildings") %>% 
  select(scenario,encar = variable,period,sector,value)

vars.trans.TH <- c(
  "FE|Transport|Others"="Others",
  "FE|Transport|Oil"="Fossil Liquids",
  "FE|Transport|Gas" = "Fossil Gases",
  "FE|Transport|Electricity"="Electricity",
  "FE|Transport|Heat"="Heat",
  NULL)

df.trans.TH <- data.TH %>%
  filter(variable %in% names(vars.trans.TH), unit == "EJ/yr") %>%
  revalue.levels(variable = vars.trans.TH) %>% 
  order.levels(variable = vars.trans.TH) %>% 
  mutate(sector = "Transport") %>% 
  select(scenario,encar = variable,period,sector,value)

df.FE.TH =  list(df.ind.TH, df.build.TH, df.trans.TH) %>%
  reduce(full_join) %>% 
  mutate(encar = factor(encar, levels=unique(names(plot.vars.color))))

df.FE.allmodels.CHA = list(df.FE.sec.plot %>% 
                             filter(region == "CHA") %>% 
                             select(-region,-model), df.FE.TH) %>%
  reduce(full_join) %>% 
  mutate(encar = factor(encar, levels=unique(names(plot.vars.color))))

  p.FE.sec <- ggplot(df.FE.allmodels.CHA) +
    geom_col(aes(period, value, fill=encar), alpha=0.8) +
    scale_fill_manual(values = plot.vars.color) +
    facet_wrap(sector~scenario, ncol = length(scens_short_TH)) +
    scale_x_continuous("period") +
    scale_y_continuous("Final Energy\n(EJ/yr)") +
    theme_bw()+
    theme(strip.background = element_blank(), text = element_text(size=12),
          axis.text.x = element_text(angle=90, vjust = 0.5)) +
    theme(legend.position = 'right', legend.title=element_blank())
  
  ggsave(plot = p.FE.sec, filename = paste0(plots.path, "/", run_number,"_","CHA","_FE_sec_encar_allmodels.png"),width=22, height=18, units = "cm")

############################
  df.FE.allmodels.CHA = list(df.FE.sec.plot %>% 
                               filter(region == "CHA") %>% 
                               filter(scenario == "2C medium") %>% 
                               mutate(scenario = "REMIND 2C medium") %>% 
                               filter(period %in% c(2020,2030,2050,2060)) %>% 
                               mutate(period = period +2) %>% 
                               select(-region,-model), df.FE.TH) %>%
    reduce(full_join) %>% 
    mutate(encar = factor(encar, levels=unique(names(plot.vars.color)))) %>% 
    filter(!period %in% c("2035","2045")) %>% 
    # mutate(period = period -1) %>% 
    mutate(period = as.factor(period))
  
  linetype.map <- c("C-GEM 2060NZ" = 'solid', 
                    "REMIND 2C medium" = 'dashed')
  
  df.FE.allmodels.CHA.group <- df.FE.allmodels.CHA %>% 
    select(period,scenario,sector,value) %>% 
    dplyr::group_by(period,scenario,sector) %>% 
    dplyr::summarise( sum = sum(value), .groups = "keep" ) %>% 
    dplyr::ungroup()
  
  p.FE.sec <- ggplot() +
    # Stacked bar for DIETER
    geom_bar(data = df.FE.allmodels.CHA,
             mapping = aes(x = period, y = value, fill = encar),
             stat = "identity",
             # position = "stack",
             width = 1) +
    # Border around stacked bar
    geom_bar(data = df.FE.allmodels.CHA.group,
             mapping = aes(x = period, y = sum, linetype = scenario),
             lwd = 0.5,
             colour = "black",
             stat = "identity",
             fill = "transparent",
             width = 1) + 
    scale_fill_manual(values = plot.vars.color) +
    facet_wrap(~sector) +
    scale_x_discrete("", breaks = c(2020,2030,2050,2060)) +
    scale_linetype_manual(name = "scenario", values = linetype.map) +
    ylab("Final Energy\n(EJ/yr)") +
    theme_bw()+
    theme(legend.title = element_blank()) +
    theme(axis.text=element_text(size=10), axis.title=element_text(size=10,face="bold"), plot.title = element_text(size = 12, face = "bold")) 

  ggsave(plot = p.FE.sec, filename = paste0(plots.path, "/", run_number,"_","CHA","_FE_sec_encar_compare.png"),width=22, height=10, units = "cm")
  
  