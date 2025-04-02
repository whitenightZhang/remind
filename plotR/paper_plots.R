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

for (kfile in list.files("~/Documents/INTEGRATE/data_comparison_AR6/functions/", pattern="*.R")) source(paste0("~/Documents/INTEGRATE/data_comparison_AR6/functions/", kfile))

library(cowplot)  # Useful for themes and for arranging plots
library(gridExtra)
# library(metR)  # Useful for contour_fill 
library(scales)
library(dichromat)

# Plotting style ----------------------------------------------------------
theme_set(theme_cowplot(font_size = 8))  # Use simple theme and set font size

options(warn=-1)

setwd('~/source/REMIND_integrate/remind/output')
run_number = "standalone_v43"

data.dir = paste0("./",run_number)

plot.dir = paste0("./plots/", run_number,"/")
dir.create(plot.dir, showWarnings = FALSE)

# regs = c("World", "CHA", "EUR")
regs = c("CHA")
reg = "CHA"
plot.period <- seq(2015, 2060, 5)

tt = plotstyle.add("1.5C", "1.5C", "#009E73", linestyle = "solid", marker = 19, replace = T)
tt = plotstyle.add("2C", "2C", "#882288", linestyle = "solid", marker = 19,replace = T)

scenarios_mapping_2C = c(
  "Base_INT" = "Baseline",
  "elec_INT_1150_po_plateau30_noadj" = "plateau 30",
  "elec_INT_1150_po_slope_slo_noadj" = "slow",
  "elec_INT_1150_po_slope_med_noadj" = "med",
  "elec_INT_1150_po_slope_fast_noadj" = "fast",
  # "elec_INT_1150_po_plateau30_noadj_fixbdg" = "plateau 30",
  # "elec_INT_1150_po_slope_slo_noadj_fixbdg" = "slow",
  # "elec_INT_1150_po_slope_med_noadj_fixbdg" = "med",
  # "elec_INT_1150_po_slope_fast_noadj_fixbdg" = "fast",
  NULL)

c("#0072B2", "#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

pe.color.mapping <- c("PE|Gas" = "#999959", "PE|Coal" = "#0c0c0c",
                   "PE|Solar" = "#ffcc00", "PE|Wind" = "#337fff", 
                   "PE|Geothermal" = "#334cff", "PE|Biomass" = "#005900",
                   "PE|Oil" = "#e51900", "PE|Hydro" = "#191999", "PE|Nuclear" = "#ff33ff",
                   NULL)

scenario_color_mapping_2C = c(
  "Baseline" = "#CC79A7",
  "historical" = "#0c0c0c",
  "2C plateau 30" = "#0072B2",
  "2C plateau 25" = "#882288",
  "2C slow" = "#56B4E9",
  "2C med" = "#009E73",
  "2C fast" = "#D55E00",
  "2C med adj" = "#E69F00",
  "2C slow adj" = "#56B4E9",
  "2C fast adj" = "#e51900",
  "2C plateau 30 adj" = "#0072B2",
  "2C plateau 25 adj" = "#882288",
  "med" = "#E69F00",
  "slow" = "#56B4E9",
  "fast" = "#e51900",
  "plateau 30" = "#0072B2",
  "plateau 25" = "#882288",
  NULL)

scenario_linetype_mapping_2C = c(
  "Baseline" = "dashed",
  "historical" = "solid",
  # "2C med adj" = "solid",
  # "2C slow adj" = "solid",
  # "2C fast adj" = "solid",
  # "2C plateau 30 adj" = "solid",
  # "2C plateau 25 adj" = "solid",
  "med" = "solid",
  "slow" = "solid",
  "fast" = "solid",
  "plateau 30" = "solid",
  "plateau 25" = "solid",
  NULL)

elecshare_variable_mapping <- c("FE|Electricity|Share" = "All sectors", 
                                "FE|Industry|Electricity|Share" = "Industry",
                                "FE|Buildings|Electricity|Share" = "Buildings",
                                "FE|Transport|Electricity|Share" = "Transport (with Bunkers)")

scenario_group_toplot <- c(list(scenarios_mapping_2C)) 
scenario_group_names <- c("2C")
scenario_group_color <- c(list(scenario_color_mapping_2C))

# for (i in c(1:length(scenario_group_toplot))){
  i = 1
  
  scenarios_mapping <- scenario_group_toplot[[i]]
  scenarios_mapping_nobase <- scenarios_mapping[2:length(scenarios_mapping)]
  scenario_color_mapping <- scenario_group_color[[i]]
  scenario_color_mapping.whist <- c(scenario_color_mapping, "historical" = "#000000")
  scenario_group_name <- scenario_group_names[[i]]
  scenario_linetype_mapping <- scenario_linetype_mapping_2C
  scenario_linetype_mapping.whist <- c(scenario_linetype_mapping, "historical" = "solid")
  
  data.files = lapply(names(scenarios_mapping), function(x) paste0( data.dir, "/REMIND_generic_", x, "_withoutPlus.mif")) %>% unlist
  
  run_indices = seq(from = 1, to = length(data.files), by = 1)
  df.raw = NULL

# trim df to chosen scenarios and regions
  for (i in run_indices){
    print(i)
    REMIND.data = read.quitte(data.files[[i]])
    df.raw <- rbind(df.raw, REMIND.data)
  }

  scens0 = stringr::str_replace(names(scenarios_mapping), "REMIND_generic_", "")
  scens = stringr::str_replace(scens0, "-rem-5", "")
  
  df0 = filter(df.raw, scenario %in% scens, region %in% reg ) %>%
    order.levels(scenario = scens) %>%
    revalue.levels(scenario = scenarios_mapping) %>%
    factor.data.frame()
  
  df.world = filter(df.raw, scenario %in% scens, region %in% c("World") ) %>%
    order.levels(scenario = scens) %>%
    revalue.levels(scenario = scenarios_mapping) %>%
    factor.data.frame()
  
  df.eur = filter(df.raw, scenario %in% scens, region %in% c("EUR") ) %>%
    order.levels(scenario = scens) %>%
    revalue.levels(scenario = scenarios_mapping) %>%
    factor.data.frame()
  
  df_test = filter(df.world, variable == "Emi|CO2|Gross|Energy|Supply|Electricity")

###################### Rescale price ######################
# rescale price and cost data to 2020 using 2005->2020 deflator of 1.95 https://github.com/pik-piam/GDPuc

indices = grepl("US\\$2005",df0$unit)
df0[indices, "value"] = df0[indices, "value"] * 1.95
levels(df0$unit) = gsub("US\\$2005", "US\\$2020", levels(df0$unit))

# MJ/US$2020

# rm(df.raw)

######## Tsinghua mif #########################
data.TH = read.quitte(paste0("C-GEM_CHA_NZ.mif"))

###################### Historical mif ######################################
tmax = 2060

# also read in historical data
df.hist = read.quitte(paste0("./",run_number, "/historical.mif"))
df.hist = 
  calc_addVariable(df.hist, 
                   "`Share|FE|Electricity`" = "`FE|Electricity` / `FE` * 100", units  = "%") 

df.hist_FE = filter(df.hist, model =="IEA", region %in% regs ) %>%
  factor.data.frame() %>% 
  filter(variable %in% c("FE", "FE|Industry", "FE|Building", "FE|Transport", "FE|Electricity"))

df.hist.emi = df.hist %>%
  filter(model =="PRIMAPhist", region %in% c("CHA") ) %>% 
  filter(variable == "Emi|CO2") %>%
  mutate(value = value/1e3) %>% 
  select(-scenario)

df.hist_cap = filter(df.hist, model =="IEA", region %in% regs ) %>%
  factor.data.frame() %>% 
  filter(variable %in% c("FE", "FE|Industry", "FE|Building", "FE|Transport", "FE|Electricity"))

df.hist_eshare = filter(df.hist, model =="IEA", region %in% regs ) %>%
  factor.data.frame() %>% 
  filter(variable %in% c("Share|FE|Electricity"))

df.hist_eshare2 = filter(df.hist, model =="IEA_WEO", region %in% regs, period >= 1971, period <= 2020 ) %>%
  factor.data.frame() %>% 
  filter(variable %in% c("Share|FE|Electricity"))

df.hist.elec_emi = filter(df.hist, model =="Ember", region %in% regs, period >= 1971, period <= 2020 ) %>%
  factor.data.frame() %>% 
  filter(variable %in% c("Emi|CO2|Energy|Supply|+|Electricity w/ couple prod")) %>% 
  select(-model)
  
df.hist.se.el = filter(df.hist, model =="Ember", region %in% regs, period >= 1971, period <= 2020 ) %>%
  factor.data.frame() %>% 
  filter(variable %in% c("SE|Electricity")) %>% 
  select(-model)

df.hist.CI_elec <- list(df.hist.elec_emi, df.hist.se.el) %>% 
  reduce(full_join) 

rescale_ratio_emi=5243/4432 # emission of REMIND "Emi|CO2|Energy|Supply|Electricity w/ couple prod" divided by historical "Emi|CO2|Energy|Supply|+|Electricity w/ couple prod" (Ember) for 2020, because REMIND accounts for co-production = 1.188

rescale_ratio_seel=30.968/28.965 # REMIND "SE|Electricity" divided by "SE|Electricity" (Ember), because REMIND accounts for co-production = 1.069

df0 <- df0 %>% 
  calc_addVariable("`Carbon snIntensity|SE Electricity`" = "(`Emi|CO2|Energy|Supply|Electricity w/ couple prod`) / (`SE|Electricity`) * 3.6", units = "tCO2/MWh")  

df.hist.CI_elec <- df.hist.CI_elec %>% 
  calc_addVariable("`Carbon Intensity|SE Electricity`" = "(`Emi|CO2|Energy|Supply|+|Electricity w/ couple prod`) / (`SE|Electricity`) * 3.6 * 1.188/ 1.069", units = "tCO2/MWh") %>% 
  filter(period > 2000) %>% 
  filter(variable == "Carbon Intensity|SE Electricity")

var.pe <- c("PE|Coal",
            "PE|Oil",
            "PE|Gas",
            "PE|Biomass",
            "PE|Nuclear",
            "PE|Solar",
            "PE|Wind",
            "PE|Hydro",
            "PE|Geothermal")

df.hist.pe.tech = filter(df.hist, model =="BP", region == "CHA", period >= 2001, period <= 2020 ) %>%
  factor.data.frame() %>% 
  filter(variable %in% var.pe) 

# rm(df.hist)

df.hist.ember = read.quitte("./Ember/Ember_CHA.csv") %>%
  factor.data.frame() %>%
  mutate(period = as.numeric(period))

df.hist.ember <-  df.hist.ember %>%
  calc_addVariable( "`Capacity Factor|Coal`" = "(`SE|Electricity|+|Coal`)*2.7e5  / ((`Cap|Electricity|Coal`) * 8760) * 100", units  = "%")

df.hist.ember.ci <-  df.hist.ember %>%
  filter(variable == "Carbon Intensity|SE Electricity") %>%
  filter(region == "CHN" ) %>%
  mutate(value = value)

df.hist.ember.cap <-  df.hist.ember %>%
  filter(variable == "Cap|Electricity|Coal") %>%
  filter(region == "CHN" )

df.hist.ember.gen <-  df.hist.ember %>%
  filter(variable == "SE|Electricity|+|Coal") %>%
  filter(region == "CHN" )

df.hist_FE = calc_addVariable(df.hist_FE,
                   "`FE|Electricity|Share`" = "`FE|Electricity` / `FE` * 100",
                   units  = "%")

###################### CI vs. elec rate #####################################
vars = c("FE|Electricity", "FE")

df.plot.elecshare = filter(df0, variable == "FE|Electricity|Share", period <= 2060, region == reg ) 

df.hist.plot.elecshare  = filter(df.hist_FE, variable == "FE|Electricity|Share", period <= 2060 , region == reg)  %>% 
  revalue.levels(variable = elecshare_variable_mapping) %>% 
  mutate(variable = factor(variable, levels=elecshare_variable_mapping))

p <- ggplot() +
  geom_line(data = df.hist.plot.elecshare, aes(x = period, y = value), size = 1,  color = "black") +
  geom_line(data = df.plot.elecshare, aes(x = period, y = value, color = scenario,  size = tech), size = 0.4, alpha = 0.5) +
  scale_size_manual( values = c(1,1.7)) +
  ylab("Electricity Share in Final Energy [%]") +
  xlab("") +
  scale_x_continuous(limits = c(1995, 2060), breaks = c(2000, 2020, 2040, 2060),
                     minor_breaks = unique(seq(2000, 2060,10)), ) +
  theme_bw(base_size = 7) +
  theme(legend.key.width = unit(1.5,"cm"))

ggsave(filename = paste0(plot.dir, "/", scenario_group_name, "_Elecshares.png"), width=11, height=6, units = "cm")

df0 <- df0 %>% 
calc_addVariable("`Carbon Intensity|SE Electricity`" = "(`Emi|CO2|Energy|Supply|Electricity w/ couple prod`) / (`SE|Electricity`) * 3.6", units = "tCO2/MWh")  

df.plot.ci <- df0 %>% 
  filter(variable == "Carbon Intensity|SE Electricity") %>% 
  filter(period <= 2060)

df.plot.ci.check <- df0 %>% 
  filter(variable %in% c("Carbon Intensity|SE Electricity", "Emi|CO2|Energy|Supply|Electricity w/ couple prod", "Emi|CO2|Gross|Energy|Supply|Electricity")) %>% 
  filter(period <= 2060)

p_CI <- ggplot() +
  geom_line(data = df.plot.ci %>% filter(period >2015), aes(x = period, y = value, color = scenario, linetype=scenario), size = 1, alpha = 0.85) +
  geom_point(data = df.plot.ci %>% filter(period >2015), aes(x = period, y = value, color = scenario, linetype=scenario), size = 1.5) +
  geom_line(data = df.hist.CI_elec, aes(x = period, y = value, color = scenario, linetype=scenario), size = 1) +
  scale_x_continuous(limits = c(2000, 2060), breaks = c( 2000, 2020, 2040,  2060, 2080, 2100),
                     minor_breaks = unique(seq(2000, 2100,10)), ) +
  ylab("Electricity CO2 emission \n intensity [kgCO2/MWh]") + 
  annotate(geom="text", x=2018, y=800, label="power emission intensity",size=2.7,family="serif")+
  scale_color_manual(name = "", values = scenario_color_mapping.whist) + 
  scale_linetype_manual(name = "", values = scenario_linetype_mapping.whist) + 
  xlab("") + 
  theme_bw(base_size = 8) + 
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(),legend.text = element_text(size=5)) 

ggsave(filename = paste0(plot.dir, "/CI_", scenario_group_name, ".png"),p_CI, width=10, height=8, units = "cm", bg = "white")

vari = "FE|Electricity|Share"

df.plot.eshare = filter(df0, variable == vari, period <= 2060 )

plot_scale = 12

p_CI2 <- p_CI +
  geom_line(data = df.plot.eshare %>% filter(period >2015), aes(x = period, y = value*plot_scale, color = scenario, linetype=scenario), size = 1, alpha = 0.3) +
  geom_point(data = df.plot.eshare %>% filter(period >2015), aes(x = period, y = value*plot_scale, color = scenario, linetype=scenario), size = 1) +
  geom_line(data = df.hist.plot.elecshare, aes(x = period, y = value*plot_scale, color = scenario, linetype=scenario), size = 1) +
  annotate(geom="text", x=2050, y=800, label="electrification rate",size=2.7,alpha = 0.6,family="serif")+
  scale_linetype_manual(name = "", values = scenario_linetype_mapping) +  
  guides(color=guide_legend(nrow=2, byrow=TRUE)) +
  coord_cartesian(ylim = c(0,800)) +
  
  scale_y_continuous(sec.axis = sec_axis(~./plot_scale, name = "Electricity share in final energy [%]"))

ggsave(filename = paste0(plot.dir, "/CI-elecR_", scenario_group_name, ".png"),p_CI2, width=10, height=8, units = "cm", bg = "white")

df.plot.ci.small <- df.plot.ci %>% 
  select(scenario,period, ci = value)

df.plot.eshare.small <- df.plot.eshare %>% 
  select(scenario,period, eshare = value)

remind.ci.elshare <- list(df.plot.ci.small, df.plot.eshare.small) %>% 
  reduce(full_join) 

p <- ggplot() +
  geom_text(data = remind.ci.elshare, aes(x = eshare, y = ci, color = scenario, label=period), size = 2, alpha = 0.8, hjust=1.5, vjust=0) +
  geom_point(data = remind.ci.elshare, aes(x = eshare, y = ci, color = scenario, label=period), size = 2, alpha = 0.8) +
  geom_line(data = remind.ci.elshare, aes(x = eshare, y = ci, color = scenario, label=period), size = 1, alpha = 0.3) +
  ylab("Electricity CO2 Emission Intensity [kgCO2/MWh]") + 
  scale_color_manual(name = "", values = scenario_color_mapping) + 
  scale_linetype_manual(name = "", values = scenario_linetype_mapping) + 
  theme(axis.text=element_text(size=8), axis.title=element_text(size= 8, face="bold"), strip.text = element_text(size=8)) +
  xlab("Electricity Share in Final Energy [%]") + 
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(), legend.text = element_text(size=8)) +
guides(color=guide_legend(nrow=2, byrow=TRUE))

ggsave(filename = paste0(plot.dir, "/CI-elecR-cross_", scenario_group_name, ".png"),p, width=10, height=10, units = "cm", bg = "white")


###################### power sector ##################################
# coal capacity
df.plot.coal.tot.cap = filter(df0, variable == "Cap|Electricity|Coal", period <= 2060, region == reg )

df.plot.coal_wcc = filter(df0, variable == "Cap|Electricity|Coal|w/ CC", period <= 2060, region == reg ) 

p.coalcap <- ggplot() +
  geom_line(data = df.hist.ember.cap, aes(x = period, y = value, color = scenario, linetype=scenario), size = 1, alpha = 1)+
  geom_line(data = df.plot.coal.tot.cap , aes(x = period, y = value, color = scenario, linetype=scenario), size = 1, alpha = 0.8) +
  ylab("Coal Power Capacity [GW]") + 
  scale_color_manual(name = "", values = scenario_color_mapping.whist) + 
  scale_linetype_manual(name = "", values = scenario_linetype_mapping.whist) + 
  theme(axis.text=element_text(size=8), axis.title=element_text(size= 8, face="bold"), strip.text = element_text(size=8)) +
  xlab("") + 
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(), legend.text = element_text(size=8)) +
  guides(color=guide_legend(nrow=2, byrow=TRUE))

ggsave(filename = paste0(plot.dir, "/coalCap_", scenario_group_name, ".png"), p.coalcap, width=9, height=6, units = "cm", bg = "white")

# coal generation
df.plot.coal.tot.gen = filter(df0, variable == "SE|Electricity|Coal", period <= 2060, region == reg )

p.coalgen_0 <- ggplot() +
  geom_line(data = df.hist.ember.gen, aes(x = period, y = value, color = scenario, linetype=scenario), size = 1, alpha = 1)+
  geom_line(data = df.plot.coal.tot.gen, aes(x = period, y = value, color = scenario, linetype=scenario), size = 1, alpha = 0.8) + 
  ylab("Coal power \n generation [EJ/yr]") + 
  scale_color_manual(name = "", values = scenario_color_mapping.whist) + 
  scale_linetype_manual(name = "", values = scenario_linetype_mapping.whist) + 
  theme(axis.text=element_text(size=8), axis.title=element_text(size= 8), strip.text = element_text(size=8)) +
  xlab("") + 
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(), legend.text = element_text(size=8)) +
  guides(color=guide_legend(nrow=2, byrow=TRUE))

p.coalgen <- ggplot() +
  geom_line(data = df.hist.ember.gen, aes(x = period, y = value, color = scenario, linetype=scenario), size = 1, alpha = 1)+
  geom_line(data = df.plot.coal.tot.gen, aes(x = period, y = value, color = scenario, linetype=scenario), size = 1, alpha = 0.8) + 
  ylab("Coal power generation [EJ/yr]") + 
  scale_color_manual(name = "", values = scenario_color_mapping.whist) + 
  scale_linetype_manual(name = "", values = scenario_linetype_mapping.whist) + 
  theme(axis.text=element_text(size=8), axis.title=element_text(size= 8, face="bold"), strip.text = element_text(size=8)) +
  xlab("") + 
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(), legend.text = element_text(size=8)) +
  guides(color=guide_legend(nrow=2, byrow=TRUE))

ggsave(filename = paste0(plot.dir, "/coalGen_", scenario_group_name, ".png"), p.coalgen, width=9, height=6, units = "cm", bg = "white")

p.plots <- plot_grid(p.coalcap +
                       theme(legend.position = "none"),
                     p.coalgen,
                     labels = c("", "", ""),
                     label_size = 9,
                     nrow = 2)

# p <- plot_grid(p.plots,
#                label_size = 11,
#                nrow = 2,
#                rel_heights = c(1, 0.2))

ggsave(plot = p.plots, 
       filename =  paste0(plot.dir, "/Coalgencap.png"),
       width=7, height=10, units="cm", bg = "white")

##################### SE Electricity Supply and Demand Mix ####################
### plot with positive supply and negative demand bars

el.supp.vars.mapping <- c("SE|Electricity|Wind|Offshore" = "Wind Offshore",
                          "SE|Electricity|Wind|Onshore" = "Wind Onshore",
                          "SE|Electricity|Solar" = "PV",
                          "SE|Electricity|Hydrogen" = "Hydrogen",
                          "SE|Electricity|Gas|w/ CC" = "Gas CCS",
                          "SE|Electricity|Gas|w/o CC" = "Gas without CCS",
                          "SE|Electricity|Nuclear" = "Nuclear",
                          "SE|Electricity|Biomass" = "Biomass",
                          "SE|Electricity|Solar|CSP" = "CSP",
                          "SE|Electricity|Hydro" = "Hydro",
                          "SE|Electricity|Geothermal" = "Geothermal",
                          "SE|Electricity|Coal|w/ CC" = "Coal CCS",
                          "SE|Electricity|Coal|w/o CC" = "Coal without CCS", 
                          "SE|Electricity|Oil" = "Oil",
                          NULL)

el.dem.vars.mapping <- c(
                 "SE|Input|Electricity|Self Consumption Energy System" = "for own consumption",
                 "SE|Input|Electricity|Hydrogen|direct FE H2" = "for direct H2",
                 "SE|Input|Electricity|Hydrogen|Electricity Storage" = "for power storage",
                 "SE|Input|Electricity|Hydrogen|Synthetic Fuels"  = "for synthetic fuel", 
                 # "SE|Input|Electricity|CDR" = "for CDR",
                 "SE|Input|Electricity|Buildings" = "for Buildings",
                 "SE|Input|Electricity|Industry" = "for Industry",
                 "SE|Input|Electricity|Transport" = "for Transport",
                 # "SE|Input|Electricity|Hydrogen|Synthetic Fuels|Liquids" = "for synthetic liquids", 
                 # "SE|Input|Electricity|Hydrogen|Synthetic Fuels|Gases" = "for synthetic gases",
                 NULL)

el.vars.mapping <- c(el.supp.vars.mapping, el.dem.vars.mapping)

el.color.mapping <- c( "PV" = "yellow",
                       "Wind Offshore" = "#0085B2",
                       "Wind Onshore" = "deepskyblue",
                      "Hydrogen" = "cyan",
                      "Nuclear" = "darkorchid",
                      "Biomass" = "darkgreen",
                      "CSP" = "#8E4585",
                      "Hydro" = "#186FEF",
                      "Geothermal" = "#ff8242",
                      "Gas CCS" = "#009E73",
                      "Gas without CCS" = "grey50", 
                      "Coal CCS" = "#e9e3c8",
                      "Coal without CCS" = "#222444", 
                      "Oil" = "#666633",
                      "for own consumption" = "darkgoldenrod",
                      "for direct H2" = "darkcyan",
                      "for power storage" = "mediumpurple1",
                      "for CDR" = "lightgreen",
                      "for Buildings" = "red",
                      "for Industry" = "grey50",
                      "for Transport" = "blue",
                      "for synthetic fuel" = "lightyellow3"
                      # "for synthetic liquids" = "lightyellow3", 
                      # "for synthetic gases" = "navajowhite1"
                      )


for(scen in scenarios_mapping){
  
df.el <- df0 %>% 
  filter(scenario == scen) %>% 
  filter(variable %in% names(el.supp.vars.mapping)) %>% 
  # bind demand variables with negative sign
  rbind(df0 %>% 
          filter(scenario == scen) %>% 
           filter(variable %in% names(el.dem.vars.mapping)) %>%
           mutate( value = - value)) %>% 
  revalue.levels(variable = el.vars.mapping) %>% 
  order.levels(variable = names(el.color.mapping)) %>% 
  filter(period %in% plot.period)

  p.el <- ggplot() +
    geom_col(data=df.el, 
             aes(period, value, fill=variable), 
             alpha=.6, width = 3) +
    facet_wrap(~scenario) +
    scale_y_continuous("Electricity (EJ/yr)") +
    scale_x_continuous("") +
    scale_fill_manual(values = el.color.mapping) +
    theme_bw() +
    theme( axis.text.x = element_text(angle=90, vjust = 0.5), 
           legend.position = "bottom",
           strip.background = element_blank(),
           legend.title = element_blank(),
           strip.text.y = element_text(angle = 0, hjust = 0)) +
    guides(fill=guide_legend(ncol=4, direction = "horizontal"), color = guide_legend(nrow = 4, byrow = TRUE))
  
  ggsave(plot = p.el,
         filename =  paste0(plot.dir, "/",reg, "_", scen, "_El_SinkSource.png"),
         width=15, height=16, units="cm", bg = "white")
}

scenarios_mapping_subset = c("fast", "plateau 30")
  
  df.el <- df0 %>% 
    filter(scenario %in% scenarios_mapping_subset) %>% 
    filter(variable %in% names(el.supp.vars.mapping)) %>% 
    # bind demand variables with negative sign
    rbind(df0 %>% 
            filter(scenario %in% scenarios_mapping_subset) %>% 
            filter(variable %in% names(el.dem.vars.mapping)) %>%
            mutate( value = - value)) %>% 
    revalue.levels(variable = el.vars.mapping) %>% 
    order.levels(variable = names(el.color.mapping)) %>% 
    filter(period %in% plot.period)
  
  p.el <- ggplot() +
    geom_col(data = df.el, 
             aes(period, value, fill=variable), 
             alpha=.6, width = 3) +
    facet_wrap(~scenario) +
    scale_y_continuous("Electricity (EJ/yr)") +
    scale_x_continuous("") +
    scale_fill_manual(values = el.color.mapping) +
    theme_bw() +
    theme( axis.text.x = element_text(angle=90, vjust = 0.5), 
           legend.position = "bottom",
           strip.background = element_blank(),
           legend.title = element_blank(),
           strip.text.y = element_text(angle = 0, hjust = 0)) +
    guides(fill=guide_legend(ncol=5, direction = "horizontal"), color = guide_legend(nrow = 4, byrow = TRUE)) +
    facet_wrap(~scenario) 
  
  ggsave(plot = p.el,
         filename =  paste0(plot.dir, "/",reg, "_fastslow_El_SinkSource.png"),
         width=20, height=15, units="cm", bg = "white")


df.el.scens <- df0 %>% 
  filter(variable %in% names(el.supp.vars.mapping)) %>% 
  # bind demand variables with negative sign
  rbind(df0 %>% 
          filter(variable %in% names(el.dem.vars.mapping)) %>%
          mutate( value = - value)) %>% 
  revalue.levels(variable = el.vars.mapping) %>% 
  order.levels(variable = names(el.color.mapping)) %>% 
  filter(period %in% c(2020,2030,2040,2060))

p.compare <- ggplot() +
  geom_bar(data = df.el.scens %>% 
             filter(scenario== scenarios_mapping[[1]]) %>% 
             mutate(period = period -1), 
           aes(x=period, y=value, fill=variable, linetype=scenario), colour = "black", stat="identity", position="stack", width=1) +
  geom_bar(data = df.el.scens %>% 
             filter(scenario== scenarios_mapping[[2]]) %>% 
             mutate(period = period), 
           aes(x=period, y=value, fill=variable, linetype=scenario), colour = "black", stat="identity", position="stack", width=1) +
  geom_bar(data = df.el.scens %>% 
             filter(scenario== scenarios_mapping[[3]]) %>% 
             mutate(period = period +1), 
           aes(x=period, y=value, fill=variable, linetype=scenario), colour = "black", stat="identity", position="stack", width=1) +
  geom_bar(data = df.el.scens %>% 
             filter(scenario== scenarios_mapping[[4]]) %>% 
             mutate(period = period + 2), 
           aes(x=period, y=value, fill=variable, linetype=scenario), colour = "black", stat="identity", position="stack", width=1) + 
  geom_col(position = position_stack(reverse = TRUE)) +
  scale_fill_manual(name = "Technology", values = el.color.mapping) + 
  theme(legend.title = element_blank()) +
  theme(legend.position="bottom", legend.direction="horizontal", legend.text = element_text(size=10)) +
  guides(fill=guide_legend(nrow=4,byrow=TRUE), linetype=guide_legend(nrow=4,byrow=TRUE))+
  scale_linetype_discrete(breaks=scenarios_mapping) +
  theme(axis.text = element_text(size=10), axis.title = element_text(size= 10, face="bold")) +
  xlab("period") + ylab(paste0("Generation (TWh)")) 

ggsave(plot = p.compare,
       filename =  paste0(plot.dir, "/",reg,"_",scenario_group_name,"_El_SinkSource_compare.png"),
       width=10, height=12, units="cm", bg = "white")

#################### cumulative emission ####################
df0_eu = filter(df.raw, scenario %in% scens, region %in% "EUR" ) %>%
  order.levels(scenario = scens) %>%
  revalue.levels(scenario = scenarios_mapping) %>%
  factor.data.frame()

df.cum_eu <- df0_eu %>% 
  filter(variable == "Emi|CO2|Cumulated") %>% 
  filter(period %in% plot.period) %>% 
  select(scenario,period,cum_eu = value)

df.emi <- df0 %>% 
  filter(variable == "Emi|CO2") %>% 
  filter(period %in% plot.period) %>% 
  select(scenario,period,emi_cha = value)

df.cum <- df0 %>% 
  filter(variable == "Emi|CO2|Cumulated") %>% 
  filter(period %in% plot.period) %>% 
  select(scenario,period,cum_cha = value)

df.cum.world <- df.world %>% 
  filter(variable == "Emi|CO2|Cumulated") %>% 
  filter(period %in% plot.period)%>% 
  select(scenario,period,cum_world = value)

df.cum.eur <- df.eur %>% 
  filter(variable == "Emi|CO2|Cumulated") %>% 
  filter(period %in% plot.period)%>% 
  select(scenario,period,cum_eur = value)

df.cum.fraction <- list(df.cum.world, df.cum) %>% 
  reduce(full_join) %>% 
  mutate(value = cum_cha / cum_world*1e2)

df.cum.elec <- df0 %>% 
  filter(variable == "Emi|CO2|Cumulated|Gross|Energy|Supply|Electricity") %>% 
  filter(period %in% plot.period) %>% 
  select(scenario,period,cum_cha_elec = value)

df.cum.fraction.elec <- list(df.cum.world, df.cum.elec) %>% 
  reduce(full_join) %>% 
  mutate(value = cum_cha_elec / cum_world*1e2)

df.cum.nonelec <- list(df.cum, df.cum.elec) %>% 
  reduce(full_join) %>% 
  mutate(cum_cha_nonelec = cum_cha - cum_cha_elec)

df.cum.elec_2020 <- df0 %>%  # cumulative emission of power sector in China since 2020
  filter(variable == "Emi|CO2|Cumulated|Gross|Energy|Supply|Electricity") %>% 
  filter(period == 2020) %>% 
  select(scenario,period,cum_cha_elec_2020 = value)

df.emi.elec_bw_2020_and_2023 <- df0 %>%  #annual emission
  filter(variable == "Emi|CO2|Energy|Supply|Electricity w/ couple prod") %>% 
  filter(period == 2020) %>% 
  mutate(value = value * 2.5)%>%
  select(scenario, cha_elec_bw_2020_and_2023 = value)

df.cum.elec.until2023 <- list(df.emi.elec_bw_2020_and_2023, df.cum.elec_2020) %>%  # cumulative emission of power sector in China since 2023  
  reduce(full_join) %>% 
  mutate(cum_cha_elec_until2023 = cum_cha_elec_2020 + cha_elec_bw_2020_and_2023) %>% 
  select(scenario, cum_cha_elec_until2023)

df.cum.elec.until2060 <- df0 %>%  # cumulative emission of power sector in China since 2020
  filter(variable == "Emi|CO2|Cumulated|Gross|Energy|Supply|Electricity") %>% 
  filter(period == 2060) %>% 
  select(scenario,cum_cha_elec_2060 = value)

df.cum.elec.bw_2023_2060 <- list(df.cum.elec.until2023, df.cum.elec.until2060) %>%  # cumulative emission of power sector in China since 2023  
  reduce(full_join) %>% 
  mutate(cum_cha_elec_bw2023_2060 = cum_cha_elec_2060 - cum_cha_elec_until2023) 


p.cum <- ggplot() +
  geom_line(data=df.cum %>% mutate(cum_cha = cum_cha /1e3)
            %>% filter(scenario != "Baseline"), 
            aes(period, cum_cha, color=scenario, linetype=scenario), 
            alpha=1, width = 5) +
  xlab("") + ylab(paste0("Cumulative \n emission (Gt CO2)")) +
  scale_color_manual(name = "", values = scenario_color_mapping) + 
  scale_linetype_manual(name = "", values = scenario_linetype_mapping) + 
  theme(axis.text=element_text(size=8), axis.title=element_text(size=7), strip.text = element_text(size = 8)) +
  theme( 
         legend.position = "bottom",
         strip.background = element_blank(),
         legend.title = element_blank(),
         strip.text.y = element_text(angle = 0, hjust = 0)) +
  guides(fill=guide_legend(ncol=4, direction = "horizontal"), color = guide_legend(nrow = 2, byrow = TRUE)) + 
theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(),legend.text = element_text(size=5), legend.key.width = unit(0.5,"cm")) +
  scale_x_continuous(limits = c(2015, 2060), breaks = c( 2020, 2040,2060, 2080, 2100),
                     minor_breaks = unique(seq(2000, 2100,10)), )

ggsave(plot = p.cum,
       filename =  paste0(plot.dir, "/",reg, "_Cumulative.png"),
       width=8, height=12, units="cm", bg = "white")

p.cum.elec <- ggplot() +
  geom_line(data=df.cum.elec %>% mutate(cum_cha_elec = cum_cha_elec /1e3)
            %>% filter(scenario != "Baseline"), 
            aes(period, cum_cha_elec, color=scenario, linetype=scenario), 
            alpha=1, width = 5) +
  xlab("") + ylab(paste0("Power sector cumulative \n emission (Gt CO2)")) +
  scale_color_manual(name = "", values = scenario_color_mapping) + 
  scale_linetype_manual(name = "", values = scenario_linetype_mapping) + 
  theme(axis.text=element_text(size=8), axis.title=element_text(size=7), strip.text = element_text(size = 8)) +
  theme( 
         legend.position = "bottom",
         strip.background = element_blank(),
         legend.title = element_blank(),
         strip.text.y = element_text(angle = 0, hjust = 0)) +
  guides(fill=guide_legend(ncol=4, direction = "horizontal"), color = guide_legend(nrow = 2, byrow = TRUE))+
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(),legend.text = element_text(size=7)) +
  theme(legend.key.width = unit(0.5,"cm")) +
  scale_x_continuous(limits = c(2015, 2060), breaks = c( 2020, 2040,2060, 2080, 2100),
                     minor_breaks = unique(seq(2000, 2100,10))) 

ggsave(plot = p.cum.elec,
       filename =  paste0(plot.dir, "/",reg, "_ElecCumulative.png"),
       width=9, height=9, units="cm", bg = "white")

p.cum.nonelec <- ggplot() +
  geom_line(data=df.cum.nonelec %>% mutate(cum_cha_nonelec = cum_cha_nonelec /1e3)
            %>% filter(scenario != "Baseline"), 
            aes(period, cum_cha_nonelec, color=scenario, linetype=scenario), 
            alpha=1, width = 5) +
  xlab("") + ylab(paste0("Non-power sector Cumulative emission (Gt CO2)")) +
  scale_color_manual(name = "", values = scenario_color_mapping) + 
  scale_linetype_manual(name = "", values = scenario_linetype_mapping) + 
  theme(axis.text=element_text(size=8), axis.title=element_text(size=8), strip.text = element_text(size = 8)) +
  theme( 
    legend.position = "bottom",
    strip.background = element_blank(),
    legend.title = element_blank(),
    strip.text.y = element_text(angle = 0, hjust = 0)) +
  guides(fill=guide_legend(ncol=4, direction = "horizontal"), color = guide_legend(nrow = 2, byrow = TRUE))+
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(),legend.text = element_text(size=7)) +
  theme(legend.key.width = unit(0.5,"cm")) +
  scale_x_continuous(limits = c(2015, 2060), breaks = c( 2020, 2040,2060, 2080, 2100),
                     minor_breaks = unique(seq(2000, 2100,10))) 

ggsave(plot = p.cum.nonelec,
       filename =  paste0(plot.dir, "/",reg, "_NonElecCumulative.png"),
       width=9, height=9, units="cm", bg = "white")

p.cum_frac <- ggplot() +
  geom_line(data=df.cum.fraction, 
           aes(period, value, color=scenario, linetype=scenario), 
           alpha=.8, width = 6) +
  xlab("") + ylab(paste0("China's cumulative emission share of world total (%)")) +
  scale_color_manual(name = "", values = scenario_color_mapping) + 
  scale_linetype_manual(name = "", values = scenario_linetype_mapping) + 
  theme_bw() +
  theme(axis.text=element_text(size=8), axis.title=element_text(size=6), strip.text = element_text(size = 8)) +
  theme( axis.text.x = element_text(angle=90, vjust = 0.5), 
         legend.position = "bottom",
         strip.background = element_blank(),
         legend.title = element_blank(),
         strip.text.y = element_text(angle = 0, hjust = 0)) +
  guides(fill=guide_legend(ncol=4, direction = "horizontal"), color = guide_legend(nrow = 2, byrow = TRUE))

ggsave(plot = p.cum_frac,
       filename =  paste0(plot.dir, "/",reg, "_Cumulative_Frac.png"),
       width=10, height=12, units="cm", bg = "white")

p.cum_frac.elec <- ggplot() +
  geom_line(data=df.cum.fraction.elec, 
            aes(period, value, color=scenario), 
            alpha=.8, width = 6) +
  xlab("") + ylab(paste0("China's cumulative power sector \n emission share of world total (%)")) +
  scale_color_manual(name = "", values = scenario_color_mapping) + 
  scale_linetype_manual(name = "", values = scenario_linetype_mapping) + 
  theme_bw() +
  theme(axis.text=element_text(size=6), axis.title=element_text(size=6), strip.text = element_text(size = 6)) +
  theme( axis.text.x = element_text(angle=90, vjust = 0.5), 
         legend.position = "bottom",
         strip.background = element_blank(),
         legend.title = element_blank(),
         strip.text.y = element_text(angle = 0, hjust = 0)) +
  guides(fill=guide_legend(ncol=3, direction = "horizontal"), color = guide_legend(nrow = 3, byrow = TRUE))+
  theme( legend.text = element_text(size=5))

ggsave(plot = p.cum_frac.elec,
       filename =  paste0(plot.dir, "/",reg, "_Cumulative_Elec_Frac.png"),
       width=7, height=8, units="cm", bg = "white")

df.temp <- df.world %>% 
  filter(variable == "Temperature|Global Mean") %>% 
  filter(period < 2085) %>% 
  filter(period > 2025) 

p.temp <- ggplot() +
  geom_line(data=df.temp %>% filter(!scenario %in% c("Baseline")), 
            aes(period, value, color=scenario, linetype=scenario), 
            alpha=.8, width = 6) +
  xlab("") + ylab(paste0("World Mean Temperature (\u00b0C)")) +
  scale_color_manual(name = "", values = scenario_color_mapping) + 
  scale_linetype_manual(name = "", values = scenario_linetype_mapping) + 
  theme_bw() +
  theme(axis.text=element_text(size=8), axis.title=element_text(size=8), strip.text = element_text(size = 8)) +
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(),legend.text = element_text(size=7)) +
  guides(fill=guide_legend(ncol=2, direction = "horizontal"), color = guide_legend(nrow = 1, byrow = TRUE))

ggsave(plot = p.temp, 
       filename =  paste0(plot.dir, "/",reg, "_world_temp.png"),
       width=10, height=10, units="cm", bg = "white")

p.legend <- get_legend(p.cum +
                         theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(),legend.text = element_text(size=6)) +
                         guides(color = guide_legend(nrow = 1, byrow = TRUE))+
                         theme(legend.box.margin = margin(0, 0, 0, 2)))

p.plots <- plot_grid(p.cum +
                 theme(legend.position = "none"),
               p.cum_frac +
                 theme(legend.position = "none"),
               p.temp +
                 theme(legend.position = "none"),
               labels = c("", "", ""),
               label_size = 9,
               nrow = 1)

p <- plot_grid(p.plots,
               # p.legend,
               label_size = 11,
               nrow = 2,
               rel_heights = c(1, 0.2))

ggsave(plot = p, 
       filename =  paste0(plot.dir, "/",reg,"_",scenario_group_name, "_emi_temp.png"),
       width=18, height=7, units="cm", bg = "white")


########################### paper plot 1 ############################


p.legend <- get_legend(p.cum +
                         theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(),legend.text = element_text(size=8), legend.key.size = unit(3, 'cm')) +
                         guides(color = guide_legend(nrow = 1, byrow = TRUE))+
                         theme(legend.box.margin = margin(0, 0, 0, 2)))

p_CI_dummy <- ggplot() +
  geom_line(data = df.plot.ci %>% filter(scenario %in% c("Baseline", "historical")), aes(x = period, y = value, color = scenario, linetype=scenario), size = 1, alpha = 0.85) +
  geom_line(data = df.hist.CI_elec %>% filter(scenario %in% c("Baseline", "historical")), aes(x = period, y = value, color = scenario, linetype=scenario), size = 1) +
  scale_color_manual(name = "", values = scenario_color_mapping.whist) + 
  scale_linetype_manual(name = "", values = scenario_linetype_mapping.whist) 

p.legend2 <- get_legend(p_CI_dummy +
                         theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(),legend.text = element_text(size=8), legend.key.size = unit(0.6, 'cm')) +
                         guides(color = guide_legend(nrow = 1, byrow = TRUE))+
                         theme(legend.box.margin = margin(0, 0, 0, 2)))

p_CI3 <- arrangeGrob(p_CI2 +
                       theme(legend.position = "none") +
                       annotate(geom="text", x=2000, y=800, label="(a)",size=4,family="serif"),
                       p.legend,
                       p.legend2,
                       nrow = 3, 
                       heights=c(0.9,0.1,0.1)
)

p.plot1                <- arrangeGrob(p_CI3, 
                               p.cum +
                                 theme(legend.position = "none") +
                                annotate(geom="text", x=2018, y=330, label="(b)",size=4,family="serif"),  
                               p.cum.elec +
                                 theme(legend.position = "none") +
                                annotate(geom="text", x=2018, y=143, label="(c)",size=4,family="serif"),  
                               p.coalgen_0 +
                                 theme(legend.position = "none") +
                                annotate(geom="text", x=2004, y=20, label="(d)",size=4,family="serif"),  
                               ncol = 2, 
                               widths=c(0.65,0.34),
                               layout_matrix = cbind (c(1,1,1),c(2,3,4))
)

ggsave(plot = p.plot1,
          filename =  paste0(plot.dir, "/","paper_Fig1.png"),
          width=6, height=3.5, bg = "white")

  ################# PE #################

df.pe <- df0 %>% 
  filter(variable %in% var.pe) %>% 
  filter(period <2065) 

for (i in c(1:length(scenarios_mapping))){
  
  scen <- scenarios_mapping[[i]]
  
  df.pe_whist <- list(df.pe, df.hist.pe.tech) %>% 
      reduce(full_join)
    
p.pe <-
  mip::mipArea(df.pe_whist %>% filter(scenario == scen,period >2009, period < 2065), total = F) +
  ylab("Primary Energy [EJ/yr]") +
  xlab("") +
  theme_bw(base_size = 7) + 
  theme(strip.background = element_blank()) +
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(),legend.text = element_text(size=5), legend.key.size = unit(0.5, 'cm')) 

ggsave(plot = p.pe,
       filename =  paste0(plot.dir, "/",reg,"_",scen,"_PE.png"),
       width=8, height=10, units="cm", bg = "white")
  }

# scenarios_mapping_subset = c("fast", "med", "plateau 30")
scenarios_mapping_subset = c("plateau 30", "fast")
# df.hist.pe2 <- df.hist.pe %>% 
  # filter(period >2009)

p.pe <-
mip::mipArea(df.pe_whist %>% filter(scenario %in% scenarios_mapping_subset,period >2009, period < 2065), total = F) +
ylab("Primary Energy [EJ/yr]") +
xlab("") +
theme_bw(base_size = 7) + 
theme(strip.background = element_blank()) +
theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(),legend.text = element_text(size=5), legend.key.size = unit(0.5, 'cm')) 
               
ggsave(plot = p.pe,
       filename =  paste0(plot.dir, "/",reg,"_fastslow_PE.png"),
       width=15, height=8, units="cm", bg = "white")

df.pe <- df0 %>% 
  filter(variable %in% var.pe) %>% 
  filter(period %in% c(2020,2030,2040, 2060)) 

p.pe.compare <- ggplot() +
  geom_bar(data = df.pe %>% 
             filter(scenario== scenarios_mapping[[1]]) %>% 
             mutate(period = period -1), 
           aes(x=period, y=value, fill=variable, linetype=scenario), colour = "black", stat="identity", position="stack", width=1) +
  geom_bar(data = df.pe %>% 
             filter(scenario== scenarios_mapping[[2]]) %>% 
             mutate(period = period), 
           aes(x=period, y=value, fill=variable, linetype=scenario), colour = "black", stat="identity", position="stack", width=1) +
  geom_bar(data = df.pe %>% 
             filter(scenario== scenarios_mapping[[3]]) %>% 
             mutate(period = period +1), 
           aes(x=period, y=value, fill=variable, linetype=scenario), colour = "black", stat="identity", position="stack", width=1) +
  geom_bar(data = df.pe %>% 
             filter(scenario== scenarios_mapping[[4]]) %>% 
             mutate(period = period + 2), 
           aes(x=period, y=value, fill=variable, linetype=scenario), colour = "black", stat="identity", position="stack", width=1) + 
  geom_col(position = position_stack(reverse = TRUE)) +
  scale_fill_manual(name = "", values = pe.color.mapping) +
  theme(legend.title = element_blank()) +
  theme(legend.position="bottom", legend.direction="horizontal", legend.text = element_text(size=10)) +
  guides(fill=guide_legend(nrow=4,byrow=TRUE), linetype=guide_legend(nrow=4,byrow=TRUE))+
  scale_linetype_discrete(breaks=scenarios_mapping) +
  theme(axis.text = element_text(size=10), axis.title = element_text(size= 10, face="bold")) +
  xlab("period") + ylab(paste0("PE")) 

ggsave(plot = p.pe.compare,
       filename =  paste0(plot.dir, "/",reg,"_",scenario_group_name,"_PE_compare.png"),
       width=16, height=12, units="cm", bg = "white")
# }

emivar <- c(
"Emi|CO2|Land-Use Change",
"Emi|CO2|Industrial Processes",
"Emi|CO2|Energy|Demand|Transport",
"Emi|CO2|Energy|Demand|Industry",
"Emi|CO2|Energy|Demand|Buildings",
"Emi|CO2|Energy|Supply|Non-electric",
"Emi|CO2|Energy|Supply|Electricity w/ couple prod",
"Emi|CO2|CDR|DACCS",
"Emi|CO2|CDR|EW")
  
  df.emi <- df0 %>% 
    filter(variable %in% emivar)  %>%
    filter(scenario %in% c(scenarios_mapping[[2]], scenarios_mapping[[5]])) %>% 
    mutate(value = value / 1e3)
  
  p.emi <-
    mip::mipArea(df.emi %>% filter(period >2009, period < 2065), total = F) +
    ylab("CO2 Emmission [Gt/yr]") +
    xlab("") +
    theme_bw(base_size = 7) + 
    theme(strip.background = element_blank()) +
    theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(),legend.text = element_text(size=5), legend.key.size = unit(0.5, 'cm'))  + 
    facet_wrap(~scenario)+
    guides(fill=guide_legend(nrow=4,byrow=TRUE), linetype=guide_legend(nrow=4,byrow=TRUE))
  
  ggsave(plot = p.emi,
         filename =  paste0(plot.dir, "/",reg,"_",scen,"_Emi.png"),
         width=12, height=8, units="cm", bg = "white")

neg.emi.var <- c("Emi|CO2|CDR|Land-Use Change" = "Land-Use Change",
                 "Emi|CO2|CDR|BECCS|Industry" = "BECCS Industry",
                 "Emi|CO2|CDR|BECCS|Pe2Se" = "BECCS Energy carrier production",
                 "Emi|CO2|CDR|DACCS" = "Direct air capture with CCS",
                 "Emi|CO2|CDR|EW" = "Enhanced Weathering",
                 "Emi|CO2|CDR|Industry CCS|Synthetic Fuels" = "Synthetic fuel CCS")
                 
df.neg.emi <- df0 %>% 
  filter(variable %in% names(neg.emi.var))  %>%
  revalue.levels(variable = neg.emi.var) %>%
  filter(scenario %in% c(scenarios_mapping[[2]], scenarios_mapping[[5]]))
# %>% 
  # mutate(value = value / 1e3)

p.neg.emi <-
  mip::mipArea(df.neg.emi %>% filter(period >2009, period < 2065), total = F) +
  ylab("Carbon dioxide removal [Mt/yr]") +
  xlab("") +
  theme_bw(base_size = 7) + 
  theme(strip.background = element_blank()) +
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(),legend.text = element_text(size=5), legend.key.size = unit(0.5, 'cm'))  + 
  facet_wrap(~scenario)+
  guides(fill=guide_legend(nrow=2,byrow=TRUE), linetype=guide_legend(nrow=4,byrow=TRUE)) +
  scale_x_continuous(limits = c(2020, 2060), breaks = c(2020, 2030, 2040, 2050, 2060), minor_breaks = unique(seq(2020, 2060,10))) 

ggsave(plot = p.neg.emi,
       filename =  paste0(plot.dir, "/",reg,"_",scen,"_NegEmi.png"),
       width=11, height=8, units="cm", bg = "white")

############################## AR6 data ###############################
load('/home/chengong/source/ar6data/data/ar6data_CHA.Rds')
load('/home/chengong/source/ar6data/data/ar6scenarios.rda')
tt <- plotstyle.add("ar6-1.5C", "ar6-1.5C", "#009E73", replace=T)
tt <- plotstyle.add("ar6-2C", "ar6-2C", "#882288", replace=T)
sw.reload.ar6 = T

if(sw.reload.ar6){
  # require(ar6data)
  # convert metadata into a more useful format
  ar6meta <- ar6scenarios %>% spread(key = indicator, value = value) 
  
  ar6meta.short <- ar6meta %>% select(model, scenario, Category) %>% 
    factor.data.frame()
  
  varis = c("Price|Carbon", 
            "SE|Electricity|Wind",
            "SE|Electricity|Solar", 
            "SE|Hydrogen",
            "SE|Hydrogen|Electricity", 
            "SE|Electricity|Hydro",
            "SE|Electricity|Non-Biomass Renewables",
            "SE|Hydrogen|Electricity",          
            "SE|Heat|Geothermal",
            "SE|Heat",
            "SE|Electricity",
            "FE|Electricity",
            "FE|Heat",
            "FE|Solar",
            "FE|Geothermal",
            "FE|Hydrogen",
            "FE|Gases",
            "FE|Solids",
            "FE|Liquids",
            "FE|Residential and Commercial|Electricity",
            "FE|Residential and Commercial|Hydrogen",
            "FE|Residential and Commercial",
            "FE|Transportation|Electricity",
            "FE|Transportation|Hydrogen",
            "FE|Transportation",
            "FE|Industry|Electricity",
            "FE|Industry",
            "FE", 
            "PE|Non-Biomass Renewables",
            "PE|Biomass",
            "PE|Oil", 
            "PE|Coal", 
            "Emi|CO2",
            "Emi|CO2|Energy",
            "PE|Gas", 
            "PE",
            "Carbon Sequestration|CCS",
            "Carbon Sequestration|CCS|Biomass",
            "Carbon Sequestration|CCS|Biomass|Energy|Supply|Electricity",
            "Carbon Sequestration|Land Use", 
            "Emi|CO2|Energy|Supply|Electricity",
            NULL)
  
  indicators = c( "Population",
                  "GDP|MER", 
                  "Emi|CO2|Energy and Industrial Processes",
                  "FE",
                  "Share|FE|Electricity",
                  "Share|FE|Non-Bio RE", 
                  # "Share|FE|Electricity|Transport", 
                  # "Share|FE|Electricity|Industry", 
                  # "Share|FE|Electricity|Buildings", 
                  "Share|PE|Fossil",
                  "PE|Biomass",
                  "CDR",
                  "Price|Carbon",
                  NULL)
  
  varis <-c(varis, indicators)
  
  temp.ar6data = ar6data.cha 
  
  # temp.ar6data$variable =  gsub( "Primary Energy","PE",  temp.ar6data$variable)
  levels(temp.ar6data$variable) <-  gsub(  "Primary Energy","PE",  levels(temp.ar6data$variable))
  levels(temp.ar6data$variable) <-  gsub(  "Secondary Energy","SE",  levels(temp.ar6data$variable))
  levels(temp.ar6data$variable) <-  gsub(  "Final Energy", "FE", levels(temp.ar6data$variable))
  levels(temp.ar6data$variable) <-  gsub(  "Emissions", "Emi", levels(temp.ar6data$variable))
  
  temp.ar6data = factor.data.frame(temp.ar6data)
  
  ar6data.wmeta = temp.ar6data  %>%
    filter(region == "R10CHINA+",
           variable %in% varis ) %>% 
    select(-region)
  
  indices = grepl("US\\$2010",ar6data.wmeta$unit)
  ar6data.wmeta[indices, "value"] = ar6data.wmeta[indices, "value"] * 1.1
  levels(ar6data.wmeta$unit) = gsub("US\\$2010", "US\\$2020", levels(ar6data.wmeta$unit))
  
  ar6data.wmeta <- ar6data.wmeta %>% factor.data.frame()
  
  # rm(temp.ar6data)
  
  # fix reporting errors: FE|Solar reports PV in WITCH, IMAGE
  ar6data.wmeta[(grepl("IMAGE", ar6data.wmeta$model) | grepl("IMAGE", ar6data.wmeta$model))  & (ar6data.wmeta$variable == "FE|Solar"), "value"] = 0
  
  ar6data.wmeta = ar6data.wmeta %>%
    calc_addVariable( "`GDP|per capita|MER`" = "`GDP|MER` / `Population`",
                      units  = "Thousand$/capita") %>%
    calc_addVariable( "`Intensity|Final Energy|CO2`" = "`Emi|CO2|Energy and Industrial Processes` / `FE`",
                      units  = "MtCO2/EJ") %>%
    calc_addVariable( "`Intensity|GDP|Final Energy`" = "`FE` / `GDP|MER`",
                      units  = "EJ/Billion$") %>%
    calc_addVariable( "`SE|Electricity|WindSolar`" = "(`SE|Electricity|Wind` + `SE|Electricity|Solar`)",
                      "`SE|Electricity|Non-Bio RE`" = "(`SE|Electricity|Wind` + `SE|Electricity|Solar`)",
                      "`FE|Heat|Non-Bio RE`" ="ifelse(is.na(`SE|Heat|Geothermal`), 0, `FE|Heat` * `SE|Heat|Geothermal` / `SE|Heat`) + ifelse(is.na(`FE|Geothermal`), 0,`FE|Geothermal`) + ifelse(is.na(`FE|Solar`), 0,`FE|Solar`)",
                      units  = "EJ/yr") %>%
    calc_addVariable( "`Share|H2|Electricity`" = "ifelse(is.na(`SE|Hydrogen|Electricity`), 0, `SE|Hydrogen|Electricity` / `SE|Hydrogen` * 100)"  ,
                      "`Share|Electricity|New RE`" = "(`SE|Electricity|Wind` + `SE|Electricity|Solar`) / `SE|Electricity` * 100",
                      
                      "`FE|Electricity|Share`" = "`FE|Electricity` / `FE` * 100",
                      "`Share|FE|Hydrogen`" =  "ifelse(is.na(`FE|Hydrogen`), 0, `FE|Hydrogen`) / `FE` * 100",
                      "`Share|FE|ElecH2`" = "(`FE|Electricity` + `FE|Hydrogen` * `Share|H2|Electricity` /100) / `FE` * 100",
                      "`FE|Transport|Electricity|Share`" = "`FE|Transportation|Electricity` / `FE|Transportation` * 100",
                      "`FE|Industry|Electricity|Share`" = "`FE|Industry|Electricity` / `FE|Industry` * 100",
                      "`Share|FE|ElecH2|Industry`" = "( `FE|Industry|Electricity` + `FE|Hydrogen` * `Share|H2|Electricity`/100) / `FE|Industry` * 100",
                      "`FE|Buildings|Electricity|Share`" = "`FE|Residential and Commercial|Electricity` / `FE|Residential and Commercial` * 100",
                      
                      "`Share|Electricity|New RE`" = "(`SE|Electricity|Wind` + `SE|Electricity|Solar`) / `SE|Electricity` * 100",
                      "`Share|PE|Fossil`" = "(`PE|Coal` + `PE|Gas` + `PE|Oil`) / `PE` * 100",
                      "`Share|PE|Non-Biomass Renewables`" = "`PE|Non-Biomass Renewables` / `PE` * 100",
                      units  = "%") %>%
    
    calc_addVariable( "`Share|Electricity|Non-Bio RE`" = "`SE|Electricity|Non-Biomass Renewables` / `SE|Electricity` * 100",
                      "`Share|Electricity|New RE`" = "(`SE|Electricity|Wind` + `SE|Electricity|Solar`) / `SE|Electricity` * 100",
                      units  = "%") %>%
    calc_addVariable(  "`Share|FE|Non-Bio RE`" = "((`FE|Electricity`  +  ifelse(is.na(`FE|Hydrogen`),0,`FE|Hydrogen`)  * `Share|H2|Electricity`/100)  * `Share|Electricity|Non-Bio RE`/100 +  `FE|Heat|Non-Bio RE`  ) / `FE` * 100", units  = "%") %>%
    # calc_addVariable("`Carbon Intensity|Electricity`" = "(`Emi|CO2|Energy|Supply|Electricity w/ couple prod`) / (`SE|Electricity`) * 3.6", units  = "tCO2/MWh")     %>% 
    left_join(
      ar6meta.short, by = c("model", "scenario")) %>%
    factor.data.frame()
  
  ar6data.wmeta$variable = plyr::mapvalues(ar6data.wmeta$variable, from = c("Primary Energy|Biomass"), to  = c("PE|Biomass"))
  ar6data.wmeta = factor.data.frame(ar6data.wmeta)
  
  save( ar6data.wmeta, file = "df.ar6.rdata" )
} else {
  load( "df.ar6.rdata" )
}

ar6scencats = c("ar6-2C","ar6-1.5C")

elecshare_var <- c("FE|Electricity|Share" = "Total_Elec_Share", 
                   "FE|Transport|Electricity|Share" = "Tran_Elec_Share",
                   "FE|Industry|Electricity|Share" = "Indu_Elec_Share",
                   "FE|Buildings|Electricity|Share" = "Buil_Elec_Share")

ylabels <- c("Electricity Share in Final Energy [%]",
             "Transport Sector Electricity Share in Final Energy [%]",
             "Industry Sector Electricity Share in Final Energy [%]",
             "Buildings Sector Electricity Share in Final Energy [%]",
             NULL)

df.pl.ar6 <- 
  ar6data.wmeta %>%
  filter(
    Category %in% c('C1','C2','C3'),
    variable %in% names(elecshare_var), 
    period %in% c(2005, seq(2010, 2060, 10))) %>% 
  mutate(scenar6 = case_when(Category %in% c('C1','C2') ~ "ar6-1.5C",
                             Category %in% c('C3') ~ "ar6-2C" )) %>%  
  factor.data.frame()  %>% 
  revalue.levels(variable = elecshare_variable_mapping) %>% 
  mutate(variable = factor(variable, levels=elecshare_variable_mapping))

df.pl.ar6.stat <- df.pl.ar6 %>%
  filter( period %in% c(2005, seq(2010, 2060, 10))) %>%
  group_by(period, variable, unit, scenar6) %>%
  calc_quantiles(probs = c(q0 = 0, q10= 0.10, q25 = 0.25, q50 = 0.5, 
                           q75 = 0.75, q90 = 0.9, q100 = 1)) %>%
  ungroup() %>%
  factor.data.frame() %>% 
  spread(key = quantile, value = value) 

df.plot.elecshares = filter(df0, variable %in% names(elecshare_var), period <= 2060 ) %>% 
  revalue.levels(variable = elecshare_variable_mapping) %>% 
  mutate(variable = factor(variable, levels=elecshare_variable_mapping))

p <- ggplot() +
  geom_ribbon(data = filter(df.pl.ar6.stat) , aes(x = period, ymin = q10, ymax = q90, fill = scenar6),  alpha = 0.2) +
  geom_ribbon(data = filter(df.pl.ar6.stat) , aes(x = period, ymin = q0, ymax = q100,fill = scenar6),  alpha = 0.05) +
  geom_line(data = filter(df.pl.ar6,scenar6 == "ar6-2C"), aes(x = period, y = value, group= interaction(model, scenario)), size = 0.12,  color = plotstyle("ar6-2C"), alpha = 0.08 )+
  geom_line(data = filter(df.pl.ar6,scenar6 == "ar6-1.5C"  ), aes(x = period, y = value, group= interaction(model, scenario)), size = 0.12,  color = plotstyle("ar6-1.5C"), alpha = 0.08 )+
  geom_line(data = df.hist.plot.elecshare, aes(x = period, y = value), size = 1,  color = "black") +
  geom_line(data = filter(df.plot.elecshares), aes(x = period, y = value, color = scenario, linetype=scenario), size = 0.5, alpha = 1) +
  geom_point(data = filter(df.plot.elecshares), aes(x = period, y = value, color = scenario, linetype=scenario), size = 0.4) +
  scale_fill_manual( values = plotstyle(levels(df.pl.ar6$scenar6)),
                     labels = plotstyle(levels(df.pl.ar6$scenar6), out = "legend"), name = "") +
  labs(y = ylabels[[1]], x = "") +
  scale_x_continuous(limits = c(1995, 2060), breaks = c(2000, 2020, 2040, 2060),
                     minor_breaks = unique(seq(2000, 2060,10))) +
  theme_bw(base_size = 8) +
  theme(legend.key.width = unit(1,"cm")) +
  scale_color_manual(name = "", values = scenario_color_mapping) + 
  scale_linetype_manual(name = "", values = scenario_linetype_mapping) + 
  facet_wrap(~variable)

ggsave(filename = paste0(plot.dir, "/paper_Fig2.png"),width=14, height=8, units = "cm", bg = "white")

  ##################### discrete AR 6 indicators ###########################
# 
# climscens = 
#   c(  "1.5C",
#       "2C",
#       NULL)
# 
# indicators_a = c("GDP|per capita|MER", 
#                 "Intensity|Final Energy|CO2", 
#                 "Intensity|GDP|Final Energy",
#                 "Share|FE|Non-Bio RE", 
#                 "Share|Electricity|New RE", 
#                 "FE",
#                 "Share|PE|Fossil",
#                 "PE|Biomass",
#                 "CDR",
#                 # "Share|PE|Fossil",
#                 "Price|Carbon",
#                 "Carbon Sequestration|CCS|Biomass",
#                 "Emi|CO2|CDR|BECCS",
#                 NULL)
# 
# df.pl.ar6.test <- ar6data.wmeta %>%
#   filter(variable == "Intensity|GDP|Final Energy")
# 
# plot_ind_t = 2050
# 
# df.pl.ar6 <- ar6data.wmeta %>%
#   filter(Category %in% c('C1', 'C2', 'C3', 'C4'),
#          variable %in% indicators_a, 
#          period == plot_ind_t) %>% 
#   # TODO: merge to model.scen variable
#   unite(model.scen, model, scenario, remove = F) %>% 
#   mutate(scencat=case_when(
#     Category %in% c("C1", "C2") ~ climscens[1],
#     Category %in% c("C3", "C4") ~ climscens[2]),
#          value = case_when(variable %in% c("Carbon Sequestration|CCS|Biomass", "CDR") ~ value/1000,
#                            TRUE ~ value),
#     value = case_when(variable %in% c("Intensity|GDP|Final Energy") ~ value*1000,
#                       TRUE ~ value)) %>%
#   order.levels(variable = indicators_a) %>%       
#   factor.data.frame() 
# 
# df.pl.ar6.test2 <- df.pl.ar6 %>%
#   filter(variable == "Intensity|GDP|Final Energy")
# 
# df.plot = filter(df0, variable %in% indicators_a, period == plot_ind_t) %>%   
#   mutate(value = case_when(variable %in% c("Carbon Management|Carbon Capture", "CDR") ~ value/1000,
#            TRUE ~ value),
#          scencat = strtrim(scenario, 4)) %>%
#   factor.data.frame()
# 
# df.pl.ar6_a <- df.pl.ar6 %>% 
#   filter(variable %in% c("Share|Electricity|New RE", "Share|FE|Non-Bio RE")) %>% 
#   filter(value < 100)
# 
# df.pl.ar6_b <- df.pl.ar6 %>% 
#   filter(variable %in% c("Price|Carbon")) %>% 
#   filter(value < 2000)
# 
# df.pl.ar6_non <- df.pl.ar6 %>% 
#   filter(!variable %in% c("Share|Electricity|New RE", "Share|FE|Non-Bio RE","Price|Carbon"))
# 
# df.pl.ar6 <- list(df.pl.ar6_a, df.pl.ar6_b, df.pl.ar6_non) %>% 
#   reduce(full_join) 
# 
#  p <- ggplot()+
#   geom_violin(data=df.pl.ar6, aes(x=scencat, y=value, fill=scencat), alpha=0.6)+
#   geom_point(data = df.plot, aes( x = scencat, y = value, fill = scencat), stat = "identity", size = 2, shape = 23, linewidth = 2,  alpha = 0.7, color = "black")  +
#   facet_wrap(
#     vars(forcats::fct_recode(variable,  
#                              "GDP MER per capita \n[Thousand$2020]" = "GDP|per capita|MER",
#                              "Energy Intensity\n[EJ/Billion$2020]" = "Intensity|GDP|Final Energy",
#                              "Carbon Intensity\n[tCO2/EJ]" = "Intensity|Final Energy|CO2",
#                              "Electricity\nin FE [%]"= "Share|FE|Electricity",
#                              "Wind/Solar \nin Elec. [%]" = "Share|Electricity|New RE", 
#                              "VRE\n[EJ]" = "SE|Electricity|WindSolar"  ,  
#                              "BECCS\n[GtCO2]" = "Carbon Sequestration|CCS|Biomass",
#                              "CDR\n[GtCO2]" = "CDR",
#                              "CO2 Price\n[$2020/tCO2]" = "Price|Carbon",
#                              "Non-Bio RE\nin FE [%]" = "Share|FE|Non-Bio RE",
#                              "Final\nEnergy [EJ]" = "FE",
#                              "Biomass\n[EJ]" = "PE|Biomass",
#                              "Fossils\nin PE [%]"= "Share|PE|Fossil") ), nrow = 2, scales = "free") +
#   ylim(0,NA) +
#    scale_fill_manual( values = plotstyle(levels(df.pl.ar6$scenar6)),
#                       labels = plotstyle(levels(df.pl.ar6$scenar6), out = "legend"), name = "") +
#   scale_color_manual( values = plotstyle(climscens),
#   labels = plotstyle(climscens, out = "legend"), name = "") +
#    # scale_fill_manual(name = "scencat", values = scenario_color_mapping) + 
#   theme_bw(base_size = 8) +
#   theme(
#     # axis.text.x = element_text(size = 6.5),
#     axis.text.x = element_blank(),
#     legend.position="none",
#     strip.background = element_blank())
# ggsave(filename = paste0(plot.dir, "/ar6comp_Panel2.png"),width=25, height=14, units = "cm")

##################### Steel intensity plot ###########################

# emission which are non electric and non H2 part of primary steel production
emission_nonelec_nonh2_primSteel_lst <- NULL 

linetype_mapping_steel <- c("Primary Steel BF-BOF" = "solid",
                            "Primary Steel H2-DRI-EAF (grid)" = "dotted",
                            "Secondary Steel" = "dashed")

for (i in c(1:length(scenarios_mapping))){
    print(i)
    scen = names(scenarios_mapping_2C)[[i]]
    gdx = paste0(data.dir, "/","fulldata_", scen, ".gdx")
    sm_c_2_co2 <- read.gdx(gdx, "sm_c_2_co2")
    GtC_2_MtCO2 <- sm_c_2_co2 * 1000 
    
    pm_eta_conv <- read.gdx(gdx, "pm_eta_conv") %>% 
      filter(all_regi == "CHA") %>% 
      filter(all_te == "elh2") %>% 
      filter(tall > 2000) %>% 
      filter(tall <= 2060) %>% 
      select(period=tall, eta_conv=value)
    
  emission_primSteel <- file.path(gdx) %>% 
    read.gdx("o37_emiFeNonElecNonH2PrimSteel", squeeze = FALSE) %>%
    filter(ttot < 2070) %>%
    filter(all_regi == reg) %>% 
    mutate(scenario = scen) %>% 
    revalue.levels(scenario = scenarios_mapping) %>%
    mutate(value = value * GtC_2_MtCO2)
    
  emission_nonelec_nonh2_primSteel_lst <- rbind(emission_nonelec_nonh2_primSteel_lst, emission_primSteel)
  
}

for (i in c(1:length(scenario_group_toplot))){
  scenarios_mapping <- scenario_group_toplot[[i]]
  scenario_color_mapping <- scenario_group_color[[i]]
  scenario_color_mapping.whist <- c(scenario_color_mapping, "historical" = "#000000")
  scenario_group_name <- scenario_group_names[[i]]

  data.files = lapply(names(scenarios_mapping), function(x) paste0( data.dir, "/REMIND_generic_", x, "_withoutPlus.mif")) %>% unlist
  
  run_indices = seq(from = 1, to = length(data.files), by = 1)
  df.raw = NULL
  
  # trim df to chosen scenarios and regions
  for (i in run_indices){
    print(i)
    REMIND.data = read.quitte(data.files[[i]])
    df.raw <- rbind(df.raw, REMIND.data)
  }

scens0 = stringr::str_replace(names(scenarios_mapping), "REMIND_generic_", "")
scens = stringr::str_replace(scens0, "-rem-5", "")
# scens = c(  
#             "elec_INT_1150_po_plateau30_noadj_fixbdg",
#             "elec_INT_1150_po_slope_med_noadj_fixbdg",
#             # "elec_INT_1150_po_slope_slo_noadj_fixbdg",
#             # "elec_INT_1150_po_plateau25_noadj_fixbdg",
#             "elec_INT_1150_po_slope_fast_noadj_fixbdg")
scens = c(  
  "elec_INT_1150_po_plateau30_noadj",
  "elec_INT_1150_po_slope_med_noadj",
  # "elec_INT_1150_po_slope_slo_noadj",
  "elec_INT_1150_po_slope_fast_noadj")

df1 = filter(df.raw, scenario %in% scens, region %in% reg ) %>%
  order.levels(scenario = scens) %>%
  revalue.levels(scenario = scenarios_mapping) %>%
  factor.data.frame()
}

df.priceCO2 = filter(df0, variable == "Price|Carbon", period <= 2060 ) %>% 
  select(scenario, period,variable, value)

p.price.elec <- ggplot() + 
  geom_line(data = df.priceCO2, aes(x = period, y = value, color = scenario), size = 1, alpha = 1) +
  geom_point(data = df.priceCO2, aes(x = period, y = value, color = scenario), size = 1.5, alpha = 1) +
  xlab("") + 
  ylab(expression("CO2 price [US$2020/tCO2]")) + 
  theme_minimal_grid(12) +
  scale_color_manual(name = "Scenarios", values = scenario_color_mapping.whist) + 
  theme(legend.position="right", legend.direction="vertical", legend.title = element_blank(), legend.text = element_text(size=6)) + 
  theme_bw() +
  scale_y_continuous(limits = c(0, 1100)) +
  theme(axis.text=element_text(size=8), axis.title=element_text(size=8, face = "bold"), plot.title = element_text(size = 8, face = "bold"), axis.line = element_line(colour = "black")) +
  guides(linetype=guide_legend(nrow=5), color=guide_legend(nrow=5))

ggsave(filename = paste0(plot.dir, "/price_CO2", scenario_group_name, ".png"), p.price.elec, width=16, height=11.5, units = "cm", bg = "white")


df.fe.primsteel.elec = filter(df1, variable == "FE|Industry|Steel|Primary|Electricity", period <= 2060 ) 

df1 <- df1 %>% 
  calc_addVariable("`Carbon Intensity|SE Electricity`" = "(`Emi|CO2|Energy|Supply|Electricity w/ couple prod`) / (`SE|Electricity`)", units = "MtCO2/EJ")  %>% 
  calc_addVariable( "`Emi|Electricity for direct FE H2`" = "(`SE|Input|Electricity|Hydrogen|direct FE H2`) * (`Carbon Intensity|SE Electricity`)", units  = "MtCO2/yr") 
  # calc_addVariable( "`Emi|Electricity for direct FE H2`" = "(`SE|Input|Electricity|Hydrogen|Standard Electrolysis` + `SE|Input|Electricity|Hydrogen|direct FE H2`) * (`Carbon Intensity|SE Electricity`)", units  = "MtCO2/yr") %>%
  # calc_addVariable( "`Carbon Intensity|FE Hydrogen`" = "(`Emi|CO2|Energy|Supply|Hydrogen w/ couple prod` + `Emi|Electricity for direct FE H2`) / (`SE|Hydrogen|Electricity`)", units = "MtCO2/EJ") %>%
  # calc_addVariable( "`Carbon Intensity|FE Hydrogen`" = "(`Emi|CO2|Gross|Energy|Supply|Hydrogen` + `Emi|Electricity for direct FE H2`) / (`SE|Hydrogen|Electricity`)", units = "MtCO2/EJ") %>%
  # calc_addVariable( "`Carbon Intensity|FE Hydrogen`" = "(`Emi|Electricity for direct FE H2`) / (`SE|Hydrogen|Electricity`)", units = "MtCO2/EJ")

df.test24 = filter(df1, variable == "SE|Input|Electricity|Hydrogen|direct FE H2", period <= 2060 ) 

df.se.elec.forFEH2 = filter(df1, variable == "SE|Input|Electricity|Hydrogen|direct FE H2", period <= 2060 ) %>% 
  select(scenario,period,se.elec.forFEH2=value) %>% 
  right_join(pm_eta_conv) %>% 
  mutate(feH2 = se.elec.forFEH2 * eta_conv) %>% 
  select(scenario, period, value=feH2) %>% 
  mutate(model= "REMIND", region = "CHA", unit = "MtCO2/EJ", variable = "FE|Hydrogen|Electricity")

df1 <- rbind(df1, df.se.elec.forFEH2)

df.test20 = filter(df1, variable == "SE|Hydrogen|Electricity", period <= 2060 ) 
df.test22 = filter(df1, variable == "FE|Hydrogen|Electricity", period <= 2060 ) 
df.test23 = filter(df1, variable == "FE|Hydrogen", period <= 2060 ) 

df1 <- df1 %>% 
  calc_addVariable( "`Carbon Intensity|Direct FE green H2`" = "(`Emi|Electricity for direct FE H2`) / (`FE|Hydrogen|Electricity`)", units = "MtCO2/EJ") %>% 
  calc_addVariable( "`Emi|Industry|Steel|Primary|Hydrogen`" = "(`Carbon Intensity|Direct FE green H2`) * (`FE|Industry|Steel|Hydrogen`)", units = "MtCO2") 

# df.test0 = filter(df1, variable == "Emi|CO2|Energy|Supply|Hydrogen w/ couple prod", period <= 2060 )
# df.test = filter(df1, variable == "Emi|CO2|Gross|Energy|Supply|Hydrogen", period <= 2060 ) 
# it goes 
df.test3 = filter(df1, variable == "Carbon Intensity|Direct FE green H2", period <= 2060 ) 

df.test4 = filter(df1, variable == "Emi|Industry|Steel|Primary|Hydrogen", period <= 2060 ) 

df.test14 = filter(df1, variable == "Emi|Electricity for Standard Electrolysis", period <= 2060 ) 

df.test15 = filter(df1, variable == "Emi|Electricity for direct FE H2", period <= 2060 )
# df.test15 = filter(df1, variable == "SE|Input|Electricity|Hydrogen|Standard Electrolysis", period <= 2060 ) 

df.test19 = filter(df1, variable == "FE|Industry|Steel|Hydrogen", period <= 2060 ) 

df.test16 = filter(df1, variable == "Carbon Intensity|SE Electricity", period <= 2060 ) 

# to find out emission intensity of steel per BF-BOF route or per H2 DRI-EAF route, we need to first split the steel output into how many tons of steel are produced via each route, and use that as denominator. To split the steel output we need the FE share and the energy intensity of the route. But since energy intensity of the route is quite similar for BF-BOF and natural gas DRI-EAF (pp 42 of Iron and Steel Technology Roadmap), and assuming natural gas DRI-EAF has the same energy intensity as H2 DRI-EAF, we can assume the two routes share the same energy intensity. This means the FE shares should under approximation reflect the share of the products for each route. 

df1 <- df1 %>% 
  calc_addVariable( "`Production|Industry|Steel|Primary|H2-DRI-EAF`" = "(`Production|Industry|Steel|Primary`) * (`FE|Industry|Steel|Hydrogen`) / (`FE|Industry|Steel|Primary`)", units = "Mt/yr") %>% 
  calc_addVariable( "`Production|Industry|Steel|Primary|BF-BOF`" = "(`Production|Industry|Steel|Primary`) * (`FE|Industry|Steel|Solids`) / (`FE|Industry|Steel|Primary`)", units = "Mt/yr")

df.test6 = filter(df1, variable == "Production|Industry|Steel|Primary|H2-DRI-EAF", period <= 2060 ) 
df.test7 = filter(df1, variable == "Production|Industry|Steel|Primary|BF-BOF", period <= 2060 ) 

#emission intensity of each route 
df1 <- df1 %>% 
  calc_addVariable( "`EmiInt|Industry|Steel|Primary|H2-DRI-EAF`" = "(`Emi|Industry|Steel|Primary|Hydrogen`) / (`Production|Industry|Steel|Primary|H2-DRI-EAF`)", units = "tCO2/t") 

df.test5 = filter(df1, variable == "EmiInt|Industry|Steel|Primary|H2-DRI-EAF", period <= 2060 ) 

df1 <- df1 %>% 
  calc_addVariable( "`Carbon Intensity|FE Electricity`" = "(`Emi|CO2|Energy|Supply|Electricity w/ couple prod`) / (`FE|Electricity`)", units  = "MtCO2/EJ") %>%
  # calc_addVariable( "`Carbon Intensity|FE Electricity`" = "(`Emi|CO2|Gross|Energy|Supply|Electricity`) / (`FE|Electricity`)", units = "MtCO2/EJ") %>% 
  calc_addVariable( "`Emi|Industry|Steel|Primary|Electricity`" = "(`Carbon Intensity|FE Electricity`) * (`FE|Industry|Steel|Primary|Electricity`)", units = "MtCO2") %>% 
  calc_addVariable( "`Emi|Industry|Steel|Secondary|Electricity`" = "(`Carbon Intensity|FE Electricity`) * (`FE|Industry|Steel|Secondary|Electricity`)", units = "MtCO2")

df.test17 = filter(df1, variable == "Carbon Intensity|FE Electricity", period <= 2060 ) 

df.fe.elec.ci = filter(df1, variable == "Carbon Intensity|FE Electricity", period <= 2060 ) %>% #  MtCO2/EJ
          mutate(value = 3.6 * value)      # kgCO2/MWh_el

df.fe.primsteel.elec = filter(df1, variable == "FE|Industry|Steel|Primary|Electricity", period <= 2060 ) 

df.emi.primsteel.elec = filter(df1, variable == "Emi|Industry|Steel|Primary|Electricity", period <= 2060 ) %>% 
  select(scenario, period,emi_prim_elec = value)

df.emi.primsteel.nonelec.nonh2 <- emission_nonelec_nonh2_primSteel_lst %>% 
  # filter(scenario == scen) %>%
  select(scenario, period=ttot, emi_prim_nonelec_nonh2 = value) 

df.emi.primsteel <- list(df.emi.primsteel.nonelec.nonh2, df.emi.primsteel.elec) %>% 
  reduce(full_join) %>% 
  mutate(emi_prim_nonh2 = emi_prim_nonelec_nonh2 + emi_prim_elec) %>% 
  select(scenario, period, emi_prim_nonh2)

# df.primsteel = filter(df1, variable == "Production|Industry|Steel|Primary", period <= 2060, region == reg ) %>% 
#   # filter(period > 2020) %>%
#   select(scenario,period,primsteel = value)

df.primsteel_bof = filter(df1, variable == "Production|Industry|Steel|Primary|BF-BOF", period <= 2060, region == reg ) %>% 
    # filter(period > 2020) %>%
    select(scenario, period, primsteel_bof = value)

df.ci.primsteel_nonh2 <- list(df.primsteel_bof, df.emi.primsteel) %>% 
  reduce(full_join) %>% 
  filter((period < 2055) & (period > 2010) ) %>% 
  mutate(primsteel.ci.nonh2 = emi_prim_nonh2 / primsteel_bof) %>% 
  mutate(steeltype = "Primary Steel BF-BOF") %>% 
  right_join(df.fe.elec.ci  %>% 
               filter((period < 2055) & (period > 2010) ) %>% 
               select(scenario, period, fe_elec_ci=value))

df.ci.primsteel_h2 = filter(df1, variable == "EmiInt|Industry|Steel|Primary|H2-DRI-EAF", period <= 2060 ) %>% 
  select(scenario, period, primsteel.ci.h2 = value) %>% 
  mutate(steeltype = "Primary Steel H2-DRI-EAF (grid)")%>% 
  filter((period < 2055) & (period > 2010)) %>% 
  right_join(df.fe.elec.ci %>% 
               filter((period < 2055) & (period > 2010)) %>% 
               select(scenario, period, fe_elec_ci=value))

df.emi.secsteel.elec = filter(df1, variable == "Emi|Industry|Steel|Secondary|Electricity", period <= 2060 ) %>% 
  select(scenario, period, emi_sec_elec = value)

df.secsteel = filter(df1, variable == "Production|Industry|Steel|Secondary", period <= 2060, region == reg ) %>% 
  select(scenario, period, secsteel = value)

df.ci.secsteel <- list(df.secsteel, df.emi.secsteel.elec) %>% 
  reduce(full_join) %>% 
  filter((period < 2055) & (period > 2010))%>% 
  mutate(secsteel.ci = emi_sec_elec / secsteel) %>% 
  mutate(steeltype = "Secondary Steel") %>% 
  right_join(df.fe.elec.ci %>% 
               filter((period < 2055) & (period > 2010)) %>% 
               select(scenario, period, fe_elec_ci=value))

p.steel.t <- ggplot() + 
  geom_line(data = df.ci.secsteel, aes(x = period, y = secsteel.ci, color = scenario, linetype = steeltype), size = 0.7, alpha = 1) +
  geom_line(data = df.ci.primsteel_h2, aes(x = period, y = primsteel.ci.h2, color = scenario, linetype = steeltype), size = 0.7, alpha = 1) +
  geom_line(data = df.ci.primsteel_nonh2, aes(x = period, y = primsteel.ci.nonh2, color = scenario, linetype = steeltype), size = 0.7, alpha = 1) +
  geom_point(data = df.ci.secsteel , aes(x = period, y = secsteel.ci, color = scenario), size = 1.5, alpha = 1) +
  geom_point(data = df.ci.primsteel_h2, aes(x = period, y = primsteel.ci.h2, color = scenario), size = 1.5, alpha = 1) +
  geom_point(data = df.ci.primsteel_nonh2 , aes(x = period, y = primsteel.ci.nonh2, color = scenario), size = 1.5, alpha = 1) +
  ylab("Steel production emission \n intensity [tCO2/t]") + 
  xlab("") + 
  theme_minimal_grid(12) +
  scale_color_manual(name = "Scenarios", values = scenario_color_mapping.whist) + 
  scale_linetype_manual(name = "", values = linetype_mapping_steel) + 
  theme_bw() +
  theme(legend.position="right", legend.direction="vertical", legend.title = element_blank(), legend.text = element_text(size=7)) + 
  theme(axis.text=element_text(size=8), axis.title=element_text(size=8,face="bold"), plot.title = element_text(size = 8, face = "bold")) +
  theme(axis.line = element_line(colour = "black")) +
  guides(linetype=guide_legend(nrow=5), color=guide_legend(nrow=5))

ggsave(filename = paste0(plot.dir, "/steelInt_vs_t_", scenario_group_name, ".png"), p.steel.t, width=12, height=8, units = "cm", bg = "white")

p.steel.ci <- ggplot() + 
  geom_line(data = df.ci.secsteel, aes(x = fe_elec_ci, y = secsteel.ci, color = scenario, linetype = steeltype), size = 1, alpha = 1) +
  geom_line(data = df.ci.primsteel_h2, aes(x = fe_elec_ci, y = primsteel.ci.h2, color = scenario, linetype = steeltype), size = 1, alpha = 1) +
  geom_line(data = df.ci.primsteel_nonh2, aes(x = fe_elec_ci, y = primsteel.ci.nonh2, color = scenario, linetype = steeltype), size = 1, alpha = 1) +
  geom_point(data = df.ci.secsteel, aes(x = fe_elec_ci, y = secsteel.ci, color = scenario), size = 1.5, alpha = 1) +
  geom_point(data = df.ci.primsteel_h2, aes(x = fe_elec_ci, y = primsteel.ci.h2, color = scenario), size = 1.5, alpha = 1) +
  geom_point(data = df.ci.primsteel_nonh2, aes(x = fe_elec_ci, y = primsteel.ci.nonh2, color = scenario), size = 1.5, alpha = 1) +
  geom_text(data = df.ci.secsteel %>% 
              filter(period %in% c(2015,2020,2030)), aes(x = fe_elec_ci, y = secsteel.ci, color = scenario, label=period), size = 2, alpha = 0.8, hjust=1.5, vjust=1) +
  geom_text(data = df.ci.primsteel_h2 %>% 
              filter(period %in% c(2015,2020,2030)), aes(x = fe_elec_ci, y = primsteel.ci.h2, color = scenario, label=period), size = 2, alpha = 0.8, hjust=1.5, vjust=1) +
  ylab("Steel production emission \n intensity [tCO2/t]") + 
  theme(axis.text=element_text(size=8), axis.title=element_text(size= 8, face="bold"), strip.text = element_text(size=8)) +
  xlab("Power emission intensity [kgCO2/MWh]") + theme_minimal_grid(12) +
  scale_color_manual(name = "Scenarios", values = scenario_color_mapping.whist) + 
  scale_linetype_manual(name = "", values = linetype_mapping_steel) + 
  theme(axis.line = element_line(colour = "black"))+
  theme_bw() +
  guides(linetype=guide_legend(nrow=5), color=guide_legend(nrow=5)) +
  theme(legend.position="right", legend.direction="vertical", legend.title = element_blank(), legend.text = element_text(size=6)) + 
  scale_x_reverse(limits = c(950,0))

ggsave(filename = paste0(plot.dir, "/steelInt_vs_IC_", scenario_group_name, ".png"), p.steel.ci, width=12, height=8, units = "cm", bg = "white")


######################## heat pumps ##################################################

linetype_mapping_heat <- c("Steam boilers" = "solid",
                          "Heat pumps (<100C)" = "dashed",
                          "Heat pumps (100C-200C)" = "dotted",
                          "Coal CHP" = "3313")

df.heat_techs <- df.fe.elec.ci %>%   
  select(scenario, period, fe_elec_ci = value) %>% 
  mutate(heatpump_st100C = fe_elec_ci * 0.27) %>%  #MWh_el/MWh_th for heat pumps (<100C) = 0.27
  mutate(heatpump_gt100C = fe_elec_ci * 0.45) %>%  #MWh_el/MWh_th for heat pumps (100C-200C) = 0.45
  mutate(steamboiler = fe_elec_ci * 1.05)  #MWh_el/MWh_th for steam boilers = 1.05

df.heat_techs.heatpump_st100C = df.heat_techs %>% 
  filter((period <= 2050) & (period > 2010)) %>% 
  select(scenario, period, heatpump_st100C, fe_elec_ci) %>% 
  mutate(heattype = "Heat pumps (<100C)")

df.heat_techs.heatpump_gt100C = df.heat_techs %>% 
  filter((period <= 2050) & (period > 2010)) %>% 
  select(scenario, period, heatpump_gt100C, fe_elec_ci) %>% 
  mutate(heattype = "Heat pumps (100C-200C)")

df.heat_techs.steamboiler = df.heat_techs %>% 
  filter((period <= 2050) & (period > 2010)) %>% 
  select(scenario, period, steamboiler, fe_elec_ci) %>% 
  mutate(heattype = "Steam boilers")

df1 <- df1 %>% 
  calc_addVariable( "`Carbon Intensity|Building Coal`" = "(`Emi|CO2|Energy|Demand|Buildings|Solids`) / (`FE|Buildings|Solids|Fossil`) * 3.6", units = "gCO2/kWh")

df.ci.building.chp = filter(df1, variable == "Carbon Intensity|Building Coal", period <= 2060, region == reg ) %>% 
  select(scenario, period, chp.ci = value) %>% 
  full_join(df.fe.elec.ci %>% 
               filter((period <= 2050) & (period > 2010)) %>% 
               select(scenario, period, fe_elec_ci = value)) %>% 
  replace(is.na(.), 325.7979) %>% 
  mutate(heattype = "Coal CHP")

p.heat.t <- ggplot() + 
  geom_line(data = df.heat_techs.heatpump_st100C, aes(x = period, y = heatpump_st100C, color = scenario, linetype = heattype), size = 1, alpha = 1) +
  geom_line(data = df.heat_techs.heatpump_gt100C, aes(x = period, y = heatpump_gt100C, color = scenario, linetype = heattype), size = 1, alpha = 1) +
  geom_line(data = df.heat_techs.steamboiler, aes(x = period, y = steamboiler, color = scenario, linetype = heattype), size = 1, alpha = 1) +
  geom_line(data = df.ci.building.chp %>% filter(scenario == "fast"), aes(x = period, y = chp.ci, color = scenario, linetype = heattype), size = 1, alpha = 1) +
  geom_point(data = df.heat_techs.heatpump_st100C, aes(x = period, y = heatpump_st100C, color = scenario), size = 1.5, alpha = 1) +
  geom_point(data = df.heat_techs.heatpump_gt100C, aes(x = period, y = heatpump_gt100C, color = scenario), size = 1.5, alpha = 1) +
  geom_point(data = df.heat_techs.steamboiler, aes(x = period, y = steamboiler, color = scenario), size = 1.5, alpha = 1) +
  geom_point(data = df.ci.building.chp, aes(x = period, y = chp.ci, color = scenario), size = 1.5, alpha = 1) +
  ylab("Heat technology emission \n intensity [gCO2/kWh(th)]") + 
  xlab("") + 
  theme_minimal_grid(12) +
  scale_color_manual(name = "Scenarios", values = scenario_color_mapping.whist) + 
  scale_linetype_manual(name = "", values = linetype_mapping_heat) + 
  theme_bw() +
  theme(legend.position="right", legend.direction="vertical", legend.title = element_blank(), legend.text = element_text(size=7)) + 
  theme(axis.text=element_text(size=8), axis.title=element_text(size=8,face="bold"), plot.title = element_text(size = 8, face = "bold")) +
  theme(axis.line = element_line(colour = "black")) +
  guides(linetype=guide_legend(nrow=5), color=guide_legend(nrow=5))

ggsave(filename = paste0(plot.dir, "/heatInt_vs_t", scenario_group_name, ".png"), p.heat.t, width=12, height=8, units = "cm", bg = "white")


p.heat.ci <- ggplot() + 
  geom_line(data = df.heat_techs.heatpump_st100C, aes(x = fe_elec_ci, y = heatpump_st100C, color = scenario, linetype = heattype), size = 1, alpha = 1) +
  geom_line(data = df.heat_techs.heatpump_gt100C, aes(x = fe_elec_ci, y = heatpump_gt100C, color = scenario, linetype = heattype), size = 1, alpha = 1) +
  geom_line(data = df.heat_techs.steamboiler, aes(x = fe_elec_ci, y = steamboiler, color = scenario, linetype = heattype), size = 1, alpha = 1) +
  geom_line(data = df.ci.building.chp %>% filter(scenario == "fast"), aes(x = fe_elec_ci, y = chp.ci, color = scenario, linetype = heattype), size = 1, alpha = 1) +
  geom_point(data = df.heat_techs.heatpump_st100C, aes(x = fe_elec_ci, y = heatpump_st100C, color = scenario), size = 1.5, alpha = 1) +
  geom_point(data = df.heat_techs.heatpump_gt100C, aes(x = fe_elec_ci, y = heatpump_gt100C, color = scenario), size = 1.5, alpha = 1) +
  geom_point(data = df.heat_techs.steamboiler, aes(x = fe_elec_ci, y = steamboiler, color = scenario), size = 1.5, alpha = 1) +
  geom_point(data = df.ci.building.chp%>% filter(scenario == "fast"), aes(x = fe_elec_ci, y = chp.ci, color = scenario, linetype = heattype), size = 1, alpha = 1) +
  geom_text(data = df.heat_techs.heatpump_st100C %>% 
              filter(period %in% c(2010,2020,2030)), aes(x = fe_elec_ci, y = heatpump_st100C, color = scenario, label=period), size = 2, alpha = 0.8, hjust=1.5, vjust=1) +
  geom_text(data = df.heat_techs.heatpump_gt100C%>% 
              filter(period %in% c(2010,2020,2030)), aes(x = fe_elec_ci, y = heatpump_gt100C, color = scenario, label=period), size = 2, alpha = 0.8, hjust=1.5, vjust=1) +
  geom_text(data = df.heat_techs.steamboiler%>% 
              filter(period %in% c(2010,2020,2030)), aes(x = fe_elec_ci, y = steamboiler, color = scenario, label=period), size = 2, alpha = 0.8, hjust=1.5, vjust=1) +
  xlab("Power emission intensity [kgCO2/MWh]") + 
  ylab("Heat technology emission \n intensity [gCO2/kWh(th)]") + 
  theme_minimal_grid(12) +
  scale_color_manual(name = "Scenarios", values = scenario_color_mapping.whist) + 
  scale_linetype_manual(name = "", values = linetype_mapping_heat) + 
  theme(legend.position="right", legend.direction="vertical", legend.title = element_blank(), legend.text = element_text(size=6)) + 
  theme_bw() +
  theme(axis.text=element_text(size=8), axis.title=element_text(size=8,face="bold"), plot.title = element_text(size = 8, face = "bold")) +
  theme(axis.line = element_line(colour = "black")) +
  guides(linetype=guide_legend(nrow=5), color=guide_legend(nrow=5))+
  scale_x_reverse()

ggsave(filename = paste0(plot.dir, "/heatInt_vs_CI", scenario_group_name, ".png"), p.heat.ci, width=12, height=8, units = "cm", bg = "white")

######################## LCA ##################################################
lca_folder = "/LCA_results_update5"
lca.file <- grep("LCA_new_*", list.files(paste0(run_number,lca_folder), full.names = T), value=T)

Activity_map <- c("internal combustion engine " = "ICE", 
                 "battery electric " = "EV")

df.lca <- NULL

for (i in 1:length(lca.file)) {
  # i = 3
  lca.file.name = paste0(run_number, lca_folder, "/LCA_new_", names(scenarios_mapping_nobase)[i], ".csv")
  
  df.lca.in <- read.csv(lca.file.name, sep = ",", header = T, colClasses = c(rep("factor", 3), "numeric", rep("factor", 6), "numeric") ) 
  
  df.lca.out <- df.lca.in %>% 
    select(Activity, Year, CO2.eq.kg.vkm.) %>% 
    revalue.levels(Activity = Activity_map) %>% 
    mutate(scenario = (scenarios_mapping_nobase)[i]) %>% 
    mutate(Year = as.numeric(as.character(Year))) %>% 
    select(period=Year, scenario, cartype = Activity, value = CO2.eq.kg.vkm.)
    
  df.lca <- rbind(df.lca, df.lca.out)
}

df.lca.ice <- df.lca %>% 
  filter(cartype == "ICE") %>% 
  right_join(df.fe.elec.ci %>% 
               filter((period < 2055) & (period > 2010)) %>% 
               select(scenario, period, fe_elec_ci=value))

df.lca.ev <- df.lca %>% 
  filter(cartype == "EV")  %>% 
  right_join(df.fe.elec.ci  %>% 
               filter((period < 2055) & (period > 2010)) %>% 
               select(scenario, period, fe_elec_ci=value))

linetype_mapping_ldv <- c("ICE" = "solid",
                          "EV" = "dashed")

p.ldv.t <- ggplot() + 
  geom_line(data = df.lca.ice, aes(x = period, y = value, color = scenario, linetype = cartype), size = 1, alpha = 1) +
  geom_line(data = df.lca.ev, aes(x = period, y = value, color = scenario, linetype = cartype), size = 1, alpha = 1) + 
  geom_point(data = df.lca.ice, aes(x = period, y = value, color = scenario), size = 1.5, alpha = 1) +
  geom_point(data = df.lca.ev, aes(x = period, y = value, color = scenario), size = 1.5, alpha = 1) +
  xlab("") + 
  ylab("LDV life-cycle emission \n intensity [kgCO2eq/vkm]") + 
  theme_minimal_grid(12) +
  scale_color_manual(name = "Scenarios", values = scenario_color_mapping.whist) + 
  scale_linetype_manual(name = "", values = linetype_mapping_ldv) + 
  theme(legend.position="right", legend.direction="vertical", legend.title = element_blank(), legend.text = element_text(size=8, face = "bold")) + 
  theme_bw() +
  theme(axis.text=element_text(size=8), axis.title=element_text(size=8,face="bold"), plot.title = element_text(size = 8, face = "bold")) +
  theme(axis.line = element_line(colour = "black"))+
  guides(linetype=guide_legend(nrow=5), color=guide_legend(nrow=5))

ggsave(filename = paste0(plot.dir, "/ldvInt_vs_t_", scenario_group_name, ".png"),p.ldv.t, width=12, height=8, units = "cm", bg = "white")

##############################################

p.ldv.ci <- ggplot() + 
  geom_line(data = df.lca.ice, aes(x = fe_elec_ci, y = value, color = scenario, linetype = cartype), size = 1, alpha = 1) +
  geom_line(data = df.lca.ev, aes(x = fe_elec_ci, y = value, color = scenario, linetype = cartype), size = 1, alpha = 1) +
  geom_point(data = df.lca.ice, aes(x = fe_elec_ci, y = value, color = scenario), size = 1.5, alpha = 1) +
  geom_point(data = df.lca.ev, aes(x = fe_elec_ci, y = value, color = scenario), size = 1.5, alpha = 1) +
  geom_text(data = df.lca.ev %>% 
              filter(period %in% c(2015,2020,2025,2030)), aes(x = fe_elec_ci, y = value, color = scenario, label=period), size = 2, alpha = 0.8, hjust=1, vjust=2) +
  xlab("Power emission intensity [kgCO2/MWh]") + 
  ylab("LDV life-cycle emission \n intensity [kgCO2eq/vkm]") + 
  theme_minimal_grid(12) +
  scale_color_manual(name = "Scenarios", values = scenario_color_mapping.whist) + 
  scale_linetype_manual(name = "", values = linetype_mapping_ldv) + 
  theme(legend.position="right", legend.direction="vertical", legend.title = element_blank(), legend.text = element_text(size=8, face = "bold")) + 
  theme_bw() +
  theme(axis.text=element_text(size=8), axis.title=element_text(size=8,face="bold"), plot.title = element_text(size = 8, face = "bold")) +
  theme(axis.line = element_line(colour = "black"))+
  guides(linetype=guide_legend(nrow=5), color=guide_legend(nrow=5))+
  scale_y_continuous(limits = c(0, 0.5)) +
  scale_x_reverse(limits = c(900,0))

ggsave(filename = paste0(plot.dir, "/ldvInt_vs_IC_", scenario_group_name, ".png"),p.ldv.ci, width=12, height=8, units = "cm", bg = "white")

##################### Sector service emission intensity plots #######################
p.legend.steel <- get_legend(p.steel.t +guides(colour = "none")+
                             theme(legend.direction="vertical", legend.title = element_blank(), legend.text = element_text(size=10)) + 
                               theme(legend.box.margin = margin(0, 0, 0, 12)))
p.legend.heat <- get_legend(p.heat.t +guides(colour = "none")+ 
                              theme(legend.direction="vertical", legend.title = element_blank(), legend.text = element_text(size=10)) + 
                              theme(legend.box.margin = margin(0, 0, 0, 12)))
p.legend.ldv <- get_legend(p.ldv.t +guides(colour = "none")+
                             theme(legend.direction="vertical", legend.title = element_blank(), legend.text = element_text(size=10)) + 
                               theme(legend.box.margin = margin(0, 0, 0, 12)))

p.plots <- plot_grid(

  p.steel.t + ggtitle("(a) steel emission intensity vs. time") + 
  theme(legend.position = "none") + theme(aspect.ratio = 1) +
    theme(plot.title = element_text(size = 9, face = "bold",hjust = 0.5)) +
    theme(axis.text=element_text(size=10), axis.title=element_text(size=9,face="bold")) +
    scale_x_continuous(limits = c(2015, 2050), breaks = c(2015, 2020, 2030, 2050),
                       minor_breaks = unique(seq(2000, 2060,10)), ),  
  
  p.heat.t + ggtitle("(b) heat emission intensity vs. time") + 
    theme(legend.position = "none") + theme(aspect.ratio = 1) +
    theme(plot.title = element_text(size = 9, face = "bold",hjust = 0.5)) +
    theme(axis.text=element_text(size=10), axis.title=element_text(size=9,face="bold")) +
    scale_x_continuous(limits = c(2015, 2050), breaks = c(2015, 2020, 2030, 2050),
                       minor_breaks = unique(seq(2000, 2060,10)), ),  
  
  p.ldv.t + ggtitle("(c) passenger car emission intensity vs. time") + 
    theme(legend.position = "none") + theme(aspect.ratio = 1) +
    theme(plot.title = element_text(size = 9, face = "bold",hjust = 0.5)) +
    theme(axis.text=element_text(size=10), axis.title=element_text(size=9,face="bold")) +
    scale_x_continuous(limits = c(2015, 2045), breaks = c(2015, 2020, 2030, 2045),
                       minor_breaks = unique(seq(2000, 2060,10)), ), 
  
  p.steel.ci + ggtitle("(d) steel vs. power emission intensity") + 
    theme(legend.position = "none") + theme(aspect.ratio = 1) +
    theme(plot.title = element_text(size = 9, face = "bold",hjust = 0.5)) +
    theme(axis.text=element_text(size=10), axis.title=element_text(size=9,face="bold")),
  
  p.heat.ci + ggtitle("(e) heat vs. power emission intensity") + 
    theme(legend.position = "none") + theme(aspect.ratio = 1)+
    theme(plot.title = element_text(size = 9, face = "bold",hjust = 0.5)) +
    theme(axis.text=element_text(size=10), axis.title=element_text(size=9,face="bold")) ,
  
  p.ldv.ci + ggtitle("(f) passenger car vs. power emission intensity") + 
    theme(legend.position = "none") + theme(aspect.ratio = 1)+
    theme(plot.title = element_text(size = 9, face = "bold", hjust = 0.5)) +
    theme(axis.text=element_text(size=10), axis.title=element_text(size=9,face="bold")) ,
  
  p.legend.steel,
  p.legend.heat,
  p.legend.ldv,
  
  nrow = 3,
  ncol = 3,
  rel_widths = c(1.1, 1.1, 1.1),
  rel_heights = c(1.1, 1.1, 0.5),
  align = "v", axis = "b")

p.legend.scenarios <- get_legend(p.ldv.t + guides(linetype = "none")+ 
                                                theme(legend.box.margin = margin(0, 0, 0, 12)))

p.6panels <- plot_grid(p.plots,
                       p.legend.scenarios,
               ncol = 2,
               rel_widths = c(1, 0.15))

ggsave(filename = paste0(plot.dir, "/paper_Fig3.png"), p.6panels, width=30.5, height=20, units = "cm", bg = "white")

  ##################### Sector direct and indirect emission plot #######################

df1 <- df1 %>% 
  calc_addVariable( "`Carbon Intensity|FE Electricity`" = "(`Emi|CO2|Energy|Supply|Electricity w/ couple prod`) / (`FE|Electricity`)", units  = "MtCO2/EJ") %>% 
  calc_addVariable( "`Emi|CO2|Industry|Electricity`" = "(`Carbon Intensity|FE Electricity`) * (`FE|Industry|Electricity`)", units = "MtCO2") %>% 
  calc_addVariable( "`Emi|CO2|Buildings|Electricity`" = "(`Carbon Intensity|FE Electricity`) * (`FE|Buildings|Electricity`)", units = "MtCO2") %>% 
  calc_addVariable( "`Emi|CO2|Transport|Electricity`" = "(`Carbon Intensity|FE Electricity`) * (`FE|Transport|Electricity`)", units = "MtCO2")

emi.sec.dir <- c("Emi|CO2|Energy|Demand|Industry",
                 "Emi|CO2|Energy|Demand|Buildings",
                 "Emi|CO2|Energy|Demand|Transport")

emi.sec.ind <- c("Emi|CO2|Industry|Electricity",
                 "Emi|CO2|Buildings|Electricity",
                 "Emi|CO2|Transport|Electricity")

scenario_toplot = c(
  "plateau 30",
  # "plateau 25",
  # "slow",
  "med",
  "fast",
  NULL)

map_sector = c(
  "Industry" = "#0072B2",
  "Buildings" = "#E69F00",
  "Transport" = "#009E73",
  NULL)

emi.sec <- c(emi.sec.dir, emi.sec.ind)

df.dir = filter(df1, variable %in% emi.sec.dir, period <= 2060 ) %>% 
  select(scenario, period, variable, value) %>% 
  mutate(sector = stringr::str_replace(variable, "Emi\\|CO2\\|Energy\\|Demand\\|", "")) %>% 
  select(-variable) %>% 
  mutate(value = value/1e3) %>%
  filter(scenario %in% scenario_toplot) 

df.indir = filter(df1, variable %in% emi.sec.ind, period <= 2060 ) %>% 
  select(scenario, period, variable, value) %>% 
  mutate(sector = stringr::str_replace(variable, "Emi\\|CO2\\|", "")) %>% 
  mutate(sector = stringr::str_replace(sector, "\\|Electricity", "")) %>% 
  select(-variable) %>% 
  mutate(value = value/1e3)  %>%
  filter(scenario %in% scenario_toplot) 

df.tot = filter(df1, variable %in% emi.sec, period <= 2060 ) %>% 
  select(scenario, period, variable, value) %>% 
  dplyr::group_by(scenario, period) %>% 
  dplyr::summarise(value = sum(value), .groups = "keep") %>%
  dplyr::ungroup(scenario, period) 

df.emi.tot = filter(df1, variable == "Emi|CO2", period <= 2060 ) %>% 
  select(scenario, period,variable, value)

p.dir <- ggplot() + 
  geom_bar(data = df.dir %>% 
             filter(period %in% c(2025,2030,2040)), aes(x=scenario, y=value, fill=sector), colour = "black", stat="identity", position="stack",size=0.2, width=0.5) + 
  ylab("Direct CO2 \n emission [GtCO2]") + 
  theme(axis.text=element_text(size=4), axis.title=element_text(size=5,face="bold"), plot.title = element_text(size = 5, face = "bold")) + 
  scale_y_continuous(limits = c(0, 6)) +
  scale_fill_manual(name = "", values = map_sector) + 
  facet_wrap(~period, nrow = 1) 

p.indir <- ggplot() + 
  geom_bar(data = df.indir %>% 
             filter(period %in% c(2025,2030,2040)), aes(x=scenario, y=value, fill=sector), colour = "black", stat="identity", position="stack",size=0.2, width=0.5) + 
  ylab("CO2 emission from \n electricity use [GtCO2]") + 
  theme(axis.text=element_text(size=4), axis.title=element_text(size=5,face="bold"), plot.title = element_text(size = 6, face = "bold")) + 
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(), legend.text = element_text(size=5)) + 
  scale_y_continuous(limits = c(0, 6)) +
  scale_fill_manual(name = "", values = map_sector) + 
  facet_wrap(~period, nrow = 1) 

p.legend <- get_legend(p.indir + theme(legend.box.margin = margin(0, 0, 0, 12)))

p.plots <- plot_grid(
  p.indir + theme(legend.position = "none"),  # Without legend
  p.dir + theme(legend.position = "none"),  # Without legend
  nrow = 2,
  rel_widths = c(1, 1),
  labels = c("(a)", "(b)"),
  label_size = 7,
  align = "v")

# Put together plot and legend
p <- plot_grid(p.plots,
               p.legend,
               nrow = 2,
               rel_heights = c(1, 0.1))

  ggsave(filename = paste0(plot.dir, "/paper_Fig4.png"), p, width=9, height=7, units = "cm", bg = "white")

  indices = grepl("US\\$2005",df0$unit)
  df0[indices, "value"] = df0[indices, "value"] * 1.95
  levels(df0$unit) = gsub("US\\$2005", "US\\$2020", levels(df0$unit))
  
  df.price.elec = filter(df0, variable == "Price|Secondary Energy|Electricity", period <= 2060, period >= 2020 ) %>% 
    mutate(value = 3.6*6.9*value/1e3)%>% 
    mutate(unit = "RMB2020/kWh")
  
  df.price.elec = filter(df0, variable == "Price|Secondary Energy|Electricity|Moving Avg", period <= 2060, period >= 2020 ) %>% 
    mutate(value = 3.6*6.9*value/1e3)%>% 
    mutate(unit = "RMB2020/kWh")
  
  
  p.price.elec <- ggplot() + 
    geom_line(data = df.price.elec, aes(x = period, y = value, color = scenario), size = 1, alpha = 1) +
    geom_point(data = df.price.elec, aes(x = period, y = value, color = scenario), size = 1.5, alpha = 1) +
    xlab("") + 
    ylab(expression("Price of electricity [RMB2020/kWh]")) + 
    theme_minimal_grid(12) +
    scale_color_manual(name = "Scenarios", values = scenario_color_mapping.whist) + 
    scale_linetype_manual(name = "", values = linetype_mapping_heat) + 
    theme(legend.position="right", legend.direction="vertical", legend.title = element_blank(), legend.text = element_text(size=6)) + 
    theme_bw() +
    scale_y_continuous(limits = c(0, 1)) +
    theme(axis.text=element_text(size=8), axis.title=element_text(size=8, face = "bold"), plot.title = element_text(size = 8, face = "bold"), axis.line = element_line(colour = "black")) +
    guides(linetype=guide_legend(nrow=5), color=guide_legend(nrow=5))
  
  ggsave(filename = paste0(plot.dir, "/price_seel", scenario_group_name, ".png"), p.price.elec, width=16, height=11.5, units = "cm", bg = "white")

  
  library(GDPuc) 
  
  my_gdp <- tibble::tibble(
    iso3c = "CHN",
    year = 2005,
    value = 1
  )
  
  convertGDP(
    gdp = my_gdp,
    unit_in = "constant 2005 US$MER",
    unit_out = "constant 2020 US$MER"
  )  
  
  
 
  