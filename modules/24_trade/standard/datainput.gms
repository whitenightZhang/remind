*** |  (C) 2006-2024 Potsdam Institute for Climate Impact Research (PIK)
*** |  authors, and contributors see CITATION.cff file. This file is part
*** |  of REMIND and licensed under AGPL-3.0-or-later. Under Section 7 of
*** |  AGPL-3.0, you are granted additional permissions described in the
*** |  REMIND License Exception, version 1.0 (see LICENSE file).
*** |  Contact: remind@pik-potsdam.de
*** SOF ./modules/24_trade/standard/datainput.gms


pm_Xport0("2005",regi,peFos) = 0;

*ML* Reintroduction of trade cost for composite good (based on export/import value difference for non-energy goods in GTAP6)
pm_tradecostgood(regi)        = 0.03;

*** load data on transportation costs of imports
parameter pm_costsPEtradeMp(all_regi,all_enty)                   "PE tradecosts (energy losses on import)"
/
$ondelim
$include "./modules/24_trade/standard/input/pm_costsPEtradeMp.cs4r"
$offdelim
/
;


table pm_costsTradePeFinancial(all_regi,char,all_enty)          "PE tradecosts (financial costs on import, export and use)"
$ondelim
$include "./modules/24_trade/standard/input/pm_costsTradePeFinancial.cs3r"
$offdelim
;

*NB* import assumptions for the activation of trade constraints
parameter p24_trade_constraints(all_regi,all_enty,tradeConst)  "parameter for the region specific trade constraints, values different to 1 activate constraints and the value is used as effectiveness to varying degress such as percentage numbers"
/
$ondelim
$include "./modules/24_trade/standard/input/p24_trade_constraints.cs4r"
$offdelim
/
;

display p24_trade_constraints;

pm_costsTradePeFinancial(regi,"XportElasticity", tradePe(enty)) = 100;
pm_costsTradePeFinancial(regi, "tradeFloor", tradePe(enty))     = 0.0125;
pm_costsTradePeFinancial(regi,"Mport","peur")                   = 1e-06;

*** Adjust tradecosts based on switch
pm_costsTradePeFinancial(regi,"Xport", "pebiolc") = pm_costsTradePeFinancial(regi,"Xport", "pebiolc") * cm_tradecostBio;

pm_costsTradePeFinancial(regi,"Xport", "pegas") = 1.5 * pm_costsTradePeFinancial(regi,"Xport", "pegas") ;
pm_costsTradePeFinancial(regi,"XportElasticity","pegas") = 2 * pm_costsTradePeFinancial(regi,"XportElasticity","pegas");

*set trase se prices to zero
pm_MPortsPrice(ttot,regi,tradeSe)=0;
pm_XPortsPrice(ttot,regi,tradeSe)=0;

*** Set trade data file suffix based on budget scenario

$ifthen.cm_hydroTrade "%cm_hydroTrade%" == "trade"

* Convert from trn$US/Gt to T$/TWa
* 1 Gt Ammonia = 18.6 EJ

$ifthen.cm_tradeSuffix not "%cm_tradeSuffix%" == "others"

Parameter
  p24_ammoniaEx(tall,all_regi) ""
  /
$ondelim
$include "./modules/37_industry/subsectors/input/trade/p37_ammonia_fuel_net_export_EJ_%cm_tradeSuffix%.cs4r"
$offdelim
  /
;

Parameter
  p24_ammoniaIm(tall,all_regi) ""
  /
$ondelim
$include "./modules/37_industry/subsectors/input/trade/p37_ammonia_fuel_net_import_EJ_%cm_tradeSuffix%.cs4r"
$offdelim
  /
;

Parameter
  p24_methanolEx(tall,all_regi) ""
  /
$ondelim
$include "./modules/37_industry/subsectors/input/trade/p37_methanol_fuel_net_export_EJ_%cm_tradeSuffix%.cs4r"
$offdelim
  /
;

Parameter
  p24_methanolIm(tall,all_regi) ""
  /
$ondelim
$include "./modules/37_industry/subsectors/input/trade/p37_methanol_fuel_net_import_EJ_%cm_tradeSuffix%.cs4r"
$offdelim
  /
;

Parameter
  p24_amImPrice(tall,all_regi) ""
  /
$ondelim
$include "./modules/37_industry/subsectors/input/trade/p37_ammonia_import_price_%cm_tradeSuffix%.cs4r"
$offdelim
  /
;

Parameter
  p24_meImPrice(tall,all_regi) ""
  /
$ondelim
$include "./modules/37_industry/subsectors/input/trade/p37_methanol_import_price_%cm_tradeSuffix%.cs4r"
$offdelim
  /
;


loop((t,regi,tradeSe)$( sameas(tradeSe,"seh2")),
  pm_MPortsPrice(t,regi,tradeSe) = p24_amImPrice(t,regi)/1000 / 18.6 / sm_EJ_2_TWa;
);

loop((t,regi,tradeSe)$( sameas(tradeSe,"seliqsyn")),
  pm_MPortsPrice(t,regi,tradeSe) = p24_meImPrice(t,regi)/1000 / 19.9 / sm_EJ_2_TWa;
);


$endif.cm_tradeSuffix

$endif.cm_hydroTrade
*** EOF ./modules/24_trade/standard/datainput.gms
