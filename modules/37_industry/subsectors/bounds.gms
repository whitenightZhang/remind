*** |  (C) 2006-2024 Potsdam Institute for Climate Impact Research (PIK)
*** |  authors, and contributors see CITATION.cff file. This file is part
*** |  of REMIND and licensed under AGPL-3.0-or-later. Under Section 7 of
*** |  AGPL-3.0, you are granted additional permissions described in the
*** |  REMIND License Exception, version 1.0 (see LICENSE file).
*** |  Contact: remind@pik-potsdam.de
*** SOF ./modules/37_industry/subsectors/bounds.gms

$ifthen.CES_parameters "%CES_parameters%" == "calibrate"
  vm_cesIO.scale(t,regi_dyn29(regi),in_industry_dyn37(in))
  = pm_cesdata(t,regi,in,"quantity");
$endif.CES_parameters

*** Include upper bounds on secondary steel production, due to scarcity of
*** steel scrap.
$ifthen.CES_parameters NOT "%CES_parameters%" == "calibrate"   !! CES_parameters
$ifthen.secondary_steel_bound "%cm_secondary_steel_bound%" == "yearly"
vm_cesIO.up(ttot,regi,"ue_steel_secondary")
  = min(
      vm_cesIO.up(ttot,regi,"ue_steel_secondary"),
      p37_cesIO_up_steel_secondary(ttot,regi,"%cm_GDPpopScen%")
    );
$elseif.secondary_steel_bound "%cm_secondary_steel_bound%" == "scenario"
$ifthen.rcp_scen "%cm_rcp_scen%" == "none"
  !! In no-policy scenarios, tight bounds representing usual scrap recycling
  !! rates apply.  Only 10% of the difference between projected secondary
  !! steel production and the upper bound with increased recycling rates are
  !! available for increased production.
  vm_cesIO.up(t,regi,"ue_steel_secondary")
    = ( ( p37_cesIO_up_steel_secondary(t,regi,"%cm_GDPpopScen%")
        / pm_fedemand(t,regi,"ue_steel_secondary")
        - 1
        )
      / 10
      + 1
      )
    * pm_fedemand(t,regi,"ue_steel_secondary");
$elseif.rcp_scen "%cm_rcp_scen%" == "rcp85"
  !! In no-policy scenarios, tight bounds representing usual scrap recycling
  !! rates apply.  Only 10% of the difference between projected secondary
  !! steel production and the upper bound with increased recycling rates are
  !! available for increased production.
  vm_cesIO.up(t,regi,"ue_steel_secondary")
    = ( ( p37_cesIO_up_steel_secondary(t,regi,"%cm_GDPpopScen%")
        / pm_fedemand(t,regi,"ue_steel_secondary")
        - 1
        )
      / 10
      + 1
      )
    * pm_fedemand(t,regi,"ue_steel_secondary");
$elseif.rcp_scen "%cm_rcp_scen%" == "rcp60"
  !! In no-policy scenarios, tight bounds representing usual scrap recycling
  !! rates apply.  Only 10% of the difference between projected secondary
  !! steel production and the upper bound with increased recycling rates are
  !! available for increased production.
  vm_cesIO.up(t,regi,"ue_steel_secondary")
    = ( ( p37_cesIO_up_steel_secondary(t,regi,"%cm_GDPpopScen%")
        / pm_fedemand(t,regi,"ue_steel_secondary")
        - 1
        )
      / 10
      + 1
      )
    * pm_fedemand(t,regi,"ue_steel_secondary");
$elseif.rcp_scen "%cm_rcp_scen%" == "rcp45"
  !! In no-policy scenarios, tight bounds representing usual scrap recycling
  !! rates apply.  Only 10% of the difference between projected secondary
  !! steel production and the upper bound with increased recycling rates are
  !! available for increased production.
  vm_cesIO.up(t,regi,"ue_steel_secondary")
    = ( ( p37_cesIO_up_steel_secondary(t,regi,"%cm_GDPpopScen%")
        / pm_fedemand(t,regi,"ue_steel_secondary")
        - 1
        )
      / 10
      + 1
      )
    * pm_fedemand(t,regi,"ue_steel_secondary");
$else.rcp_scen
  !! In policy scenarios, secondary steel production can be increased up to the
  !! limit of theoretical scrap availability.
  vm_cesIO.up(t,regi,"ue_steel_secondary")
    = p37_cesIO_up_steel_secondary(t,regi,"%cm_GDPpopScen%");
$endif.rcp_scen
$endif.secondary_steel_bound
$endif.CES_parameters

vm_cesIO.fx("2005",regi,ppfKap_industry_dyn37(in))
  = max(
      pm_cesdata("2005",regi,in,"quantity"),
      abS(pm_cesdata("2005",regi,in,"offset_quantity"))
    );

*** Set lower bound for secondary steel electricity to 1 % of the lowest
*** existing lower bound (should be far above sm_eps) to avoid CONOPT getting
*** lost in the woods.
loop (in$( sameas(in,"feel_steel_secondary") ),
  vm_cesIO.lo(t,regi,in)$(    t.val ge cm_startyear
                          AND vm_cesIO.lo(t,regi,in) le sm_eps )
  = max(
      sm_eps,
      (  0.01
      * smax(ttot$( vm_cesIO.lo(ttot,regi,in) gt sm_eps),
          vm_cesIO.lo(ttot,regi,in)
        )
      ),
      abs(pm_cesdata(t,regi,in,"offset_quantity"))
    );
);

*** Default lower bounds on all industry pfs
vm_cesIO.lo(t,regi_dyn29(regi),in_industry_dyn37(in))$(
                                                  0 eq vm_cesIO.lo(t,regi,in) )
  = max(sm_eps, abs(pm_cesdata(t,regi,in,"offset_quantity")));

*' Limit biomass solids use in industry to 25% (or historic shares, if they are
*' higher) of baseline solids
*' Cement CCS might otherwise become a compelling BioCCS option under very high
*' carbon prices due to missing adjustment costs.
if (cm_startyear gt 2005,   !! not a baseline or NPi scenario
  vm_demFeSector_afterTax.up(t,regi,"sesobio","fesos","indst","ETS")
  = max(0.25 , smax(t2, pm_secBioShare(t2,regi,"fesos","indst") ) )
    * p37_BAU_industry_ETS_solids(t,regi);
);

!! Fix industry output for Bal and EnSec scenario
$if "%cm_indstExogScen%" == "forecast_bal"   $set cm_indstExogScen_set "YES"
$if "%cm_indstExogScen%" == "forecast_ensec" $set cm_indstExogScen_set "YES"
$ifthen.policy_scenario "%cm_indstExogScen_set%" == "YES"
  vm_cesIO.fx(t,regi,in)$( p37_industry_quantity_targets(t,regi,in) )
  = p37_industry_quantity_targets(t,regi,in);
$endif.policy_scenario
$drop cm_indstExogScen_set

v37_regionalWasteIncinerationCCSshare.lo(t,regi) = 0.;
v37_regionalWasteIncinerationCCSshare.up(t,regi) = p37_regionalWasteIncinerationCCSMaxShare(t,regi);

!! fix processes procudction in historic years
if (cm_startyear eq 2005,
    loop((ttot,regi,tePrc2opmoPrc(tePrc,opmoPrc))$(ttot.val ge 2005 AND ttot.val le 2020),
      vm_outflowPrc.fx(ttot,regi,tePrc,opmoPrc) = pm_outflowPrcHist(ttot,regi,tePrc,opmoPrc);
    );
);

!! Switch to turn off all CCS
if (cm_IndCCSscen ne 1,
  vm_cap.fx(t,regi,teCCPrc,rlf) = 0.;
);
!! TOCHECK:Qianzhi
if (cm_CCS_steel ne 1,
  loop(tePrc$(teCCPrc(tePrc) AND secInd37_tePrc("steel", tePrc)),
    vm_cap.fx(t,regi,tePrc,rlf) = 0.;
  );
);
if (cm_CCS_chemicals ne 1,
  loop(tePrc$(teCCPrc(tePrc) AND secInd37_tePrc("chemicals", tePrc)),
    vm_cap.fx(t,regi,tePrc,rlf) = 0.;
  );
);

v37_shareWithCC.lo(t,regi,tePrc,opmoPrc) = 0.;
v37_shareWithCC.up(t,regi,tePrc,opmoPrc) = 1.;

$ifthen.fixedUE_scenario "%cm_fxIndUe%" == "on"

loop ((ue_industry_dyn37(in),regi_groupExt(regi_fxDem37(ext_regi),regi)),
  vm_cesIO.fx(t,regi,in)$( p37_cesIO_baseline(t,regi,in) )
  = p37_cesIO_baseline(t,regi,in);
);
$endif.fixedUE_scenario

!! Fix to avoid reoccurring random infeasibilities. May need to be excluded if e.g. synfuels (or something else) are set to zero.
vm_demFeSector_afterTax.lo(t,regi,entySe,"fesos","indst",emiMkt)$(NOT sameAs(emiMkt, "other")) = 1e-16;
!! vm_demFeSector_afterTax.lo(t,regi,entySe,"fegas","indst",emiMkt)$(NOT sameAs(emiMkt, "other")) = 1e-16;

v37_matShareChange.lo(t,regi,tePrc,opmoPrc,mat)$(tePrcStiffShare(tePrc,opmoPrc,mat)) = -cm_maxIndPrcShareChange;
v37_matShareChange.up(t,regi,tePrc,opmoPrc,mat)$(tePrcStiffShare(tePrc,opmoPrc,mat)) =  cm_maxIndPrcShareChange;

v37_matShareChange.lo(t,regi,tePrc,opmoPrc,mat)$(
    tePrcStiffShare(tePrc,opmoPrc,mat)
    and sameas(regi,'CHA')
    and (t.val >= 2050)
) = -3*cm_maxIndPrcShareChange;

v37_matShareChange.up(t,regi,tePrc,opmoPrc,mat)$(
    tePrcStiffShare(tePrc,opmoPrc,mat)
    and sameas(regi,'CHA')
    and (t.val >= 2050)
) =  3*cm_maxIndPrcShareChange;;

vm_outflowPrc.up(t,regi,"mechRe","standard") = 0.; !! Due to downgraded recycling and pure feedstock limitations


$ifthen.PlasticMFA not "%cm_PlasticMFA%" == "off"
!! not all plastic is suitable for mechanical recycling, so this bound exists apart from the bound imposed 
!! by total availability of plastic scrap
vm_outflowPrc.up(t,regi,"mechRe","standard") = p37_recycleMech(t,regi);
$endif.PlasticMFA

!!vm_outflowPrc.up(t,regi,"stCrChemRe","standard") = 0.01;
!!vm_outflowPrc.up(t,regi,"meSyChemRe","standard") = 0.01;
v37_matFlow.up(t,regi,"plasticWaste") = 0.; !! Due to the limitations of the collection

$ifthen.PlasticMFA not "%cm_PlasticMFA%" == "off"
v37_matFlow.up(t,regi,"plasticWaste") = p37_plasticWaste(t,regi); 
$endif.PlasticMFA

!!!Hot fixes to avoid infeasibilities in the short-term due to new processes with stiff shares
loop((t,regi,tePrc)$(t.val ge 2025),
  vm_cap.lo(t,regi,tePrc,rlf) = 0;
  vm_capEarlyReti.up(t,regi,tePrc) = 1;
  vm_deltaCap.lo(t,regi,tePrc,rlf) = 0;
);

loop((t,regi,tePrc)$(t.val ge 2025
                     AND sum((opmoPrc,mat), tePrcStiffShare(tePrc,opmoPrc,mat))
                     AND (pm_outflowPrcHist("2005",regi,tePrc,"standard") gt 0)),
    vm_deltaCap.lo(t,regi,tePrc,"1") = 1e-8;
    vm_outflowPrc.lo(t,regi,tePrc,"standard") = 1e-7;
);

!! Hotfix 
loop((t,regi)$(t.val ge 2030 AND NOT sameas(regi,"JPN")),
  vm_outflowPrc.lo(t,regi,"meSySol","standard") = 1e-7; !! to avoid infeasibility due to stiff shares
  vm_outflowPrc.lo(t,regi,"meSyBio","standard") = 1e-7; !! to avoid infeasibility due to stiff shares
);

loop(t$(t.val ge 2030),
  vm_deltaCap.up(t,regi,"meSySol","1") = 1e-6;
);

$ifthen.cm_hydroTrade "%cm_hydroTrade%" == "trade"

v37_matflow.up(t,regi,"ammoniaIm")= 0.;
v37_matflow.up(t,regi,"methanolIm")= 0.;

$ifthen.cm_tradeSuffix not "%cm_tradeSuffix%" == "others"

loop((t,regi,tePrc,opmoPrc)$( sameas(tePrc,"amToTrade") AND sameas(opmoPrc,"trade") ),
  vm_outflowPrc.lo(t,regi,tePrc,opmoPrc) = p37_ammoniaEx(t,regi)/1000;
);
loop((t,regi,tePrc,opmoPrc)$( sameas(tePrc,"meToTrade") AND sameas(opmoPrc,"trade") ),
  vm_outflowPrc.lo(t,regi,tePrc,opmoPrc) = p37_methanolEx(t,regi)/1000;
);

loop((t,regi,mat)$( sameas(mat,"ammoniaIm")),
  v37_matflow.up(t,regi,mat) = p37_ammoniaIm(t,regi)/1000;
);
loop((t,regi,mat)$( sameas(mat,"methanolIm")),
  v37_matflow.up(t,regi,mat) = p37_methanolIm(t,regi)/1000;
);

!!! Bottom-up fix for ammonia and methanol transition in China
vm_outflowPrc.up('2030','CHA','amSyCoal','standard') = 42 * 0.8/1000;
vm_outflowPrc.up('2030','CHA','amSyNG','standard') = 42 * 0.2/1000;
vm_outflowPrc.up('2030','CHA','meSySol','standard') = 72 * 0.75/1000;
vm_outflowPrc.up('2030','CHA','meSyNG','standard') = 72 * 0.15/1000;

vm_outflowPrc.up('2035','CHA','amSyCoal','standard') = 26 * 0.8/1000;
vm_outflowPrc.up('2035','CHA','amSyNG','standard') = 26 * 0.2/1000;
vm_outflowPrc.up('2035','CHA','meSySol','standard') = 36 * 0.75/1000;
vm_outflowPrc.up('2035','CHA','meSyNG','standard') = 36 * 0.15/1000;

loop(t$(t.val ge 2040),
vm_outflowPrc.up(t,'CHA','amSyCoal','standard') = 10 * 0.8/1000;
vm_outflowPrc.up(t,'CHA','amSyNG','standard') = 10 * 0.2/1000;
vm_outflowPrc.up(t,'CHA','meSySol','standard') = 1e-6;
vm_outflowPrc.up(t,'CHA','meSyNG','standard') = 1e-6;

);

$endif.cm_tradeSuffix

$ifthen.cm_tradeSuffix "%cm_tradeSuffix%" == "2d"

vm_outflowPrc.fx('2050','CHA','stCrLiq','standard') =  132 * 0.73/1000;
vm_outflowPrc.fx('2055','CHA','stCrLiq','standard') =  132 * 0.55/1000;
vm_outflowPrc.fx('2060','CHA','stCrLiq','standard') =  132 * 0.38/1000;
vm_outflowPrc.up(t,'CHA','stCrLiq','standard')$(t.val >= 2070) = 1e-6;

vm_outflowPrc.fx('2050','CHA','stCrNG','standard') =  15.5* 0.73/1000;
vm_outflowPrc.fx('2055','CHA','stCrNG','standard') =  15.5 * 0.55/1000;
vm_outflowPrc.fx('2060','CHA','stCrNG','standard') =  15.5 * 0.38/1000;
vm_outflowPrc.up(t,'CHA','stCrNG','standard')$(t.val >= 2070) = 1e-6;

$endif.cm_tradeSuffix

$ifthen.cm_tradeSuffix "%cm_tradeSuffix%" == "15d"

vm_outflowPrc.fx('2040','CHA','stCrLiq','standard') =  132 * 0.68/1000;
vm_outflowPrc.fx('2045','CHA','stCrLiq','standard') =  132 * 0.475/1000;
vm_outflowPrc.fx('2050','CHA','stCrLiq','standard') =  132 * 0.29/1000;
vm_outflowPrc.fx('2055','CHA','stCrLiq','standard') =  132 * 0.2/1000;
vm_outflowPrc.fx('2060','CHA','stCrLiq','standard') =  132 * 0.146/1000;
vm_outflowPrc.up(t,'CHA','stCrLiq','standard')$(t.val >= 2070) = 1e-6;

vm_outflowPrc.fx('2040','CHA','stCrNG','standard') =  15.5* 0.68/1000;
vm_outflowPrc.fx('2045','CHA','stCrNG','standard') =  15.5 * 0.475/1000;
vm_outflowPrc.fx('2050','CHA','stCrNG','standard') =  15.5* 0.29/1000;
vm_outflowPrc.fx('2055','CHA','stCrNG','standard') =  15.5 * 0.2/1000;
vm_outflowPrc.fx('2060','CHA','stCrNG','standard') =  15.5 * 0.146/1000;
vm_outflowPrc.up(t,'CHA','stCrNG','standard')$(t.val >= 2070) = 1e-6;

$endif.cm_tradeSuffix

$endif.cm_hydroTrade

*** EOF ./modules/37_industry/subsectors/bounds.gms
