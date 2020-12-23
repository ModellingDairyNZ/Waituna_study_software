$Title A Catchment Problem (CATCH)
$Ontext

This problem finds a least cost method of meeting N and P restrictions ,
using both onfarm mittigations (Farmax+Overseer), plus edge of filed mitigations.

Weeber, Neal, 2016

$Offtext


  Sets
       i   farms                  / WA01, WA02, WA03, WA04, WA05, WA06, WA07, WA08,
                                    WA09, WA10, WA101, WA102, WA103, WA104,
                                    WA105, WA106, WA107, WA11, WA12, WA13, WA14,
                                    WA15, WA16, WA17, WA18, WA19, WA20, WA21, WA22,
                                    WA23, WA24, WA25, WA26, WA27, WA28, WA29,
                                    WA30, WA31, WA32, WA33, WA34, WA35, WA36,
                                    WA37, WA38, WA39, WA40, WA41, WA42, WA43,
                                    WA44, WA45, WA46, WA47, WA48, WA49, WA50, WA51,
                                    WA52, WA53, WA54, WA55, WA56, WA57, WA58,
                                    WA59, WA61, WA62, WA64, WA65, WA66, WA67, WA68,
                                    WA69, WA70, WA71, WA72, WA73, WA74, WA75,
                                    WA76, WA77, WA78, WA79, WA80, WA81, WA83,
                                    WA84, WA85, WA86, WA87, WA88, WA89, WA91,
                                    WA92, WA93, WA94, WA97, WA98, WA99 /
       isub(i)  subset farms      / {farmid} /
*      q   types                  / t3, t6 /
       j   mitigationSteps        / s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11 /
       k   nutrientsOfInterest    / Nitrogen, Phosphorus /
       x   nutrientTargeted       / Nitrogen, Phosphorus /
       xb  nutrienTargetedBinary  / Target /
       m   edgeOfFieldMitigations / m1, m2, m3, m4, m5 /
       p   periods                / JanMar, AprJun, JulSep, OctDec /
       c   catchments             / c1, c2, c3, c4, c5, c6, c7, c8, c9, c10,
                                    c11, c12, c13, c14, c15, c16, c17, c18, c19, c20 /
       b   FSMprofitList          / LandUse, FarmType, FarmArea /
       a   EoFMcostList           / Incost, ConstCost, MaintCost, DepTime /
       d   Bind                   /  Bind       /
       e   Constr                 /  Constr     /
       f   Miti                   /  Miti       /
       g   Cons                   /  Cons       /
       h   Price                  /  Price      /
       t   Tiling                 /  Tiled, Untiled /
       u   Country                /  NewZealand /
       v   InvestmentRate         /  InvestRate /;

  Parameters


********* FSM tables************************************************************

Table AreaProf(i,b) farm specific property area and operating profit
$ondelim
$include   c:\_GAMS\Optimisation\DATA_GAMS_optimisation\AreaProf.csv
$offdelim
display AreaProf;

Table NLoss(i,p) farm specific N leaching over the periods in Kg - Period
$ondelim
$include   c:\_GAMS\Optimisation\DATA_GAMS_optimisation\NLoss.csv
$offdelim
display NLoss;

Table PLoss(i,p) farm specific P leaching over the periods in Kg - Period
$ondelim
$include   c:\_GAMS\Optimisation\DATA_GAMS_optimisation\PLoss.csv
$offdelim
display PLoss;

Table CatchA(i,c) farm specific catchment area per on-farm-catchment in Ha
$ondelim
$include   c:\_GAMS\Optimisation\DATA_GAMS_optimisation\CatchA.csv
$offdelim
display CatchA;

Table CatchFeas(i,c) farm specific EoFM feasible per catchment on farm in binary
$ondelim
$include   c:\_GAMS\Optimisation\DATA_GAMS_optimisation\CatchFeas.csv
$offdelim
display CatchFeas;

Table CatchExist(i,c) Does the catchment exists on farm in binary
$ondelim
$include   c:\_GAMS\Optimisation\DATA_GAMS_optimisation\CatchExist.csv
$offdelim
display CatchExist;

Table CatchW(i,c) farm specific suitable wetland area per on-farm-catchment in Ha
$ondelim
$include   c:\_GAMS\Optimisation\DATA_GAMS_optimisation\CatchW.csv
$offdelim
display CatchW;

Table CatchTiled(i,c) farm specific tiled per on-farm-catchment in binary
$ondelim
$include   c:\_GAMS\Optimisation\DATA_GAMS_optimisation\CatchTiled.csv
$offdelim
display CatchTiled;

Table FSMN_B(i,j) FSM applicable nitrogen focussed reduction steps
$ondelim
$include   c:\_GAMS\Optimisation\DATA_GAMS_optimisation\FSMN_B.csv
$offdelim
display FSMN_B;

Table FSMN_Prof(i,j) FSM Operating profit per hectare under nitrogen focussed reduction steps
$ondelim
$include   c:\_GAMS\Optimisation\DATA_GAMS_optimisation\FSMN_Prof.csv
$offdelim
display FSMN_Prof;

Table FSMN_ProfPer(i,j) FSM Operating profit reduction per hectare under nitrogen focussed reduction steps
$ondelim
$include   c:\_GAMS\Optimisation\DATA_GAMS_optimisation\FSMN_ProfPer.csv
$offdelim
display FSMN_ProfPer;

Table FSMN_Prod(i,j) FSM Production per hectare under nitrogen focussed reduction steps
$ondelim
$include   c:\_GAMS\Optimisation\DATA_GAMS_optimisation\FSMN_Prod.csv
$offdelim
display FSMN_Prod;

Table FSMN_N(i,j) FSM nitrogen reduction under nitrogen focussed reduction steps
$ondelim
$include   c:\_GAMS\Optimisation\DATA_GAMS_optimisation\FSMN_N.csv
$offdelim
display FSMN_N;

Table FSMN_P(i,j) FSM phosphorus reduction under nitrogen focussed reduction steps
$ondelim
$include   c:\_GAMS\Optimisation\DATA_GAMS_optimisation\FSMN_P.csv
$offdelim
display FSMN_P;

Table FSMP_B(i,j) FSM applicable phosphorus focussed reduction steps
$ondelim
$include   c:\_GAMS\Optimisation\DATA_GAMS_optimisation\FSMP_B.csv
$offdelim
display FSMP_B;

Table FSMP_Prof(i,j) FSM Operating profit per hectare under phosphorus focussed reduction steps
$ondelim
$include   c:\_GAMS\Optimisation\DATA_GAMS_optimisation\FSMP_Prof.csv
$offdelim
display FSMP_Prof;

Table FSMP_ProfPer(i,j) FSM Operating profit reduction per hectare under phosphorus focussed reduction steps
$ondelim
$include   c:\_GAMS\Optimisation\DATA_GAMS_optimisation\FSMP_ProfPer.csv
$offdelim
display FSMP_ProfPer;

Table FSMP_Prod(i,j) FSM Production per hectare under phosphorus focussed reduction steps
$ondelim
$include   c:\_GAMS\Optimisation\DATA_GAMS_optimisation\FSMP_Prod.csv
$offdelim
display FSMP_Prod;

Table FSMP_N(i,j) FSM nitrogen reduction under phosphorus focussed reduction steps
$ondelim
$include   c:\_GAMS\Optimisation\DATA_GAMS_optimisation\FSMP_N.csv
$offdelim
display FSMP_N;

Table FSMP_P(i,j) FSM phosphorus reduction under phosphorus focussed reduction steps
$ondelim
$include   c:\_GAMS\Optimisation\DATA_GAMS_optimisation\FSMP_P.csv
$offdelim
display FSMP_P;


********* EoFSM tables**********************************************************

Table EoFMTiledPropTN(m,t) Load proportion of catchment that reaches EoFM depending on tiling as factor
$ondelim
$include   c:\_GAMS\Optimisation\DATA_GAMS_optimisation\EoFMTiledPropTN.csv
$offdelim
display EoFMTiledPropTN;

Table EoFMTiledPropTP(m,t) Load proportion of catchment that reaches EoFM depending on tiling as factor
$ondelim
$include   c:\_GAMS\Optimisation\DATA_GAMS_optimisation\EoFMTiledPropTP.csv
$offdelim
display EoFMTiledPropTP;

Table EoFMCost(m,a) EoFM costs and depreciation over EoFM in NZD and NZD-yr and yr
$ondelim
$include   c:\_GAMS\Optimisation\DATA_GAMS_optimisation\EoFMCost.csv
$offdelim
display EoFMCost;

Table EoFMSloN(m,p) EoFM nitrogen reduction for specific calculated % area and period in %
$ondelim
$include   c:\_GAMS\Optimisation\DATA_GAMS_optimisation\EoFMSloN.csv
$offdelim
display EoFMSloN;

Table EoFMQuadN(m,p) EoFM nitrogen quadratic reduction for specific calculated % area and period in %
$ondelim
$include   c:\_GAMS\Optimisation\DATA_GAMS_optimisation\EoFMQuadN.csv
$offdelim
display EoFMQuadN;

Table EoFMCubN(m,p) EoFM nitrogen cubic reduction for specific calculated % area and period in %
$ondelim
$include   c:\_GAMS\Optimisation\DATA_GAMS_optimisation\EoFMCubN.csv
$offdelim
display EoFMCubN;

Table EoFMQuarN(m,p) EoFM nitrogen quartic reduction for specific calculated % area and period in %
$ondelim
$include   c:\_GAMS\Optimisation\DATA_GAMS_optimisation\EoFMQuarN.csv
$offdelim
display EoFMQuarN;

Table EoFMQuinN(m,p) EoFM nitrogen quintic reduction for specific calculated % area and period in %
$ondelim
$include   c:\_GAMS\Optimisation\DATA_GAMS_optimisation\EoFMQuinN.csv
$offdelim
display EoFMQuinN;

Table EoFMSexN(m,p) EoFM nitrogen sextic reduction for specific calculated % area and period in %
$ondelim
$include   c:\_GAMS\Optimisation\DATA_GAMS_optimisation\EoFMSexN.csv
$offdelim
display EoFMSexN;

Table EoFMSloP(m,p) EoFM phosphorus reduction for specific calculated % area and period in %
$ondelim
$include   c:\_GAMS\Optimisation\DATA_GAMS_optimisation\EoFMSloP.csv
$offdelim
display EoFMSloP;

Table EoFMQuadP(m,p) EoFM phosphorus quadratic reduction for specific calculated % area and period in %
$ondelim
$include   c:\_GAMS\Optimisation\DATA_GAMS_optimisation\EoFMQuadP.csv
$offdelim
display EoFMQuadP;

Table EoFMCubP(m,p) EoFM phosphorus cubic reduction for specific calculated % area and period in %
$ondelim
$include   c:\_GAMS\Optimisation\DATA_GAMS_optimisation\EoFMCubP.csv
$offdelim
display EoFMCubP;

Table EoFMQuarP(m,p) EoFM phosphorus quartic reduction for specific calculated % area and period in %
$ondelim
$include   c:\_GAMS\Optimisation\DATA_GAMS_optimisation\EoFMQuarP.csv
$offdelim
display EoFMQuarP;

Table EoFMQuinP(m,p) EoFM phosphorus quintic reduction for specific calculated % area and period in %
$ondelim
$include   c:\_GAMS\Optimisation\DATA_GAMS_optimisation\EoFMQuinP.csv
$offdelim
display EoFMQuinP;

Table EoFMSexP(m,p) EoFM phosphorus sextic reduction for specific calculated % area and period in %
$ondelim
$include   c:\_GAMS\Optimisation\DATA_GAMS_optimisation\EoFMSexP.csv
$offdelim
display EoFMSexP;

Table EoFMCon(m,g) Specific calculated % area and constraint in %
$ondelim
$include   c:\_GAMS\Optimisation\DATA_GAMS_optimisation\EoFMCon.csv
$offdelim
display EoFMCon;

********* Model tables**********************************************************
Table FSMPrice(i,h) Current price for milksolid etc.
$ondelim
$include   c:\_GAMS\Optimisation\DATA_GAMS_optimisation\FSMPrice.csv
$offdelim
display FSMPrice;

Table FSMPriceOld(i,h) Price for milksolid during OVERSEER scenarios etc.
$ondelim
$include   c:\_GAMS\Optimisation\DATA_GAMS_optimisation\FSMPriceOld.csv
$offdelim
display FSMPriceOld;

Table InvestRate(u,v) Investment rate for costs
$ondelim
$include   c:\_GAMS\Optimisation\DATA_GAMS_optimisation\InvestRate.csv
$offdelim
display InvestRate;



*Scales
  Scalar msqrperha  m to ha conversion  /10000/ ;
  Scalar kgpert     kg to tonne conversion / 1000/ ;

Parameter  BaseNL(i,c,p) Base N loss  ;

       BaseNL(i,c,p) = CatchA(i,c) * Nloss(i,p) * CatchExist(i,c);
display BaseNL;

Parameter BaseNLSum(i)  Base N loss per farm;
         BaseNLSum(i) = sum(c,sum(p,BaseNL(i,c,p)));
*                        yearly load
display BaseNLSum;

Parameter  BasePL(i,c,p) Base P loss  ;
       BasePL(i,c,p) = CatchA(i,c) * Ploss(i,p) * CatchExist(i,c);
display BasePL;

Parameter  BasePLSum(i) Base P loss per farm;
        BasePLSum(i) = sum(c,sum(p,BasePL(i,c,p)));
*                        yearly load
display BasePLSum;

Parameter EoFMVarCost(m) EoFM variable cost over mitigation;
       EoFMVarCost(m) = EoFMCost(m,'MaintCost') + (EoFMCost(m,'ConstCost') / EoFMCost(m,'DepTime'));
display EoFMVarCost;
* Currently straight line depreciation and not annualised with a positive discount rate

Parameter EoFMInYr(m) EoFM Initial cost over time;
       EoFMInYr(m) = EoFMCost(m,'InCost') / EoFMCost(m,'DepTime');
display EoFMInYr;
* Currently straight line depreciation and not annualised with a positive discount rate

Parameter FSMN_Exp(i,j) FSM expenses when nitrogen focussed calculated from the OP and profit;
       FSMN_Exp(i,j) = (FSMN_B(i,j) * FSMN_Prod(i,j) * AreaProf(i,'FarmArea')* FSMPriceOld(i,'Price')) - (FSMN_B(i,j) * AreaProf(i,'FarmArea')*FSMN_Prof(i,j));
display FSMN_Exp;

Parameter FSMP_Exp(i,j) FSM expenses when phosphorus focussed calculated from the OP and profit;
       FSMP_Exp(i,j) = (FSMP_B(i,j) * FSMP_Prod(i,j) * AreaProf(i,'FarmArea')* FSMPriceOld(i,'Price')) - (FSMP_B(i,j) * AreaProf(i,'FarmArea')*FSMP_Prof(i,j));
display FSMP_Exp;

Parameter FSMN_BN(i,j) Feasible FSM nitrogen reduction when nitrogen focussed;
       FSMN_BN(i,j) = FSMN_B(i,j) * FSMN_N(i,j);
display FSMN_BN;

Parameter FSMN_BP(i,j) Feasible FSM phosphorus reduction when nitrogen focussed;
       FSMN_BP(i,j) = FSMN_B(i,j) * FSMN_P(i,j);
display FSMN_BP;

Parameter FSMP_BN(i,j) Feasible FSM nitrogen reduction when phosphorus focussed;
       FSMP_BN(i,j) = FSMP_B(i,j) * FSMP_N(i,j);
display FSMP_BN;

Parameter FSMP_BP(i,j) Feasible FSM phosphorus reduction when phosphorus focussed;
       FSMP_BP(i,j) = FSMP_B(i,j) * FSMP_P(i,j);
display FSMP_BP;

Parameter FSMN_BProd(i,j) Feasible FSM production when nitrogen focussed;
       FSMN_BProd(i,j) = FSMN_B(i,j) * FSMN_Prod(i,j);
display FSMN_BProd;

Parameter FSMP_BProd(i,j) Feasible FSM production when phosphorus focussed;
       FSMP_BProd(i,j) = FSMP_B(i,j) * FSMP_Prod(i,j);
display FSMP_BProd;

Parameter FSMN_BExp(i,j) Feasible FSM expenses when nitrogen focussed;
       FSMN_BExp(i,j) = FSMN_B(i,j) * FSMN_Exp(i,j);
display FSMN_BExp;

Parameter FSMP_BExp(i,j) Feasible FSM expenses when phosphorus focussed;
       FSMP_BExp(i,j) = FSMP_B(i,j) * FSMP_Exp(i,j);
display FSMP_BExp;

Parameter NPrice Price per Kg nitrogen;
       NPrice = {nprice};
display NPrice;

Parameter PPrice Price per Kg phosphorus;
       PPrice = {pprice};
display PPrice;

Parameter PropEoFMTN(isub,c,m) Proportion of the cacthment load that reaches the EoFM;
         PropEoFMTN(isub,c,m) = CatchTiled(isub,c) * EoFMTiledPropTN(m,"Tiled") +
         (1-CatchTiled(isub,c)) * EoFMTiledPropTN(m,"Untiled");
display PropEoFMTN;

Parameter PropEoFMTP(isub,c,m) Proportion of the catchment load that reaches the EoFM;
         PropEoFMTP(isub,c,m) = CatchTiled(isub,c) * EoFMTiledPropTP(m,"Tiled") +
         (1-CatchTiled(isub,c)) * EoFMTiledPropTP(m,"Untiled");
display PropEoFMTP;


$ontext
Parameter EoFMApp(i,c,m) Eofm;
         EoFMApp(i,c,m)=0;
         EoFMApp(i,'c2','m2') = 1;
display EoFMApp;
$offtext

 Variables
       FSMRev(isub)     revenues per farm from FSM
       FSMExp(isub)     expenses per farm from FSM
       EoFMCosts(isub)  costs associated with EoFM
       EoFMApp(isub,c,m)  Edge of field mitigations applied per catchment and farm
       EoFMPropSize(isub,c,m) proportional size of the EoFM to the catchment
       FSMTargetVB(isub)   Binary variable declaring which farm targets what nutrients (1 = N 0 = P)
       EoFMSize(isub,c,m) Size of the EoFM per catchment and farm
       EoFMSizeF(isub,c,m) Factor for the mitigation size to deal with catchment size is 0
       EoFMInvC(isub,c,m) Investment cost of EoFM
       mit(isub,j,x)  Mitigation steps taken for FSM
       farmN(isub) N load per farm
       NKgHa(isub) N load per hectare on farm
       NLRedFSM(isub) N load removed by FSM
       NLRedEoFM(isub) N load removed by EoFM
       farmP(isub) P load per farm
       PKgHa(isub) P load per hectare on farm
       PLRedFSM(isub) P load removed by FSM
       PLRedEoFM(isub) P load removed by EoFM
       z       total EBIT dollars including shadowprice
       zact    total EBIT dollars

  Binary Variable EoFMApp
  Binary Variable FSMTargetVB
  Positive Variable EoFMSize
  Positive Variable EoFMPropSize;

  Binary Variable mit ;


  Positive Variable RedFacFSMN ;
  Positive Variable RedFacEoFMN ;


  Positive Variable RedFacFSMP ;
  Positive Variable RedFacEoFMP ;



  Equations
       ebitact         calculate ebit
       ebit            define objective function
       FSMRevC         calculate revenues from FSM
       FSMExpC         calculate expenses from FSM
       EoFMCC          calculate EoFM costs
       EoFMInvestCost  calculate EoFM investment costs
       EoFMSizeFC      Constraint for mitigation size vs catchment area
       EoFMWetSizeC    Constraint for mitigation size on wetland
       EoFPropSizeC    proprotional size of eofm to catchment area
       mitStepLimit    constraint on mit variable per step
       EoFMAppLimit    constraint on EoFMApp variable
       RedFacFSMNC     Reduction Factor of mitigations per catchment for Nitrogen
       RedFacEoFMNC    Reduction Factor of mitigations per catchment for Nitrogen
       NLoadCalc       Nitrogen load calculation
       NLoadRFSM       Nitrogen load removed by FSM
       NLoadREoFM      Nitrogen load removed by EoFM
       NKgHaCalc       Nitrogen load per Hectare
       RedFacFSMPC     Reduction Factor of mitigations per catchment for Phosphorus
       RedFacEoFMPC    Reduction Factor of mitigations per catchment for Phosphorus
       PLoadCalc       Phosphorus load calculation
       PLoadRFSM       Phosphorus load removed by FSM
       PloadREoFM      Phosphorus load removed by EoFM
       PKgHaCalc       Phosphorus load per Hectare;



   ebitact ..        zact =e=                   sum(isub, FSMRev(isub)- FSMExp(isub)- EoFMCosts(isub));
   ebit ..           z  =l=                     sum(isub, FSMRev(isub)- FSMExp(isub)- EoFMCosts(isub)) - sum(isub,NPrice * farmN(isub)) - sum(isub,PPrice * farmP(isub));

****FINANSIALS

*  FSM financials
   FSMRevC(isub)..      FSMRev(isub) =l=              AreaProf(isub,'FarmArea')* FSMPrice(isub,'Price') * sum(j,mit(isub,j,'Nitrogen') *  FSMN_BProd(isub,j))
                                              + AreaProf(isub,'FarmArea')* FSMPrice(isub,'Price') * sum(j,mit(isub,j,'Phosphorus') *  FSMP_BProd(isub,j));
   FSMExpC(isub)..      FSMExp(isub) =g=              sum(j,mit(isub,j,'Nitrogen')* FSMN_BExp(isub,j) + mit(isub,j,'Phosphorus')* FSMP_BExp(isub,j));

*  EoFM financials
   EoFMCC(isub)..                    EoFMCosts(isub)  =g=   sum(c,sum(m, (EoFMInYr(m) * EoFMApp(isub,c,m) + EoFMVarCost(m) * EoFMSize(isub,c,m) + EoFMInvC(isub,c,m))* EoFMApp(isub,c,m)) );
   EoFMInvestCost(isub,c,m)..        EoFMInvC(isub,c,m) =e= ((EoFMCost(m,'ConstCost') * EoFMSize(isub,c,m) + EoFMCost(m,'InCost') * EoFMApp(isub,c,m)) * InvestRate('NewZealand','InvestRate')); 
   EoFMSizeFC(isub,c,m)..            EoFMPropSize(isub,c,m) =l= EoFMCon(m,'Cons')* EoFMApp(isub,c,m)* CatchFeas(isub,c) ;
   EoFMWetSizeC(isub,c,"m1")..       EoFMSize(isub,c,"m1")  =l= CatchW(isub,c) * EoFMApp(isub,c,"m1");
   EoFPropSizeC(isub,c,m)..          (EoFMSize(isub,c,m)/CatchA(isub,c)) =e= EoFMPropSize(isub,c,m);

*  Limits of application
   mitStepLimit(isub)..              sum(j,sum(x,mit(isub,j,x))) =l= 1 ;
   EoFMAppLimit(isub,c)..            sum(m,EoFMApp(isub,c,m)) =l= 1 * CatchFeas(isub,c);

****NITROGEN

   RedFacFSMNC(isub) ..        (1- sum(j, (FSMN_BN(isub,j)* mit(isub,j,'Nitrogen') +
                           FSMP_BN(isub,j)* mit(isub,j,'Phosphorus')))) =e= RedFacFSMN(isub);

*   RedFacEoFMNC(isub,c,p)..    prod(m, 1 - ((EoFMSloN(m,p) * EoFMPropSize(isub,c,m)) +
*                                    (EoFMQuadN(m,p) * power(EoFMPropSize(isub,c,m),2)) + (EoFMCubN(m,p) * power(EoFMPropSize(isub,c,m),3)) +
*                                    (EoFMQuarN(m,p) * power(EoFMPropSize(isub,c,m),4)) + (EoFMQuinN(m,p) * power(EoFMPropSize(isub,c,m),5)) +
*                                    (EoFMSexN(m,p) * power(EoFMPropSize(isub,c,m),6))  ))    =e= RedFacEoFMN(isub,c,p);

   RedFacEoFMNC(isub,c,p)..    prod(m, 1 - ((EoFMSloN(m,p) * EoFMPropSize(isub,c,m)) +
                                    (EoFMQuadN(m,p) * power(EoFMPropSize(isub,c,m),2)) + (EoFMCubN(m,p) * power(EoFMPropSize(isub,c,m),3)) ))    =e= RedFacEoFMN(isub,c,p);



   NLoadCalc(isub)..         sum(c,sum(p, BaseNL(isub,c,p) * RedFacFSMN(isub) * (1 - sum(m,PropEoFMTN(isub,c,m) * EoFMApp(isub,c,m)) * (1 - RedFacEoFMN(isub,c,p))) ) ) =e= farmN(isub);

   NLoadRFSM(isub)..         sum(c,sum(p, BaseNL(isub,c,p) * (1-RedFacFSMN(isub)))) =e= NLRedFSM(isub);
   NLoadREoFM(isub)..        sum(c,sum(p, BaseNL(isub,c,p) * RedFacFSMN(isub) * (sum(m,PropEoFMTN(isub,c,m) * EoFMApp(isub,c,m)) * (1 - RedFacEoFMN(isub,c,p))))) =e= NLRedEoFM(isub);
   NKgHaCalc(isub)..         NKgHa(isub) =e= farmN(isub) / AreaProf(isub,'FarmArea');

****PHOSPHORUS

  RedFacFSMPC(isub) ..        (1- sum(j, (FSMN_BP(isub,j)* mit(isub,j,'Nitrogen') +
                           FSMP_BP(isub,j)* mit(isub,j,'Phosphorus')))) =e= RedFacFSMP(isub);

*  RedFacEoFMPC(isub,c,p)..    prod(m, 1 - ((EoFMSloP(m,p) * (EoFMPropSize(isub,c,m)) ) +
*                                    (EoFMQuadP(m,p) * power(EoFMPropSize(isub,c,m),2)) + (EoFMCubP(m,p) * power(EoFMPropSize(isub,c,m),3)) +
*                                    (EoFMQuarP(m,p) * power(EoFMPropSize(isub,c,m),4)) + (EoFMQuinP(m,p) * power(EoFMPropSize(isub,c,m),5)) +
*                                    (EoFMSexP(m,p) * power(EoFMPropSize(isub,c,m),6))  ))      =e= RedFacEoFMP(isub,c,p);

  RedFacEoFMPC(isub,c,p)..    prod(m, 1 - ((EoFMSloP(m,p) * (EoFMPropSize(isub,c,m)) ) +
                                    (EoFMQuadP(m,p) * power(EoFMPropSize(isub,c,m),2)) + (EoFMCubP(m,p) * power(EoFMPropSize(isub,c,m),3))  ))      =e= RedFacEoFMP(isub,c,p);


  PLoadCalc(isub)..         sum(c,sum(p, BasePL(isub,c,p) * RedFacFSMP(isub) * (1 - sum(m,PropEoFMTP(isub,c,m) * EoFMApp(isub,c,m)) * (1 - RedFacEoFMP(isub,c,p))) ) ) =e= farmP(isub);

  PLoadRFSM(isub)..         sum(c,sum(p, BasePL(isub,c,p) * (1-RedFacFSMP(isub)))) =e= PLRedFSM(isub);
  PLoadREoFM(isub)..        sum(c,sum(p, BasePL(isub,c,p) * RedFacFSMP(isub) * (sum(m,PropEoFMTP(isub,c,m) * EoFMApp(isub,c,m)) * (1 - RedFacEoFMP(isub,c,p))))) =e= PLRedEoFM(isub);
  PKgHaCalc(isub)..         PKgHa(isub) =e= farmP(isub) / AreaProf(isub,'FarmArea');

  Model CatchLP /all/ ;
*  option Savepoint=1;
* Set a longer solve time as limitation was reached (default = 1000.0)
*  option reslim=50000.0
*  Solve CatchLP using rminlp maximizing z ;
*  option Savepoint=1;
*  Execute_loadpoint 'Backup.gdx'
*SOLVED WITH COUNNE
  Solve CatchLP using minlp maximizing z ;

*solve model using lp or dnlp (save basis levels into file) Target (as prarmeter)

*solve model using mip (include levels from file)  Target as variable (initial values from parameter)


* if we keep integer/binary, need to change from lp to?

*  display mit.l;
*  Display mit.l, farmN.l, farmP.l;
*$Offtext
