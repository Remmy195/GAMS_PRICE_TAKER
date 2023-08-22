$title Multi-stage Stochastic Water Reservoir Model solved with SDDP
$eolcom //

Sets w          Weeks                              / w1*w52   /
     t          Hours of the year                  / t1*t8736 /
     ft         Types of fuel available (plants)   / Hydro, HardCoal, Nuclear /

Parameters
     demand(t)       Power demand MW
     exchange(t)     Cross border flow MW
     wCapacity(w,ft) Capacity of plant MW
     wPrices(w,*)    Coal and CO2 price EUR per ton
     wInflow(w)      Inflows into the reservoir MWh per week
     resmax(t)       Max reservoir level MW / t1*t8735 106.2e6, t8736 87e6 /
     resmin(t)       Min reservoir level MW / t1*t8735    10e6, t8736 87e6 /;

$onecho > gdxxrw.txt
par=demand    rng=t_Demand!a2          Rdim=1
par=exchange  rng=t_Flow!a2            Rdim=1
par=wCapacity rng=w_Capacity!a2 Rdim=1 Cdim=1
par=wPrices   rng=w_Prices!a2   Rdim=1 Cdim=1
par=wInflow   rng=w_Inflow!a2          Rdim=1
par=sw_Inflow rng=sw_Inflow!a2  Rdim=1 Cdim=1
$offecho

$set skipExcel 1
$if not %system.filesys% == UNIX $set skipExcel 0
$if %skipExcel%==0 $call 'gdxxrw.exe water.xlsx o=water.gdx @gdxxrw.txt'
$if %skipExcel%==0 $if errorlevel 1 $abort 'Problems reading water.xls'

$gdxin water.gdx
$load demand exchange wCapacity wPrices wInflow

* Week hour mapping
Set  h               Hours of one week / h1*h168 /
     wt(w,t)         Map weeks and hours
     last(w,t)       Last hour t of each week w
     first(w,t)      First hour t of each week w;

loop(w,
  loop((h,t)$sameas('t1',t), wt(w,t+((ord(w)-1)*card(h)+ord(h)-1)) = yes);
  loop(t$sameas('t1',t),
    first(w,t+((ord(w)-1)*card(h))) = yes;
    last(w,t+(ord(w)*card(h)-1)) = yes));

Parameter
     capacity(t,ft)  Capacity of plant MW
     gencost(t,ft)   Generating cost EUR per MW / #t.Hydro 0, #t.Nuclear 15 /
     inflow(t)       Inflows into the reservoir MW;

inflow(t)             = sum(wt(w,t), wInflow(w)/card(h));
capacity(t,ft)        = sum(wt(w,t), wCapacity(w,ft));
gencost(t,'HardCoal') = sum(wt(w,t), (wPrices(w,'HardCoal') + 2.361*wPrices(w,'CO2')))/(6.98*0.39);

Set  tt(t)         Control set for hours
     ww(w)         Control set for weeks;

Positive Variables
     GAP(t)         Gap generation in hour t MW
     RES(t)         Reservoir energy (level) at end of t MW
     SPILL(t)       Spillage in hour t MW
     X(t,ft)        Generated power in hour t by plant with fuel type MW
     SLACKUP(t)     Slack variable for the upper reservoir level equation MW
     SLACKLO(t)     Slack variable for the lower reservoir level equation MW;
Variable
     COST           Total generation cost EUR;

Equations
     Cont(t)        Hydraulic continuity equation
     DemSat(t)      Demand satisfaction
     ResUp(t)       Maximum reservoir level
     ResLo(t)       Minimum reservoir level
     PlantCap(t,ft) Plant power generation capacity
     Obj            Objective Function;

Cont(tt(t))..
     RES(t) - RES(t--1) + X(t,'Hydro') + SPILL(t) =e= inflow(t);

DemSat(tt(t))..
     sum(ft$capacity(t,ft), X(t,ft)) + GAP(t) =g= demand(t) - exchange(t);

ResUp(tt(t))..
     -RES(t) + SLACKUP(t) =g= -resmax(t);

ResLo(tt(t))..
     RES(t) + SLACKLO(t) =g= resmin(t);

PlantCap(tt(t),ft)$capacity(t,ft)..
     -X(t,ft) =g= -capacity(t,ft);

Obj.. COST =e=  sum((tt(t),ft)$capacity(t,ft), gencost(t,ft)*X(t,ft))
              + sum(tt(t), 1000*GAP(t) + 10e6*(SLACKUP(t) + SLACKLO(t)));

Model water / all /;

* Deterministic model
tt(t)=yes;
$if set solvedet Solve water using LP min Cost;

$title SDDP Algorithm

* Scenario data
Set  s Scenarios     / s1*s12 /;
Parameter
     sw_Inflow(w,s)  Inflow realizations MWh per week;
$load sw_Inflow

$if not set jmax $set jmax 100
$if not set imax $set imax   5
Sets i            Trial solutions / i1*i%imax% /
     j            Iteration index / j1*j%jmax% /
     jj(j)        Dynamic j;

Parameters
     cont_m(j,i,w)  Dual variables associated with the mass balance constraint
     delta(j,i,w)   RHS of the Benders cuts;

cont_m(j,i,w) = 0;
delta(j,i,w)  = 0;
jj(j)         = no;

Free Variables
     ACOST         Approximation of cost
Positive Variable
     ALPHA(t)      Approximation of future cost function (FCF);

Equations
     Obj_Approx    Objective function for the one-stage dispatch model
     Cuts(j,i,w,t) Benders cuts;

Obj_Approx.. 
     ACOST =e=  sum((tt(t),ft)$capacity(t,ft), gencost(t,ft)*X(t,ft))
              + sum(tt(t), 1000*GAP(t) + 10e6*(SLACKUP(t) + SLACKLO(t)))
              + sum(last(ww,tt(t)), ALPHA(t+1));

Cuts(jj,i,last(ww(w),t))$(ord(w)<card(w))..
     ALPHA(t+1) - cont_m(jj,i,w+1)*RES(t) =g= delta(jj,i,w);

model waterSDDP / water, obj_approx, cuts /;

* No superflous output and in core solving
option limrow=0, limcol=0, solprint=silent, solvelink=%Solvelink.LoadLibrary%;

Parameters
     i_res(i,t)     Trial decisions
Set
     jwis(j,w,i,s)  Scenario trial sample;

*  Select some for the first backward recursion;
loop(i,
  i_res(i,t) = resmin(t) + (ord(i)-1)/(card(i)-1)*(resmax(t)-resmin(t)));
loop(s$sameas('s1',s),
   jwis(j,w,i,s+UniFormInt(0,card(s)-1))$(not sameas('w1',w)) = yes);
* The Concert implementation needs the following file 
* so sampling in GAMS and Concert is the same
execute_unload 'jwis.gdx', jwis;


* We need to iterate w from back to front
Set revt(w,w) Backward loop for backward recursion excluding week 1;
loop(w$(ord(w)<card(w)), revt(w,w+(card(w)-2*ord(w)+1)) = yes);

$if not set debug $set debug 0
Parameter
     done             Indicator for convergence /0/
     conv(j,*)        Convergence parameters
     zt(j,i)          Objective for trial solution
$if %debug% == 1 sol(*,j,i,t,*)   Solution matrix
;

$onechoV > storeSol.gms
$ifthen %debug% == 1
  sol('res',j,%1,tt,'')     = RES.l(tt);
  sol('x',j,%1,tt,ft)       = X.l(tt,ft);
  sol('gap',j,%1,tt,'')     = GAP.l(tt);
  sol('spill',j,%1,tt,'')   = SPILL.l(tt);
  sol('slackLo',j,%1,tt,'') = SLACKLO.l(tt);
  sol('slackUp',j,%1,tt,'') = SLACKUP.l(tt);
$endif
$offecho

Alias (w,wp,wloop),(i,il);

$if not set timelim $set timelim 2  // Run for max 2 hours
Scalar start; start=jnow;
option solveopt=clear;
loop(j$(not done and (jnow-start)*24<%timelim%), // main iteration
  loop(revt(wp,wloop), // Backward recursion
    tt(t) = no; tt(t)$wt(wloop,t) = yes;
    ww(w) = no; ww(wloop)         = yes;
    loop(il, // trial loop
      RES.fx(t)$last(wloop-1,t) = i_res(il,t);
      loop(s, // for all realizations
        inflow(t)$wt(wloop,t) = sw_Inflow(wloop,s)/card(h);
        Solve waterSDDP min ACOST using LP; 
        abort$(waterSDDP.modelstat<>%modelstat.Optimal%) 'Solve not optimal';
        //calculate cut using marginals of the first hour of the current week
        cont_m(j,il,wloop) = sum(first(wloop,t), Cont.m(t))/card(s) + cont_m(j,il,wloop);
        //use all hours of wloop for the delta calculation
        delta(j,il,wloop-1)  = [sum{wt(wloop,t),
                          Cont.m(t)*inflow(t)
                        + sum(ft$capacity(t,ft), PlantCap.m(t,ft)*(-capacity(t,ft)))
                        + ResUp.m(t)*(-resmax(t)) + ResLo.m(t)*resmin(t)
                        + DemSat.m(t)*(demand(t) - exchange(t))}
                        + sum((jj,i,t)$last(wloop,t), 
                            cuts.m(jj,i,wloop,t)*delta(jj,i,wloop))$(ord(wloop)<card(w))]/card(s)
                        + delta(j,il,wloop-1);
      ); // end of realization loop
    ); // end of trial solution loop
    jj(j) = yes; // we want the cuts from the stage w+1
    RES.lo(t)$last(wloop-1,t)=0; RES.up(t)$last(wloop-1,t)=inf; // free out fixings
  ); //end of backward recursion

  loop(wloop, // Forward simulation
    tt(t) = no; tt(t)$wt(wloop,t) = yes;
    ww(w) = no; ww(wloop)         = yes;
    if (sameas('w1',wloop),
      RES.fx(t)$last(wloop--1,t) = resmin(t);
      //we are using the original inflow first stage, not one of the 12 scenarios
      Solve waterSDDP min ACOST using LP;
      abort$(waterSDDP.modelstat<>%modelstat.Optimal%) 'Solve not optimal';
$     batinclude storeSol i
      conv(j,'lo') = ACOST.l;   //store the first stage cost as the lower bound
      zt(j,i)      = ACOST.l - sum(last(ww,tt(t)), ALPHA.l(t+1)); 
      i_res(i,tt)$last(wloop,tt) = RES.l(tt);
      RES.lo(t)$last(wloop--1,t)=0; RES.up(t)$last(wloop--1,t)=inf; // free out fixings
    else
      loop(jwis(j,wloop,il,s),
        Inflow(t)$wt(wloop,t) = sw_Inflow(wloop,s)/card(h);
        //fix the reservoir levels of the previous stage when going forward
        RES.fx(t)$last(wloop-1,t) = i_res(il,t);
        Solve waterSDDP min ACOST using LP;
        abort$(waterSDDP.modelstat<>%modelstat.Optimal%) 'Solve not optimal';
$       batinclude storeSol il
        i_res(il,tt)$last(wloop,tt) = RES.l(tt);
        zt(j,il) = zt(j,il) + ACOST.l - sum(last(ww,tt(t)), ALPHA.l(t+1)); 
      ); //end of trial loop
    ); // end of if
  ); // end for forward simulation
  conv(j,'up') = sum(i, zt(j,i))/card(i);

$ontext
* Check convergence (original Pereira and Pinto (1991) criterion)
  conv(j,'sigma') = sqrt[sum(n, sqr(conv(j,'up') - zt(j,n)))/sqr(card(n))];
  if (conv(j,'lo') > (conv(j,'up')-conv(j,'sigma')) and
      conv(j,'lo') < (conv(j,'up')+conv(j,'sigma')),
    done = 1);
$offtext

* Convergence criterion of SDDP presented by Shapiro (2009) - remark1
  conv(j,'sigma') = sqrt[sum(i, sqr(conv(j,'up') - zt(j,i)))/(card(i)-1)];
  if (abs[conv(j,'lo')-(conv(j,'up')+1.96*conv(j,'sigma')/sqrt(card(i)))] < 0.01*abs(conv(j,'lo')),
    done = 1);
  conv(j,'time [min]')=(jnow-start)*24*60;
);