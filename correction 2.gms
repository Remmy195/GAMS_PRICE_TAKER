$sTitle Energy Storage Price-Taker Optimization Tool
$onSymXRef

set time /1*120/;
alias (time,i);

parameter h "number of hours of storage", p "power capacity of storage device", E_min, E_max, PF, cost_op, max_charge, max_disch, eff_c, eff_dc, rate_loss, ini_energy;

h = 10;
p = 500;
E_min = 0;
E_max = h*p;
PF = 0;
eff_c = 0.52;
eff_dc = 0.52;
cost_op = 0;
rate_loss = 0;
ini_energy = 2500;



table ts(time,*)
$offlisting
$ondelim
$include Data_120.csv
$offdelim
$onlisting
;

Parameter price(time);
price(time)=ts(time,'price');

display price;

variable E_stored(time);
variable E_pur(time);
variable E_sold(time);
Variable E_dischar(time);
Variable E_charge(time);

variable E_aval(time);
variable E_loss(time);
variable rev(time);
variable cost(time);
variable Cost_pur(time);
variable Profit;
Binary Variable z_charge(time);
Binary Variable z_dischar(time);
E_charge.lo(time) = 0;
E_dischar.lo(time) = 0;



equation e1(time), e1a(time), e2(time), e3(time), e4(time), e4a(time), e5(time), e6(time), e7(time), e9(time), e10(time), e11(time), e13(time), e14;

e1(time)..    E_min =l= E_aval(time);
e1a(time)..   E_aval(time) =l= E_max;

e2(time)..   E_charge(time) =l= z_charge(time)*p;

e3(time)..   E_dischar(time) =l= z_dischar(time)*p;
e4(time)..    z_charge(time) + z_dischar(time) =l= 1;
e4a(time)..   E_charge(time) =e= E_pur(time)*eff_c;
e5(time)..    E_dischar(time) =e= E_sold(time)/eff_dc;
e6(time)..    E_aval(time) =e= E_stored(time)-E_loss(time);
e7(time)..    E_stored(time) =e= ini_energy$(ord(time)=1)+E_stored[time-1]$(ord(time)>1)+E_charge(time)-E_dischar(time);

e9(time)..    E_loss(time) =e= E_stored(time)*rate_loss;
e10(time)..   Rev(time) =e= price(time)*E_sold(time);
e11(time)..   cost(time) =e= Cost_pur(time)+(cost_op*E_aval(time));

e13(time)..   Cost_pur(time) =e= E_pur(time)*price(time);
e14..   Profit =e=sum(time, rev(time)-cost(time))

model energy_storage /all/;

*this creates the iis option file for debugging infeasibility*
$onecho > cplex.opt
iis 1
$offEcho

*this tells the solver to look for the option file to load*
energy_storage.optfile=1;

solve energy_storage maximizing profit using mip;

