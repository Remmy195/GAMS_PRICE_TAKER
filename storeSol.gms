$ifthen %debug% == 1
  sol('res',j,%1,tt,'')     = RES.l(tt);
  sol('x',j,%1,tt,ft)       = X.l(tt,ft);
  sol('gap',j,%1,tt,'')     = GAP.l(tt);
  sol('spill',j,%1,tt,'')   = SPILL.l(tt);
  sol('slackLo',j,%1,tt,'') = SLACKLO.l(tt);
  sol('slackUp',j,%1,tt,'') = SLACKUP.l(tt);
$endif
