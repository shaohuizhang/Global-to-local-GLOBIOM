$ontext

This is a script to allocate land information to a specific region

$offtext
$version 1

*
* input and output file
*
$set gdxData    H:\MyDocuments\Projects\Global-to-local-GLOBIOM\Model\ZMB\Data\input_data_ZMB_2000.gdx
$set gdxOutput  H:\MyDocuments\Projects\Global-to-local-GLOBIOM\Model\ZMB\Results\min_entropy_ZMB_2000.gdx



*
* Define model
*

$onempty

sets
    i       Pixels
    j       Crops with system identifier
    s       Crop list
    k       adm names

    n(s,j)  Crops with corresponding systems
    l(k,i)  grid cells in adm with statistics
    m(k,s)  Crops in adm with statistics
;

scalars
    scalef         Scaling parameter for GAMS: number of grid cells
;

parameters
    adm_area(k,s)   Crop area per adm
    lc(i)           Crop cover per grid cell
    crop_area(j)    Total area per crop
    priors(i,j)     Prior information about area shares
    ir_area(i)      Irrigated area per grid cell
    ir_crop(j)      Total irrigated area per crop
    det_area(i,j)   Detailed land use information
;


*
* load data from GDX file
*
$gdxin %gdxData%
$load i j s k
$loaddc n l m
$loaddc adm_area lc crop_area priors scalef ir_crop ir_area det_area

display priors
display scalef

abort$sum(j$(crop_area(j)<0),1) "crop_area should be positive",crop_area;
*abort$sum(i$(lc(i,c)<0),1) "lc should be positive",lc;

sets
   sign /plus,minus/
   nonzero(i,j)  allocation that can be non zero
;

nonzero(i,j) = crop_area(j) > 0;


variables
    alloc(i,j)          Allocation of crop j to plot i
*    adm_slack(k,s,sign) Slack variable for adm area
    entropy             Cross entropy
    lc_slack(i)         Slack for land cover
    ir_slack(i)         Slack for ir_area
;

scalars
    epsilon original Tolerance to allow zero area shares /0.000001/
    epsOne /0.0001/
    Thou /1000/
    allow /0.05/
    pixel /0.95/

    totL /0.0/
    totU /0.0/
    totS /0.0/
;

* Ensure that variables are positive
positive variable alloc, lc_slack, ir_slack(i);



*
* Set upper boundary of allocation
*
* Alloc can not be higher than scalef (=100% or 1 after scaling)
* meaning all crop_area in one grid cell i or scalef*lc(i)/crop_area(j),
* or grid-cell area/total crop area when crop area is divided over more grid cells

*alloc.up(i,j) = scalef;
alloc.up(i,j) = min(scalef, scalef*lc(i)/crop_area(j))$crop_area(j);
* this implies crop_area=0 -> alloc.fx=0

parameter chkalloc(j,*);
chkalloc(j,'max total') = smax(i,alloc.up(i,j));
chkalloc(j,'req total')$crop_area(j) = scalef;
display chkalloc;


*
* Set initial values
*
alloc.l(i,j) = priors(i,j);
*adm_slack.l(k,s,sign) = 0.0 ;


equations
    obj_entropy     Objective function: minimize entropy
    sum_one(j)      Sum of land allocation shares is 1
    grid_cover(i)   Sum of allocated area should not be larger than grid cell cover
    adm_stat(k,s)   Allocated area should be equal to adm statistics
    ir_cover(i)     Irrigated crops should be allocated to irrigated area
    det_alloc(i,j)  Allocate shares for crops for which spatial data is lcable
;


**** OBJECTIVE FUNCTIONS

*
* entropy objective function
*
parameter log_priors(i,j);
log_priors(i,j)=log(priors(i,j));

obj_entropy.. entropy =e= sum(nonzero(i,j),
  alloc(i,j)*(log(alloc(i,j)+epsilon)-log_priors(i,j)));


**** CONSTRAINTS

* Constraint 1
* Allocated shares are in between 0 and 1
* Set by postive values and alloc.up statements above.


* Constraint 2
* sum of allocated shares for each crop over all grid cells is 1
*
sum_one(j)$crop_area(j)..
    (1/scalef)*sum(i, alloc(i,j)) =e= 1;


* Constraint 3
* Sum of allocated area over all crops should not exceed actual cropland in a grid cell.
*grid_cover(i)..
*    (1/scalef)*sum(nonzero(i,j), alloc(i,j)*crop_area(j)) =l= lc(i)+lc_slack(i);
grid_cover(i)..
    (1/scalef)*sum(nonzero(i,j), alloc(i,j)*crop_area(j)) =l= lc(i);

* Contstraint 4
* Irrigated grid cells are allocated
*
ir_cover(i)..
    (1/scalef)*sum(j$ir_crop(j), alloc(i,j)*ir_crop(j)) =l= ir_area(i) + ir_slack(i);
*ir_cover(i)..
*    (1/scalef)*sum(j$ir_crop(j), alloc(i,j)*ir_crop(j)) =l= ir_area(i);

* Constraint 5
* Total allocation per crop should be equal to land use in adm
* allow slack between adm_area and total allocation into (k,s)
* if adm_area = 0 or artificial subnat unit (zz) then we don't allow any slack
*
*adm_stat(m(k,s))..
* (1/scalef)*sum((l(k,i),n(s,j)), alloc(i,j)*crop_area(j)) =e=
*            adm_area(k,s) + (slack(k,s,'plus')-slack(k,s,'minus'))$adm_area(k,s);

adm_stat(m(k,s))..
    (1/scalef)*sum((l(k,i),n(s,j)), alloc(i,j)*crop_area(j)) =e= adm_area(k,s);


* Constraint 6
* Set shares for crops-gridcell combinations for which have spatiald data
*

det_alloc(i,j)..
    (1/scalef)*alloc(i,j)$det_area(i,j) =e= det_area(i,j);

* Model
model min_entropy   /obj_entropy, sum_one, grid_cover, ir_cover, adm_stat, det_alloc/;

* Solver options
option reslim = 900000;
*option nlp = CONOPT4;
option nlp = mosek;
option Subsystems;
option limrow = 10;
option limcol = 10;


*
* Reporting
*

* Reporting parameters
parameters
   report(*)
   rep(*,*,*)
;
report('mstat') = 13;
report('sstat') = 13;


* Fixes constant variables (where lower and upper bound is equal) and simplifies model
min_entropy.holdfixed=1;

* Solve model
solve min_entropy using nlp minimizing entropy;

report('mstat') = min_entropy.modelstat;
report('sstat') = min_entropy.solvestat;
report('iterusd') = eps+min_entropy.iterusd;
report('resusd') = eps+min_entropy.resusd;
report('mrun')  = 1;


set labels /allocation, suitable, lcable, unused/;

parameters
   palloc(i,j)      'allocations (unscaled)'
   result_crop(j,*)  'results for crops'
   result_pixel(i,*) 'results for pixels'
;


palloc(i,j) = alloc.l(i,j)*crop_area(j)/scalef;
palloc(i,j)$(palloc(i,j)<1.0e-5) = 0;


* Summary per crop
result_crop(j,'Allocation') = sum(i,palloc(i,j));
*ResultCrop(j,'Suitable') = sum(i,suitable(i,j));
*ResultCrop(j,'Unused')$ResultCrop(j,'Suitable') = 100*(ResultCrop(j,'Suitable')-ResultCrop(j,'Allocation'))/ResultCrop(j,'Suitable');
result_crop(j,'Unused')$(result_crop(j,'Unused')<1.0e-5) = 0;

display result_crop;


* Summary per pixel
result_pixel(i,'Allocation') = sum(j,palloc(i,j));
result_pixel(i,'available') = lc(i);
result_pixel(i,'Difference') = result_pixel(i,'Allocation')-result_pixel(i,'available');
*ResultPixel(i,'Unused')$ResultPixel(i,'lcable') = 100*(ResultPixel(i,'lcable')-ResultPixel(i,'Allocation'))/ResultPixel(i,'lcable');
result_pixel(i,'Unused')$(result_pixel(i,'Unused')<1.0e-5) = 0;

display result_pixel;


parameters control_alloc(k,s);
control_alloc(k,s) =
    (1/scalef)*sum((l(k,i),n(s,j)), alloc.l(i,j)*crop_area(j));


* Compare ir_slack with availability
parameter sum_ir_slack;
sum_ir_slack = sum(i, ir_slack.l(i));

display sum_ir_slack;

parameters
comp_ir_slack(i,*);
comp_ir_slack(i,'availability')$ir_slack.l(i) = lc(i);
comp_ir_slack(i,'irslack')$ir_slack.l(i) = ir_slack.l(i);
comp_ir_slack(i,'irarea')$ir_slack.l(i) = ir_area(i);
comp_ir_slack(i,'allocated')$ir_slack.l(i) = sum(j,palloc(i,j));


*
* save solution
*
execute_unload "%gdxOutput%" , i, j, scalef, labels, alloc, palloc, report, result_crop, result_pixel,
                   k, s, sign, control_alloc, adm_area, lc, crop_area, priors, ir_area, ir_crop, lc_slack, ir_slack, comp_ir_slack;

$ontext
$offtext
