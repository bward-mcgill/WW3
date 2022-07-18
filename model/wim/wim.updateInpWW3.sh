#! /bin/bash

year_new=${1}
month_new=${2}
day_new=${3}
sec_ini_new=${4}
dt=${5} 
exp=${6}
W3_REP_INP=${7}

((sec_end_new=sec_ini_new+dt))
((sec_rst_new=sec_ini_new+dt+dt))
#str="${str// /_}"
w3_start_new=`python3 -c "import wimCouplerWW3 as couplerWW3; couplerWW3.createTimeWW3(${year_new}, ${month_new}, ${day_new}, ${sec_ini_new})"`
w3_start_new="${w3_start_new//-/ }"
#w3_IC2_new="`echo ${w3_start_new} | cut -c -8` `echo ${w3_start_new} | cut -c 9-`"
w3_end_new=`python3 -c "import wimCouplerWW3 as couplerWW3; couplerWW3.createTimeWW3(${year_new}, ${month_new}, ${day_new}, ${sec_end_new})"`
w3_end_new="${w3_end_new//-/ }"
w3_endRst_new=`python3 -c "import wimCouplerWW3 as couplerWW3; couplerWW3.createTimeWW3(${year_new}, ${month_new}, ${day_new}, ${sec_rst_new})"`
w3_endRst_new="${w3_endRst_new//-/ }"

echo "|------------Update WW3 namelists ${W3_REP_INP}/ww3_shel(ounf)_${exp}.inp-------------|"

sed -i "s/ .*WimUpStr/  ${w3_start_new} \$WimUpStr/" ${W3_REP_INP}/ww3_shel_${exp}.inp
echo "Start date has been updated to $w3_start_new ${W3_REP_INP}/ww3_shel_${exp}.inp"

sed -i "s/ .*WimUpEnd/  ${w3_end_new} \$WimUpEnd/" ${W3_REP_INP}/ww3_shel_${exp}.inp
echo "End date has been updated to $w3_end_new ${W3_REP_INP}/ww3_shel_${exp}.inp"

sed -i "s/ .*WimUpOut/   ${w3_start_new} ${dt} ${w3_end_new} \$WimUpOut/" ${W3_REP_INP}/ww3_shel_${exp}.inp
echo "Output option has been updated to ${w3_start_new} ${dt} ${w3_end_new} in ${W3_REP_INP}/ww3_shel_${exp}.inp"

sed -i "s/ .*WimUpRst/   ${w3_end_new} ${dt} ${w3_endRst_new} \$WimUpRst/" ${W3_REP_INP}/ww3_shel_${exp}.inp
echo "Restart option has been updated to ${w3_end_new} ${dt} ${w3_endRst_new} in ${W3_REP_INP}/ww3_shel_${exp}.inp"

sed -i "s/ .*WimUpIC2/    'IC2' ${w3_start_new} 1.83e-6 \$WimUpIC2/" ${W3_REP_INP}/ww3_shel_${exp}.inp
echo "IC2 option has been updated to 'IC2' ${w3_start_new} 1.83e-6 in ${W3_REP_INP}/ww3_shel_${exp}.inp"

sed -i "s/ .*WimUpOunf/   ${w3_start_new} ${dt} 1 \$WimUpOunf/" ${W3_REP_INP}/ww3_ounf_${exp}.inp
echo "Output option has been updated to ${w3_start_new} ${dt} 1 in ${W3_REP_INP}/ww3_ounf_${exp}.inp"
