#!/bin/bash 

# --------------------------------------------------------------------------- #
# launcher_ww3.sh : Launch uncoupled simulation of WW3                        #
#                                                                             #
# use  : ./wim.launcherww3.sh                                                    #
#                                                                             #
#                                                      Benjamin Ward          #
#                                                      June 2022              #
#                                                      McGill University      #
# --------------------------------------------------------------------------- #

#Variables
REP_MODEL=${HOME}/wim/ww3/model
exp='case11'

#Constants
REP_BIN=${REP_MODEL}/bin
REP_EXE=${REP_MODEL}/exe
REP_INP=${REP_MODEL}/inp/${exp}
REP_WRK=${REP_MODEL}/work/${exp}
REP_NML=${REP_MODEL}/nml/${exp}
REP_OUT=${REP_MODEL}/out/${exp}
REP_IMG=${REP_OUT}/img

#Check if switch NC4 is there or not.
export WWATCH3_NETCDF=NC4
export NETCDF_CONFIG="/usr/bin/nc-config"
switch_file=`cat $REP_INP/switch_${exp}`
if [[ "$switch_file" == *"$WWATCH3_NETCDF"* ]]; then
   bool_NETCDF=true
else
   bool_NETCDF=false
fi

#List of prog to run.
if ! $bool_NETCDF; then
#   list_prog="ww3_grid ww3_strt ww3_prep ww3_shel ww3_outf ww3_outp"
   list_prog="ww3_grid ww3_strt ww3_prep"
else
#   list_prog="ww3_grid ww3_strt ww3_prnc"
    list_prog="ww3_grid ww3_strt ww3_prnc ww3_shel ww3_ounf"
fi

#Create some repositories.
if [ ! -d ${REP_INP} ]; then
   echo "Please create an input directory (${REP_INP}) with all the required files."
   exit 1
fi

if [ ! -d ${REP_OUT} ]; then 
    mkdir -p ${REP_OUT}
fi

if [ ! -d ${REP_WRK} ]; then
   mkdir -p ${REP_WRK}
fi

#Get to work !
cd ${REP_WRK}

#----------------------Set up WW3 environment -------------------# 

echo '|------------Set up WW3-------------|'

rm -f ${REP_BIN}/switch_${exp}
ln -s ${REP_INP}/switch_${exp} ${REP_BIN}/switch_${exp}
bash ${REP_BIN}/w3_setup ${REP_MODEL} -c Gnu -s ${exp}

#----------------------Run each prog-----------------------------#

for prog in $list_prog
do
   echo "|-----Compile and execute $prog------|"

   #Compile
   bash ${REP_BIN}/w3_make ${prog}

   #Execute
   inputs="$(ls ${REP_INP}/${prog}*.inp 2>/dev/null)"
   loop_cond=`echo "$inputs" | wc -w`
   if [ $loop_cond -gt 1 ]; then
      for ifile in $inputs
      do
         otype="$(basename $ifile .inp | sed s/^${prog}_//)"
         output=${REP_WRK}/${prog}_${otype}.out
         rm -f ${prog}.inp
         ln -s ${ifile} ${prog}.inp
         ${REP_EXE}/${prog} > ${output}
         rm -f ${prog}.inp
      done
   else
      output=${REP_WRK}/${prog}_${exp}.out
      rm -f ${prog}.inp
      ln -s ${REP_INP}/${prog}_${exp}.inp ${prog}.inp
      ${REP_EXE}/${prog} > ${output}
      rm -f ${prog}.inp     
   fi
done

#------------------Move output and clean stuff--------------#

#if ! $bool_NETCDF ; then
#   mv ${REP_WRK}/ww3.*.* ${REP_OUT}/grd/
#else
#   mv ${REP_WRK}/ww3.*.nc ${REP_OUT}/grd/
#fi
#
#mv ${REP_WRK}/tab*.ww3 ${REP_OUT}/ptn/
#mv ${REP_WRK}/${prog}*.out ${REP_OUT}/ptn/

#if [ ! -d ${REP_IMG} ]; then
#   mkdir -p ${REP_IMG}
#fi

#-----------------------------THE END--------------------------------------------#
