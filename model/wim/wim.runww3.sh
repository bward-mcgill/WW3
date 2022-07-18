#!/bin/bash 

# --------------------------------------------------------------------------  #
# wim.runww3 : Run WW3 as part of a wave-in-ice model (WIM) coupled           # 
#                with CICE.                                                   #
#                                                                             #
# use  : ./wim.runww3.sh ${REP_MOD} ${EXP} ${LIST_PROG}                       #
#                                                                             #
#                                                      Benjamin Ward          #
#                                                      June 2022              #
#                                                      McGill University      #
# --------------------------------------------------------------------------- #

#Read argument
REP_MODEL=${1}
exp=${2}
dateTs=${3}

i=1
for arg in $@
do
if [ $i -le 3 ];then
   ((i=i+1))
   continue
else
    list_prog="$list_prog $arg"
fi
done

#Constants
REP_EXE=${REP_MODEL}/exe
REP_INP=${REP_MODEL}/inp/${exp}
REP_WRK=${REP_MODEL}/work/${exp}
REP_NML=${REP_MODEL}/nml/${exp}
REP_OUT=${REP_MODEL}/out/${exp}
REP_IMG=${REP_OUT}/img

#Get to work !
cd ${REP_WRK}

#----------------------Run each prog-----------------------------#

for prog in $list_prog
do
   echo "|-----Execute $prog------|"

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

   if [ $prog = "ww3_strt" ]; then
      cp restart.ww3 restart_${dateTs}.ww3
   fi

   if [ $prog = "ww3_shel" ]; then
      mv restart001.ww3 restart_${dateTs}.ww3
      ln -fs restart_${dateTs}.ww3 restart.ww3
   fi

done

#------------------Move output and clean stuff--------------#

#if ! $bool_NETCDF ; then
#   mv ${REP_WRK}/ww3.*.* ${REP_OUT}/grd/
#else
#   mv ${REP_WRK}/ww3.*.nc ${REP_OUT}/grd/
#fi

#mv ${REP_WRK}/tab*.ww3 ${REP_OUT}/ptn/
#mv ${REP_WRK}/${prog}*.out ${REP_OUT}/ptn/

#if [ ! -d ${REP_IMG} ]; then
#   mkdir -p ${REP_IMG}
#fi

#-----------------------------THE END--------------------------------------------#
