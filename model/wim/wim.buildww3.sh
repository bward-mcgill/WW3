#!/bin/bash 

# --------------------------------------------------------------------------  #
# wim.buildww3 : Compile WW3 as part of a wave-in-ice model (WIM) coupled     # 
#                with CICE.                                                   #
#                                                                             #
# use  : ./wim.buildww3.sh ${REP_MOD} ${EXP} ${LIST_PROG}                     #
#                                                                             #
#                                                      Benjamin Ward          #
#                                                      June 2022              #
#                                                      McGill University      #
# --------------------------------------------------------------------------- #

#Read argument
REP_MOD=${1}
exp=${2}

i=1
for arg in $@
do
if [ $i -le 2 ];then
   ((i=i+1))
   continue
else
    list_prog="$list_prog $arg"
fi
done

#Constants
REP_BIN=${REP_MOD}/bin
REP_WRK=${REP_MOD}/work/${exp}
REP_OUT=${REP_MOD}/out/${exp}

if [ ! -d ${REP_OUT} ]; then 
    mkdir -p ${REP_OUT}
fi

if [ ! -d ${REP_WRK} ]; then
   mkdir -p ${REP_WRK}
fi

#----------------------Compile each prog-----------------------------#
for prog in $list_prog
do
   echo "|-----Compile $prog------|"
   #Compile
   bash ${REP_BIN}/w3_make ${prog}
done
#-----------------------------THE END--------------------------------------------#
