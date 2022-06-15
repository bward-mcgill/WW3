#!/bin/bash 

REP_MODEL=${HOME}/ww3/model
exp='test6'

REP_BIN=${REP_MODEL}/bin
REP_EXE=${REP_MODEL}/exe
REP_INP=${REP_MODEL}/inp/${exp}
REP_WRK=${REP_MODEL}/work/${exp}
REP_NML=${REP_MODEL}/nml/${exp}
REP_OUT=${REP_MODEL}/out/${exp}

REP_IMG=${REP_OUT}/img

export WWATCH3_NETCDF=NC4
export NETCDF_CONFIG=/usr/bin/nf-config

if [ ! -d ${REP_INP} ]; then
   echo "Please create an input directory (${REP_INP}) with all the required files."
   exit 1
fi

#Move to work directory
if [ ! -d ${REP_WRK} ]; then
   mkdir -p ${REP_WRK}
fi

cd ${REP_WRK}

## Step 0 : Set up WW3 environment -------------------------------------------------# 
#
#echo ' '
#echo '+-------------------------------+'
#echo '|          Set up WW3           |'
#echo '+-------------------------------+'
#echo ' '
#
#rm -f ${REP_BIN}/switch_${exp}
#ln -s ${REP_INP}/switch_${exp} ${REP_BIN}/switch_${exp}
#bash ${REP_BIN}/w3_setup ${REP_MODEL} -c Intel -s ${exp}

# # Step 1 : Grid pre-processor ---------------------------------------------------- #

echo ' '
echo '+--------------------------------+'
echo '|  Compile and execute ww3_grid  |'
echo '+--------------------------------+'
echo ' '

prog="ww3_grid"

#Compile
bash ${REP_BIN}/w3_make ${prog}

#Execute (I can pass all the parameters throught the .inp file or the .nml).
#I will use the .inp for now since it seems like the order does not matter in the .nml

output=${REP_WRK}/${prog}_${exp}.out
rm -f ${prog}.inp
ln -s ${REP_INP}/${prog}_${exp}.inp ${prog}.inp
${REP_EXE}/${prog} > ${output}
rm -f ${prog}.inp

# This create the executable mod_def.ww3 that is required for the execution of the other modules.
# For example it will be stored under : /aos/home/bward/WW3/model/work/${exp}/mod_def.ww3 

# Step 2 : Initial conditions ------------------------------------------------------#

echo ' '
echo '+------------------------------+'
echo '| Compile and execute ww3_strt |'
echo '+------------------------------+'
echo ' '

prog="ww3_strt"

# #Compile
bash ${REP_BIN}/w3_make ${prog}

# #Execute
output=${REP_WRK}/${prog}_${exp}.out
rm -f ${prog}.inp
ln -s ${REP_INP}/${prog}_${exp}.inp ${prog}.inp
${REP_EXE}/${prog} > ${output}
rm -f ${prog}.inp

# This create the executable restart.ww3 that is required for the execution of the ww3_shell.
# For example it will be stored under : /aos/home/bward/WW3/model/work/${exp}/restart.ww3 

# Step 3 : Boundary conditions ------------------------------------------------------#

echo ' '
echo '+-------------------------------+'
echo '| Compile and execute ww3_bound |'
echo '+-------------------------------+'
echo ' '

echo "There is no boundary conditions required for that exp."

#Need to do another one if BC is netCDF

# Step 4 : Prepare forcing fields ----------------------------------------------------#

#There is 3 fields that I need to prep : 1) Ice concentration (ICE), 2) Ice thickness (IC1) and 3) Ice floe mean diameter (IC5)

echo ' '
echo '+-------------------------------+'
echo '| Compile and execute ww3_prep |'
echo '+-------------------------------+'
echo ' '

prog="ww3_prep"

#Compile
bash ${REP_BIN}/w3_make ${prog}

inputs="$(ls ${REP_INP}/${prog}*.inp 2>/dev/null)"

for ifile in $inputs
do
    otype="$(basename $ifile .inp | sed s/^${prog}_//)"
    output=${REP_WRK}/${prog}_${otype}.out
    rm -f ${prog}.inp
    ln -s ${ifile} ${prog}.inp
    ${REP_EXE}/${prog} > ${output}
    rm -f ${prog}.inp
done

# This create the executable ice.ww3, ice1.ww3, ice5.ww3 that is required for the execution of the ww3_shell.
# For example it will be stored under : /aos/home/bward/WW3/model/work/${exp}/ice.ww3 
# Idem would need to do another loop if forcing are netCDF files.

# Step 5 : Run the model ----------------------------------------------------#

echo ' '
echo '+------------------------------------+'
echo '|    Compile and execute ww3_shel    |'
echo '+------------------------------------+'
echo ' '

prog="ww3_shel"

# Compile
bash ${REP_BIN}/w3_make ${prog}

# Execute (there is somehow problems with the .nml (cant read the point files)) so Ill go with the .inp
output=${REP_WRK}/${prog}_${exp}.out
rm -f ${prog}.inp
ln -s ${REP_INP}/${prog}_${exp}.inp ${prog}.inp
${REP_EXE}/${prog} > ${output}
rm -f ${prog}.inp


# Demain finir la compilation, execution pour les outputs.
# Faire des scripts the post-processing (image et tout). Pour montrer les resultats a Bruno ?

# Step 6 : Process the output ------------------------------------------------#

if [ ! -d ${REP_OUT} ]; then 
    mkdir -p ${REP_OUT}
fi

# ----------------------------------------------------------------------#
# 6.1 Grid integration : ww3_gint just for multigrid not needed for that, let's skip it for now 
#-----------------------------------------------------------------------#


# 6.2 Grided output : ww3_outf (or eventually ww3_ounf if I download netCDF librairies).

# Can do a loop for every species (like forcing), but I can also specify a list of species in the .inp file, so I don't really understand what is the point of creating a bunch of those file.
# Maybe I'll understand later

#if [ ! -d ${REP_OUT}/grd ]; then 
#    mkdir -p ${REP_OUT}/grd
#fi
#
#prog="ww3_outf"
#
#echo ' '
#echo '+--------------------+'
#echo "Compile and execute ${prog}"
#echo '+--------------------+'
#echo ' '
#
#bash ${REP_BIN}/w3_make ${prog}
#
#output=${REP_WRK}/${prog}_${exp}.out
#rm -f ${prog}.inp
#ln -s ${REP_INP}/${prog}_${exp}.inp ${prog}.inp
#${REP_EXE}/${prog} > ${output}
#rm -f ${prog}.inp
#
##I'll have to change that to make it cleaner eventually.
#
#

#if [ ! -d ${REP_OUT}/grd ]; then 
#    mkdir -p ${REP_OUT}/grd
#fi

prog="ww3_ounf"

echo ' '
echo '+--------------------+'
echo "Compile and execute ${prog}"
echo '+--------------------+'
echo ' '

bash ${REP_BIN}/w3_make ${prog}

output=${REP_WRK}/${prog}_${exp}.out
rm -f ${prog}.inp
ln -s ${REP_INP}/${prog}_${exp}.inp ${prog}.inp
${REP_EXE}/${prog} > ${output}
rm -f ${prog}.inp

#I'll have to change that to make it cleaner eventually.

#mv ${REP_WRK}/ww3.*.* ${REP_OUT}/grd/


## 6.3 Point output : ww3_outp (or eventually ww3_ounp if I download netCDF librairies).

if [ ! -d ${REP_OUT}/ptn ]; then 
    mkdir -p ${REP_OUT}/ptn
fi

prog="ww3_outp"

echo ' '
echo '+--------------------+'
echo "Compile and execute ${prog}"
echo '+--------------------+'
echo ' '

bash ${REP_BIN}/w3_make ${prog}

inputs="$(ls ${REP_INP}/${prog}*.inp 2>/dev/null)"

for ifile in $inputs
do
    otype="$(basename $ifile .inp | sed s/^${prog}_//)"
    output=${REP_WRK}/${prog}_${otype}.out
    rm -f ${prog}.inp
    ln -s ${ifile} ${prog}.inp
    ${REP_EXE}/${prog} > ${output}
    rm -f ${prog}.inp
done

mv ${REP_WRK}/tab*.ww3 ${REP_OUT}/ptn/
mv ${REP_WRK}/${prog}*.out ${REP_OUT}/ptn/

# 6.4 Track output : ww3 trck (or eventually ww3_trnc if I download netCDF librairies).
# Not needed right now !

# Step 7 : Update restart file ? (ww3_uprstr)

#Not needed right now !

if [ ! -d ${REP_IMG} ]; then
   mkdir -p ${REP_IMG}
fi

#-----------------------------THE END--------------------------------------------#
