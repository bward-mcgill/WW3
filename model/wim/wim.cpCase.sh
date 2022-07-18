#! /bin/bash

#launch : ./wim.cpCase.sh old_case new_case

old_case=${1}
new_case=${2}

REP_MODEL=${HOME}/wim/ww3/model
REP_INP=${REP_MODEL}/inp

if [ -d $REP_INP/${old_case} ]; then
   rm -rf $REP_INP/${new_case}
   cp -r $REP_INP/${old_case} $REP_INP/${new_case}
else
   echo "${old_case} don't exist you want to copy an exp that already exist !"
   exit 1
fi

cd $REP_INP/${new_case}
list_files=`ls ./*`

for file in $list_files
do
   file=$(basename -- $file)
   mv -- "${file}" "${file//${old_case}/${new_case}}" > /dev/null 2>&1
done

find ./ -type f -exec sed -i "s/$old_case/$new_case/g" {} \;

echo "New exp $REP_INP/$new_case is created, you can now edit it."
