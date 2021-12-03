#! /bin/bash

data=$(mips-img-linux-gnu-readelf --program-headers $1)
has_data=$(echo $data | grep '.data' -c)
if [ $has_data -le 0 ]; then
    echo '00000000 00000000 00000000 00000000 00000000' > $2
else
    mips-img-linux-gnu-objdump -s -z --section=.data $1 > $2
fi
