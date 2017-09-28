#!/bin/bash
#
# Copyright (C) 2013 Shujie Zhang <zhang.shujie87 at gmail.com>. All Rights
# Reserved.
# This file is licensed under the GPLv3+. Please see COPYING for more
# information.
#
# Usage:
# Export cvs file from firefox by using extention "Password Exporter"
# https://addons.mozilla.org/en-US/firefox/addon/password-exporter/
#
# You can test run it to check if bash syntax are OK:
#         ./firefox2pass.sh testrun CVS_FILE
# Import to pass run as :
#         ./firefox2pass.sh import CVS_FILE
# Feature:
# * If username do not exist, use NONAME as username

args=("$@")

helptext=$'Usage:
You can test run it to check if bash syntax are OK:
        ./firefox2pass.sh testrun CVS_FILE
Import to pass run as :
        ./firefox2pass.sh import CVS_FILE

Feature:
* If username do not exist, use NONAME as username'


main(){
cat ${args[1]} \
    | tail -n +3 \
    | awk -F '","' '{ OFS = ","; print $1"\"", "\""$2"\"", "\""$3"\"", "\""$3"\""}' \
    | sed "s|https\?\://|www/|g" \
    | sed 's/","","/","NONAME","/g' \
    | sed 's|","|/|1' \
    | sed "s/^/pass insert /g" \
    | sed  's/","/"\n/1' \
    | sed 's/","\(.*\)"$/\n\1/'


}


if [ -z "${args[1]}" ] ; then
        echo "$helptext"
elif [ "${args[0]}" = "testrun" ]; then
        main
elif [ "${args[0]}" = "import" ]; then
        main | bash
else    echo "$helptext"
fi

