#! /bin/bash

awk '
BEGIN {
    ssid=""
    sec=""
    sig=""
    active="&"
}
{
    if ($0 ~ "^BSS .*"){
        if ( ssid != "" )
            printf ("%s|%sdB|%s|%s\n", active, sig, ssid, sec);
        if ($0 ~ /-- associated$/)
            active="*"
        else
            active="&"
    } else if ( $0 ~ "^\\sWPA:"){
        sec="WPA"
    } else if ( $0 ~ "^\\sSSID:"){
         ssid=gensub(/(\sSSID: )(.*)$/, "\\2", "1");
    } else if ( $0 ~ "^\\ssignal:"){
         sig=$2;
    }
}
END {
    if ( ssid != "" )
        printf ("%s|%sdB|%s|%s\n", active, sig, ssid, sec);
}
' /tmp/scan | sort | awk -F "|" '
{
    if ($1 != "*")
        $1 = " ";
    printf("%s|%s|%s|%s\n", $1, $3, $2, $4)
}
'

