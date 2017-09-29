cat xmonad.hs ~/projects/haskell/xmonad/src/XMonad/Config.hs \
| grep -ve '^\s*--' \
| grep xK_ \
| grep -v xK_0 \
| grep "\(modm\)\|\(modMask\)" \
| sed 's/-- .*\(%!.*\)$/\1/'\
| awk '{
if ( /%!/ )
    print $0
else
    print $0 " %! TODO"
}' \
| awk -F "%!" '{print $2 "%!" $1}' \
| sed 's/^\(.*xK_\w*\).*/\1/' \
| sed 's/.*/\0 M/' \
| sed 's/.*[^M]$/\0 0/' \
| sed 's/.*controlMask.*\(xK_\w*\).*/\0 C/' \
| sed 's/.*[^C]$/\0 0/' \
| sed 's/.*shiftMask.*\(xK_\w*\).*/\0 S/' \
| sed 's/.*[^S]$/\0 0/' \
| awk -F "%!" '{print $2 " %!" $1}' \
| sed 's/.*xK_\(.*\)/\1/' \
| sed 's/  / - /' \
| awk '{
comp=$1$2$3$4
if (!seen[comp]++)
    print
}' \
| sort \
| awk '{
{printf "%s-%s-%s-%s", $2, $3, $4, $1}
for(i=5;i<=NF;i++){
    printf " %s", $i
}
printf "\n"
}' \
| sed 's/^\(\S*\)-0\(\S*\)/\1\2/' \
| sed 's/^\(\S*\)-0\(\S*\)/\1\2/' \
| sed 's/%!/->/' \
| sed 's/^M/Meta/' \
| sed 's/^\(\S*\)-S-\(\S*\)/\1-Shift-\2/' \
| sed 's/^\(\S*\)-C-\(\S*\)/\1-Ctrl-\2/' \
#| column -s " " -t
