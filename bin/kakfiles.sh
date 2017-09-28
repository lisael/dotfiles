#! /bin/bash

WORK="$(mktemp -d)" || exit $?
trap "rm -rf '$WORK'" EXIT
FILES=${WORK}/list
GIT_FILES=${WORK}/git
CANNON_GIT_FILES=${WORK}/cgit

kak_buffers=$1
kak_pwd=$2


cannonize(){
    printf "%s\t%s\n" "$@" $(readlink -f "$@")
}
export -f cannonize


printf '%s' ${kak_buffers} | sed 's/:/\n/g' | grep -ve '^\*\(debug\|scratch\)\*$' >> ${FILES}

GIT_DIR=$(dirname $(realpath -s $(git rev-parse --git-dir)))
#IFS='\n'
if [ "$GIT_DIR" != "" ]; then
    #printf '%s' ${kak_buffers} | sed 's/:/\n/g' | sed "s|^~|$HOME|" |grep -ve '^\*debug\*$' >> ${GIT_FILES}
    #git status --porcelain -c "${GIT_DIR}"| grep -e "^\s*M" | awk '{print $2}' | xargs stat -c '%Y %n' | sort -r | awk '{print $2}'>> ${GIT_FILES}
    git status --porcelain | grep -v "^??" | awk '{print $2}' | xargs -n 1 printf "%s/%s\n" $GIT_DIR | xargs realpath -s --relative-to . >> ${GIT_FILES}

    git diff --name-only master... | xargs -n 1 printf "%s/%s\n" $GIT_DIR | xargs realpath -s --relative-to . >> ${GIT_FILES}

    git ls-files -c "$GIT_DIR" | xargs stat -c '%Y %n' | sort -r | awk '{print $2}' >> ${GIT_FILES}

    git status --porcelain | grep "^??" | awk '{print $2}' | xargs -n 1 printf "%s/%s\n" $GIT_DIR | xargs realpath -s --relative-to . >> ${GIT_FILES}

    echo $GIT_DIR/.git/config >> ${GIT_FILES}
    
    cp ${GIT_FILES} ${CANNON_GIT_FILES}

    fasd -fR | awk '{print $2}' >> "${GIT_FILES}"
    fasd -fR | awk '{print $2}' | xargs -P 10 realpath --relative-to=$PWD -s >> "${CANNON_GIT_FILES}"

    paste $GIT_FILES $CANNON_GIT_FILES | awk '!seen[$2]++ {print $1}' >> ${FILES}
    
else
    fasd -fR | awk '{print $2}' >> "${FILES}"
    # find ~/projects -not \( \( -path "*/.cache" -o -path "*/.mozilla" -o -path "*/.git/objects/*" -o -path "*/.config/chromium" -o -path "*/.git/refs*" -o -path "*/.git/hook*" -o -path "*/.git/log*" \) -prune \) -type f -mtime -7 -printf "%T@ %p\n" | sort -nr | head -3000 | awk '{print $2}' >> ${FILES}
fi
  
awk '!seen[$0]++' ${FILES} | rofi -dmenu -sync -i -font "mono 6" -p "Open ($(wc -l $FILES | awk '{print $1}')):" -scroll-method 1 -lines 30 -matching glob -hide-scrollbar -line-margin 0
