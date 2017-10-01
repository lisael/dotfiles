#! /bin/bash

PASS_DIR=~/.password-store
CLIPMENU_DIR=$HOME/.clipmenu.4
CM_LOCKFILE=${CLIPMENU_DIR}/lock
lock_timeout=2
PASS_HASH_FILE=${CLIPMENU_DIR}/hash
CM_LINES=${CLIPMENU_DIR}/line_cache
CM_LINES_TMP=${CLIPMENU_DIR}/line_cache.tmp

PASS_PATH=$(
    find ${PASS_DIR} -type f -and -not -name '.*' -printf "%T@ %p\n" \
    | sort -n \
    | awk '{print $2}' \
    | xargs -I {} realpath --relative-to ${PASS_DIR} {} \
    | sed 's/.gpg$//' \
    | rofi -dmenu -sync -i -font "mono 6" -p "Pass :" -scroll-method 1 -lines 30 -matching glob -hide-scrollbar -line-margin 0
)

[[ $PASS_PATH ]] || exit 1

# update mtime to make most recent used passwd displayed first
touch $PATH_DIR/$PASS_PATH.gpg

exec {lock_fd}> "$CM_LOCKFILE"

if ! flock -x -w "$lock_timeout" "$lock_fd"; then
    printf 'ERROR: %s\n' 'Timed out waiting for lock' >&2
    exit 1
fi

# the lock is released at exit whatever happens
trap "flock -u \"$lock_fd\"" EXIT

PASSWORD=$(pass show ${PASS_PATH})
username=$(basename ${PASS_PATH})

[[ $PASSWORD ]] || exit 1

# clean old occurences of the pass
grep -v "${PASSWORD}" $CM_LINES > $CM_LINES_TMP
mv $CM_LINES_TMP $CM_LINES
sum=$(cksum <<< "$PASSWORD")
rm "$CLIPMENU_DIR/$sum" || true

# add the password hash in ignore file
#grep -v "${PASSWORD}" $CM_LINES
found=false
touch ${PASS_HASH_FILE}
chmod 600 ${PASS_HASH_FILE}

for line in $(cat ${PASS_HASH_FILE}); do
    if $( python -c "import sys, crypt; data = sys.stdin.read(); print( crypt.crypt(data[:-1], '${line}') == '${line}' and 'true' or 'false')" <<< ${PASSWORD}) ; then
        found=true
        break
    fi
done

if ! $found ; then
    python -c "import crypt, sys; data = sys.stdin.read(); print(crypt.crypt(data[:-1], crypt.mksalt(crypt.METHOD_SHA512)))" <<< ${PASSWORD} >> ${PASS_HASH_FILE}
fi


PASSWORD_STORE_X_SELECTION=primary pass show -c $PASS_PATH
xclip -selection clipboard <<< "$username"
