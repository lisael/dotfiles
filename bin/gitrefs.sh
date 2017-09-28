#! /bin/bash

branch=$({
             git for-each-ref --sort=-committerdate refs/heads --format='%(objectname) %(refname:short) %(upstream:short)%(upstream:trackshort) - %(contents:subject) - %(authorname) (%(committerdate:relative))'
             git for-each-ref --sort=-committerdate refs/remotes --format='%(objectname) %(refname:short) %(upstream:short)%(upstream:trackshort) - %(contents:subject) - %(authorname) (%(committerdate:relative))'
             git for-each-ref --sort=-committerdate refs/tags --format='%(objectname) %(refname:short) %(upstream:short)%(upstream:trackshort) - %(contents:subject) - %(authorname) (%(committerdate:relative))'
         } | awk '!seen[$1]++ {for (i=2; i<NF; i++) printf $i " "; print $NF}' \
           | rofi -dmenu -sync -i -font "mono 6" -p 'make:' \
             -scroll-method 1 -lines 30 -matching glob \
             -hide-scrollbar -line-margin 0 )

branch=$(echo $branch | awk '{print $1}')
final_branch=$branch
for name in $(git remote); do
    prefix=${name}/
    if [[ "$branch" = ${prefix}* ]]; then
        final_branch=${branch#$prefix}
    fi
done

git checkout $final_branch
