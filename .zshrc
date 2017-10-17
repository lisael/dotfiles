# The following lines were added by compinstall

zstyle ':completion:*' completer _expand _complete _ignored _approximate
zstyle ':completion:*' matcher-list 'm:{[:lower:]}={[:upper:]}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
zstyle :compinstall filename '/home/lisael/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall
# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000000
SAVEHIST=1000000
setopt appendhistory autocd
bindkey -v
# End of lines configured by zsh-newuser-install
#source /usr/lib/python2.7/site-packages/powerline/bindings/zsh/powerline.zsh
source ~/.profile
source /bin/liquidprompt

export EDITOR=kak

preprod(){
     ssh vfw-rh2-preprod.novapost.net
}

prod(){
     ssh vfw-rh2-prod.novapost.net
}

PATH+=:/home/lisael/bin:/home/lisael/perl5/bin

export PATH
export GOPATH=/home/lisael/projects/perso/go
export GOBIN=/home/lisael/projects/perso/go/bin

export LISAEL_IP=62.210.237.25
export LISAEL_IP2=163.172.56.220
source /usr/bin/virtualenvwrapper.sh
source ~/.bash_aliases
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
export XDG_CONFIG_HOME=~/.config
export PAGER=$XDG_CONFIG_HOME/kak/bin/pager
export PAGER=vimpager
# export MANPAGER=$XDG_CONFIG_HOME/kak/bin/man-pager
export TERMINAL=xfce4-terminal

#PATH="/home/lisael/perl5/bin${PATH:+:${PATH}}"; export PATH;
#PERL5LIB="/home/lisael/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
#PERL_LOCAL_LIB_ROOT="/home/lisael/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
#PERL_MB_OPT="--install_base \"/home/lisael/perl5\""; export PERL_MB_OPT;
#PERL_MM_OPT="INSTALL_BASE=/home/lisael/perl5"; export PERL_MM_OPT;

eval "$(fasd --init auto)"
alias k='f -e kak'

h(){
    cmd="$(history 0 | awk '{$1="";print}' | tac | awk '!seen[$0]++' | fzf --no-sort -e -q "$@")"
    echo "$cmd"
    eval $cmd
}

kg(){
    kak -e "grep \"$1\" $2"
}

eval $(thefuck --alias)

y(){
   printf "%s" "$(history -1 | awk '{$1=""; print}' | sed 's/^\s\(.*\)$/\1/')" | xsel -i 
}

add(){
    GIT_DIR=$(dirname $(realpath -s $(git rev-parse --git-dir)))
    if [ "$GIT_DIR" != "" ]; then
        for file in $( git status --porcelain \
                     | awk -v GIT_DIR=$GIT_DIR '{print $1 " " GIT_DIR "/" $2 }' \
                     | awk '{"realpath -s --relative-to . " $2 | getline path; print $1 " " path}' \
                     | rofi -dmenu -sync -i -font "mono 6" -p 'git add:' \
                       -scroll-method 1 -lines 30 -matching glob \
                       -hide-scrollbar -line-margin 0 -multi-select \
                     | awk '{print $2}'); do
            git add $file
        done
    fi
}

ignore(){
    GIT_DIR=$(dirname $(realpath -s $(git rev-parse --git-dir)))
    if [ "$GIT_DIR" != "" ]; then
        for file in $( git status --porcelain \
                     | grep -e "^??" \
                     | awk -v GIT_DIR=$GIT_DIR '{print $1 " " GIT_DIR "/" $2 }' \
                     | awk '{"realpath -s --relative-to . " $2 | getline path; print $1 " " path}' \
                     | rofi -dmenu -sync -i -font "mono 6" -p 'git ignore:' \
                       -scroll-method 1 -lines 30 -matching glob \
                       -hide-scrollbar -line-margin 0 -multi-select \
                     | awk '{print $2}'); do
            file=$(realpath --relative-to $GIT_DIR $file)
            echo $file
            echo $file >> $GIT_DIR/.gitignore
        done
    fi
}

prune_files(){
    GIT_DIR=$(dirname $(realpath -s $(git rev-parse --git-dir)))
    if [ "$GIT_DIR" != "" ]; then
        for file in $( git status --porcelain \
                     | grep -e "^??" \
                     | awk -v GIT_DIR=$GIT_DIR '{print $1 " " GIT_DIR "/" $2 }' \
                     | awk '{"realpath -s --relative-to . " $2 | getline path; print $1 " " path}' \
                     | rofi -dmenu -sync -i -font "mono 6" -p 'rm: ' \
                       -scroll-method 1 -lines 30 -matching glob \
                       -hide-scrollbar -line-margin 0 -multi-select \
                     | awk '{print $2}'); do
            rm $file
        done
    fi
}


commit(){
    GIT_DIR=$(dirname $(realpath -s $(git rev-parse --git-dir)))
    if [ "$GIT_DIR" != "" ]; then
        add
        git commit
    fi
}

fixup(){
    add
    sha=$( git log --oneline \
         | rofi -dmenu -sync -i -font "mono 6" -p 'git commit --fixup:' \
           -scroll-method 1 -lines 30 -matching glob \
           -hide-scrollbar -line-margin 0 \
         | awk '{print $1}')
    git commit --fixup $sha
}

show(){
    sha=$( git log --oneline \
         | rofi -dmenu -sync -i -font "mono 6" -p 'git show:' \
           -scroll-method 1 -lines 30 -matching glob \
           -hide-scrollbar -line-margin 0 \
         | awk '{print $1}')
    git show $sha
}

squash(){
    msg=$( git log --oneline | grep 'fixup!' | head -1 | sed 's/^[0-9a-f]*\s*fixup!\s*//' )
    sha=$( git log --oneline | grep "$msg" | head -2 | tail -1 | awk '{print $1}' )
    # git log --oneline | grep "$msg" | head -2 | tail -1 | awk '{print $1}'
    git rebase -i --autosquash ${sha}~1
}

unstage(){
    GIT_DIR=$(dirname $(realpath -s $(git rev-parse --git-dir)))
    if [ "$GIT_DIR" != "" ]; then
        for file in $( git status --porcelain \
                     | grep -v "^??" \
                     | grep -v "^ " \
                     | awk '{print $2}' \
                     | xargs -n 1 printf "%s/%s\n" $GIT_DIR \
                     | xargs realpath -s --relative-to . \
                     | rofi -dmenu -sync -i -font "mono 6" -p 'git unstage:' \
                       -scroll-method 1 -lines 30 -matching glob \
                       -hide-scrollbar -line-margin 0 -multi-select); do
            git reset HEAD $file
        done
    fi
}

rsh(){
    host=$( cat ~/.ssh/config \
          | grep "^\s*Host\(Name\)\?" \
          | grep -v "\*" \
          | sed 's/^\s*//' \
          | awk '{print $2}' \
          | rofi -dmenu -sync -i -font "mono 6" -p 'ssh to:' \
            -scroll-method 1 -lines 30 -matching glob \
            -hide-scrollbar -line-margin 0 ) || return
    ssh $host
}

checkout(){
    branch=$({
                 git for-each-ref --sort=-committerdate refs/heads --format='%(objectname) B %(refname:short) %(upstream:short)%(upstream:trackshort) â %(contents:subject) â %(authorname) â (%(committerdate:relative))'
                 git for-each-ref --sort=-committerdate refs/remotes --format='%(objectname) R %(refname:short) %(upstream:short)%(upstream:trackshort) â %(contents:subject) â %(authorname) â (%(committerdate:relative))'
                 git for-each-ref --sort=-committerdate refs/tags --format='%(objectname) T %(refname:short) %(upstream:short)%(upstream:trackshort) â %(contents:subject) â %(authorname) â (%(committerdate:relative))'
             } | awk '!seen[$1]++ {for (i=2; i<NF; i++) printf $i " "; print $NF}' \
               | column -s "â" -t \
               | rofi -dmenu -sync -i -font "mono 6" -p 'git checkout:' \
                 -scroll-method 1 -lines 30 -width 80 -matching glob \
                 -hide-scrollbar -line-margin 0 ) || return

    branch=$(echo $branch | awk '{print $2}')
    final_branch=$branch
    for name in $(git remote); do
        prefix=${name}/
        if [[ "$branch" = ${prefix}* ]]; then
            final_branch=${branch#$prefix}
        fi
    done

    git checkout $final_branch
}

gitclean(){
    git remote prune origin
    git fetch --prune
    git branch -r | awk '{print $1}' | egrep -v -f /dev/fd/0 <(git branch -vv | grep origin) | awk '{print $1}' | xargs git branch -d
    prune_files
}

rmake(){
    target=$( make -p 2&>/dev/null \
            | grep -A 100000 "# Files" \
            | grep -v "^$" \
            | grep -v "^\(\s\|#\|\.\)" \
            | grep -v "Makefile:" \
            | cut -d ":" -f 1 \
            | rofi -dmenu -sync -i -font "mono 6" -p 'make:' \
              -scroll-method 1 -lines 30 -matching glob \
              -hide-scrollbar -line-margin 0 )
    make $target
}
