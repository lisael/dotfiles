# dotfiles

My dotfiles. Mostly for my own usage but feel free to pick whatever you want.

## Most intersting stuff

Lots of files here are just borring configuration and are full of quick and 
dirty ininteresting stuff. Some parts however may worth it.

- `.xmonad/` stuff is quite clean (help text missing, maybe)
- in `bin/` you may apreciate:
    - `dotify` a simple script that moves a file to my dotfiles repo and
      creates a symlink
    - the `easy*` series, to navigate to any window on the curent workspace
      in two keystrokes
    - `find_pass` rofi-based wrapper around `pass` password-store
- in my `.zshrc` I find git helpers usefull (`commit`, `fixup`, `checkout`,
  `squash`, `prune_files`, `git_clean`, `unstage`). These may become a script
  with less code duplication, and a set of aliases

## Requirements

From the to of my head, I think the scripts need those packages (archlinux):

- xfce4 (although I suspect that most of the deps are useless)
- xfce4-goodies (???)
- rofi
- python-osd (doesn't exist in arch repo, I had to sudo_pip_install this one)
- python2-xlib
- python2-wnck
- xmonad
- xmonad-contrib
- awesome
- gnome-keyring
- pass
- tray
- xboomx
