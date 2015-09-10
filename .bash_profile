# Python
PATH="/Library/Frameworks/Python.framework/Versions/2.7/bin:${PATH}"
export PATH

# Show current git branch in prompt:
parse_git_branch() {
    git branch 2> /dev/null | sed -e "/^[^*]/d" -e "s/* \(.*\)/(\1)/"
}

# Customizations to prompt:
export PS1="\[\033[36m\]\u\[\033[m\]@\[\033[32m\]\h:\[\033[33;1m\]\w\[\033[34m\]\$(parse_git_branch)\[\033[m\]\$ "
export CLICOLOR=1
export LSCOLORS=ExFxBxDxCxegedabagacad
alias ls="ls -GFh"
export PATH=/usr/local/gcc-arm-none-eabi-4_8-2014q2/bin:$PATH
alias blender="/Applications/Blender/blender.app/Contents/MacOS/blender"
export ETS_TOOLKIT=qt4

# Better shortcut for Aquamacs:
function a
{
    for f in "$@"
    do
 	test -e $f || touch $f
    done
    open -a /Applications/Aquamacs.app "$@"
}
export EDITOR=~/bin/ec.sh
alias e="$EDITOR"
