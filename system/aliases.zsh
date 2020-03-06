#!/bin/sh
if [ "$(uname -s)" = "Darwin" ]; then
	alias ls="ls -FG"
else
	alias ls="ls -F --color"
fi
alias l="ls -lAh"
alias la="ls -A"
alias ll="ls -l"

alias grep="grep --color=auto"
alias duf="du -sh * | sort -hr"
alias less="less -r"

# quick hack to make watch work with aliases
alias watch='watch '
