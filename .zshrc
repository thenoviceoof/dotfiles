# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="frisky"

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Comment this out to disable weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while
# waiting for completion
COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in
# ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...
export PATH=/usr/lib/lightdm/lightdm:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games

PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm"

PATH=$PATH:$HOME/.local/bin
PATH=$PATH:$HOME/.cljr/bin

# assumes .local/bin/e.sh -> e is there
export EDITOR="e"

# customize git prompt

zsh_theme_git_prompt_diff () {
    DIFFSTAT=`git diff --shortstat`
    PLUSSTAT=`echo "$DIFFSTAT" | grep -oE "[0-9]+ insert" |  grep -oE "[0-9]+"`
    MINUSTAT=`echo "$DIFFSTAT" | grep -oE "[0-9]+ deleti" |  grep -oE "[0-9]+"`
    STATPROMPT=""
    if [ -n "$PLUSSTAT" ]; then
        STATPROMPT="+$PLUSSTAT"
    fi
    if [ -n "$MINUSTAT" ]; then
        STATPROMPT="$STATPROMPT-$MINUSTAT"
    fi
    if [ -n "$STATPROMPT" ]; then
        STATPROMPT="%{$fg[red]%}$STATPROMPT%{$fg[green]%}"
    fi
    echo "$STATPROMPT"
}

zsh_theme_git_prompt_stash () {
    STASHCOUNT=`git stash list | wc -l`
    if [ 0 -ne "$STASHCOUNT" ]; then
        echo -n "%{$fg[yellow]%}"
        printf '?%.0s' {1..$STASHCOUNT}
        echo -n "%{$fg[green]%}"
    fi
}

git_prompt_info () {
    ref=$(command git symbolic-ref HEAD 2> /dev/null) || \
    ref=$(command git rev-parse --short HEAD 2> /dev/null) || return 0
    echo "%{$fg[green]%}[${ref#refs/heads/}$(zsh_theme_git_prompt_diff)$(zsh_theme_git_prompt_stash)] "
}
