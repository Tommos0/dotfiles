#export TWITCH_INGEST=rtmp://live-ams.twitch.tv/app/$(cat ~/keys/twitch)
#alias start_twitch="ffmpeg -f pulse -i default -video_size 1920x1080 -f x11grab -itsoffset 0.4 -i :0.0 -vcodec libx264 -pix_fmt yuv420p -b:v 1M -acodec aac -b:a 128k -f flv ${TWITCH_INGEST}"
#alias start_rec="ffmpeg -f pulse -i default -video_size 1920x1080 -f x11grab -itsoffset 0.1 -i :0.0 -pix_fmt yuv420p \"/home/tom/Videos/rec-\$(date).mp4\""
# If you come from bash you might have to change your $PATH.
export PATH=$HOME/bin:/usr/local/bin:/snap/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH=~/.oh-my-zsh

# Edefault edotpr
export EDITOR=vim

# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="robbyrussell"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git zsh-autosuggestions zsh-syntax-highlighting)

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/rsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
#alias fd="fdfind"
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

#export ANDROID_HOME=$HOME/android-sdk
export ANDROID_SDK_ROOT=/opt/android-sdk
export ANDROID_HOME=/opt/android-sdk
export JAVA_HOME=/usr/lib/jvm/java-8-openjdk
export PATH=$PATH:$ANDROID_HOME/tools
export PATH=$PATH:$ANDROID_HOME/platform-tools

export PATH=$PATH:$HOME/.local/bin

#export PATH="$HOME/.pyenv/bin:$PATH"
#eval "$(pyenv init -)"
export WORKON_HOME=$HOME/.virtualenvs

export CHROME_BIN=`which chromium`
alias p=python
alias x=xdg-open
# added by travis gem

export PATH="$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH"
#export GOPATH="$HOME/Projects/go"
export PATH="$PATH:$GOPATH/bin"
#alias pycharm="/home/tom/Software/pycharm/bin/pycharm.sh" 
#export JAVA_HOME=/usr/lib/jvm/java-10-openjdk-amd64
export PATH=$PATH:$JAVA_HOME/bin

#loadenv() {
#  export $(cat "$1" | egrep -v "^#" | xargs)
#}

loadenv () {
	cat "$1" | egrep -v "^#" | while read -r a
	do
		export $a
	done
}


#export KUBECONFIG=~/.kube/eyra-staging-kubeconfig.yaml
export KUBECONFIG=~/.kube/eyra-aws.yaml
source <(kubectl completion zsh)

export PATH="$PATH:/snap/bin"
alias open="xdg-open"
alias rnd="~/Software/rnd/React\ Native\ Debugger"
alias c="xclip -selection clipboard"
#alias c='printf %s "$(< /dev/stdin)" | xclip -selection c'
alias v="xclip -o -selection clipboard"
alias ks='AWS_PROFILE=eyra kubectl --context aws -n eyra-staging'
alias kp='AWS_PROFILE=eyra kubectl --context aws -n eyra-prd'
alias kd='AWS_PROFILE=eyra kubectl --context minikube -n eyra-dev'
alias km='AWS_PROFILE=eyra microk8s.kubectl -n eyra-dev'
alias watch='watch '

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!

#l_conda_setup="$('/home/tomk/miniconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
#if [ $? -eq 0 ]; then
#    eval "$__conda_setup"
#else
#    if [ -f "/home/tomk/miniconda3/etc/profile.d/conda.sh" ]; then
#        . "/home/tomk/miniconda3/etc/profile.d/conda.sh"
#    else
#        export PATH="/home/tomk/miniconda3/bin:$PATH"
#    fi
#fi
#unset __conda_setup

# <<< conda initialize <<<

#source /home/tomk/.gvm/scripts/gvm
#gvm use go1.13.5 > /dev/null
#export AWS_PROFILE=eyra
#export AWS_PROFILE=escience
alias vim=nvim

#source ~/.config/broot/launcher/bash/br

#export PATH=$PATH:~/.poetry/bin
alias l="ls --color=auto -lahrt"

export DENO_INSTALL="/home/tomk/.deno"
export PATH="$DENO_INSTALL/bin:$PATH"


alias optirun="__NV_PRIME_RENDER_OFFLOAD=1 __GLX_VENDOR_LIBRARY_NAME="nvidia" __VK_LAYER_NV_optimus="NVIDIA_only" __GL_SHOW_GRAPHICS_OSD=1"
alias gl="git log --graph --abbrev-commit --decorate --format=format:'%C(bold blue)%h%C(reset) - %C(bold green)(%ar)%C(reset) %C(white)%s%C(reset) %C(dim white)- %an%C(reset)%C(bold yellow)%d%C(reset)' --all"
