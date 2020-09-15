# Notes on building a new OSX machine

## fix system defaults
* system->trackpad, scroll direction not natural
* system->keyboard
  * shortcuts -> mission control
    * applications windows --> control+shift+down
    * move left/right space to control+shift+left/right
    * mission control to control/shift up
* system->Dock->auto hide and show the dock


## install from ORNL managed software
* corkscrew
* xcode
* xquartz
* commvault


## install from dmg/pkg/appstore
* [iterm2](https://iterm2.com/downloads.html)
* [karabiner-elements](https://karabiner-elements.pqrs.org/)

## install zsh plugins
* install zsh:  `chsh -s /bin/zsh`
* install oh-my-zsh: `sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"`


## homebrew

install brew:

/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"




## set up karabiner

* simple modifications
  * caps_lock --> left_control

### old notes, may be needed for fancy keyboard
emacs mode -> C-D to forward delete
      	   -> C-V to page down
	   -> M-V to page up
	   -> C-K to command+shift_right,command+x
custom: -> home, page up, end map to command_l, etc
	-> optoin+tab sends command+`
	-> option+c+arrow for swapping display
	-> iterm2 tab togles next tab or pane
	-> x11 option sends esc
	-> iterm2 control+l/r sends esc+b/f


