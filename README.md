# My Xmonad configuration

This is rather simple [Xmonad](http://www.haskell.org/haskellwiki/Xmonad)
configuration with KDE integration (would also work
standalone) based on default one shipped with Fedora as well as these
examples:

 * [Using xmonad in KDE](http://www.haskell.org/haskellwiki/Xmonad/Using_xmonad_in_KDE)
 * [John Goerzen's Configuration](http://www.haskell.org/haskellwiki/Xmonad/Config_archive/John_Goerzen's_Configuration)

## Quick setup

~~~
$ git clone https://github.com/marbu/xmonad.git ~/.xmonad
$ mkdir ~/.kde/env
$ echo 'KDEWM=/usr/bin/xmonad' > ~/.kde/env/set_window_manager.sh
$ chmod u+x ~/.kde/env/set_window_manager.sh
~~~

Note: runner (in KDE triggered via alt-f2) is binded to meta-p as expected,
but you should configure it to use floating mode (otherwise the input
line loses focus and you are not able to type anything in it).