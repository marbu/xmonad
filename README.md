# My Xmonad configuration

This is rather simple [Xmonad](http://www.haskell.org/haskellwiki/Xmonad)
configuration with KDE integration which would also work standalone.

## Inspiration

This configuration is based on the following examples:

 * [Using xmonad in KDE](http://www.haskell.org/haskellwiki/Xmonad/Using_xmonad_in_KDE)
 * [John Goerzen's Configuration](http://www.haskell.org/haskellwiki/Xmonad/Config_archive/John_Goerzen's_Configuration)
 * Fedora default configuraton (package `xmonad-config`, see [xmonad builds](http://koji.fedoraproject.org/koji/packageinfo?packageID=8370))
 * yakuake-like [XMonad Scratchpad](http://pbrisbin.com/posts/xmonad_scratchpad/)

## Quick setup

~~~
$ git clone https://github.com/marbu/xmonad.git ~/.xmonad
$ mkdir ~/.kde/env
$ echo 'KDEWM=/usr/bin/xmonad' > ~/.kde/env/set_window_manager.sh
$ chmod u+x ~/.kde/env/set_window_manager.sh
~~~

Note: runner (in KDE triggered via alt-f2) is binded to meta-p as expected,
but you should configure it to use floating mode (otherwise the input line
loses focus and you are not able to type anything in it). This is a reported
[bug of xmonad-contrib](http://code.google.com/p/xmonad/issues/detail?id=430).
