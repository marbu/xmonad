# My Xmonad configuration

This is rather simple [Xmonad](https://xmonad.org/) configuration with
[KDE](https://kde.org/) integration which would also work standalone.

## Inspiration

Primary source of information for Xmonad KDE integration:

 * [Using xmonad in KDE](http://www.haskell.org/haskellwiki/Xmonad/Using_xmonad_in_KDE)

Moreover this configuration is based on the following examples:

 * [John Goerzen's Configuration](http://www.haskell.org/haskellwiki/Xmonad/Config_archive/John_Goerzen's_Configuration)
 * Fedora default configuraton (package `xmonad-config`, see [xmonad builds](http://koji.fedoraproject.org/koji/packageinfo?packageID=8370))
 * yakuake-like [Xmonad Scratchpad](http://pbrisbin.com/posts/xmonad_scratchpad/)
 * [Cycling through workspaces in multi monitor setup in
   XMonad](https://stackoverflow.com/questions/44355436)
 * [General xmonad.hs config tips from haskell
   wiki](https://wiki.haskell.org/Xmonad/General_xmonad.hs_config_tips)

## Quick setup

Install Xmonad and clone this repository into `~/.xmonad` directory:

~~~
$ git clone https://github.com/marbu/xmonad.git ~/.xmonad
~~~

### KDE 4

~~~
$ mkdir -p ~/.kde/env
$ echo 'KDEWM=/usr/bin/xmonad' > ~/.kde/env/set_window_manager.sh
~~~

Note: runner (in KDE triggered via alt-f2) is binded to meta-p as expected,
but you should configure it to use floating mode (otherwise the input line
loses focus and you are not able to type anything in it). This is a reported
[bug of xmonad-contrib](http://code.google.com/p/xmonad/issues/detail?id=430).

### KDE 5 Plasma Workspace

Compared to KDE 4, the only difference is the location of configuration files:

~~~
$ mkdir -p ~/.config/plasma-workspace/env
$ echo 'export KDEWM=/usr/bin/xmonad' > ~/.config/plasma-workspace/env/set_window_manager.sh
~~~

Note: Plasma now uses meta key in some default keybindings (eg. meta-tab is
catched by Plasma and not Xmonad) which needs to be reconfigured.

Unfortunatelly, there are few new issues compared to KDE 4 with xmonad:

 * ~~[KDE Bug 351141](https://bugs.kde.org/show_bug.cgi?id=351141)~~
   fixed in Plasma 5.8
 * [KDE Bug 351706](https://bugs.kde.org/show_bug.cgi?id=351706)
 * multiple screens mess with window placement (most of windows from multiple
   virtual desktops end up on single virt. desktop when screen is connected
   or disconnected) and this *may be* a consequence of new KDE 5 feature which
   maintains separate desktop configuration when new screen is attached
   (further investigation needed)
