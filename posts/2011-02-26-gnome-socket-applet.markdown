---
title: A Gnome Socket Applet
description: Making Gnome play nicely with xmonad and friends
tags: haskell
---

I'm an enthusiastic user of the [xmonad] window manager. It allows me to be more
productive while coding, and to focus on what I'm doing.

[xmonad]: http://xmonad.org/

When I got my new laptop (about 3 months ago), I decided to give [Gnome] another
try. Together with xmonad, it makes a nice combination, since you get the
excellent integration of the different gnome components, and the simplicity of
xmonad.  

[Gnome]: http://www.gnome.org/

When you use the default gnome panel, you get a list of windows, so you can see
which windows are open. However, for xmonad, this is kind of redundant: since it
is a tiling window manager, you can just see what windows are open. Instead, you
want other information in your panel: the currently used xmonad layout, the
title of the focused window...

So, I wrote a small applet called [gnome-socket-applet] which allows simple
communication from xmonad to the gnome panel. You simply add the applet to your
panel, and it will act as a small server, listening on a port you specified.

[gnome-socket-applet]: http://github.com/jaspervdj/gnome-socket-applet 

Then, xmonad simply sets the applet text by creating a connection to the applet
server and writing the desired text. In my case, the applet looks like this:

![Screenshot of the applet](/images/2011-02-26-gnome-socket-applet.png)

The applet is written in C and should work together with most window managers
that support adding custom hooks. Install instructions and example code for
xmonad are also given on the [gnome-socket-applet] page.
