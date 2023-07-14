---
title: Monitor calibration and Linux
description: How I set up color management on my Linux system
tags: photography
---

# Introduction

Not all monitors are alike.  If you are one of the many people who have an
external monitor sitting next to their laptop, you might have noticed at some
point that colors on one monitor look slightly (or sometimes wildly) different
than on the other monitor.  When you view a picture on your smartphone, colors
may be more saturated than on your computer, and vice versa.

<div class="figure flickr">
<a href="https://www.flickr.com/photos/jaspervdj/22911640999/">
<img src="/images/2016-07-06-florence-iii.jpg" width="600">
</a>
</div>

This is not a big problem when I am programming or reading, but it becomes an
issue when you are doing any visual stuff -- including HTML/CSS.  For example,
when I am editing a picture on my external monitor, I might notice that the reds
look a bit too orange-like.  But on my laptop screen, they look fine.  Does this
mean I should shift them or not?  Which monitor should I _"trust"_?

# Generating ICC profiles

The answer is simple: for things like this, you can only trust _calibrated_
monitors.  There are many tools to calibrate a monitor, and most systems provide
some sort of calibration wizard that asks you a bunch of questions.  However, if
you take digital photography (or any other hobby involving digital visuals)
seriously, the only answer is a hardware calibrator.  Without one, editing
pictures is guesswork at best.

Unfortunately, these are not free.  I picked up a [DataColor] Spyder4Express for
around $150.  I'm not sure if that particular model is still available, but
there are newer models now for similar prices.  It is not cheap, but it is still
a lot less than the average lens, and I think this benefits you more than buying
another lens in a lot of cases.  Of course, you can also consider borrowing one.
Calibrating once every month or so is more than enough.

[DataColor]: http://www.datacolor.com/

<div class="figure flickr">
<a href="https://www.flickr.com/photos/jaspervdj/27684257125/">
<img src="/images/2016-07-06-bushwick-ii.jpg" width="600">
</a>
</div>

In general, these calibrators (or wizards) generate [ICC profiles] (`.icm` or
`.icc` files).  Such a file is basically a mapping between color spaces,
in this case for a specific monitor.

[ICC profiles]: https://en.wikipedia.org/wiki/ICC_profile

Sometimes, generating and finding these files is a bit tricky.  For example, the
calibrator I have does not support Linux (but I dual boot Windows).

Additionally, it has the arbitrary restriction that you can only calibrate a
single monitor when using the software directly (it is almost like they really
want you to get the expensive model!).  The problem, however, is easily solved
by calibrating one monitor, copying the ICC profile (usually in
`/Windows/System32/spool/drivers/color/`) somewhere and then calibrating other
monitors.  Now, what remains is loading these profiles in Linux.

# Linux scripting

While there are a few
[solutions](http://www.argyllcms.com/)
[available](http://xcalib.sourceforge.net/),
I decided to implement my own in a simple script to learn a bit more about these
things -- simply because stuff like this is always interesting.

<div class="figure flickr">
<a href="https://www.flickr.com/photos/jaspervdj/28066452716/">
<img src="/images/2016-07-06-frederick-ii.jpg" width="600">
</a>
</div>

It turns out that monitors can be identified by reading their [EDID] block.
This is (usually) 128 bytes of binary data, that contains, among other things,
human-readable names for most monitors.  The whole EDID block can be extracted
by a tool like [xrandr], but we need some code to parse the data.  This [Python]
snippet extracts the human-readable parts from an EDID block. These can appear
at various offsets but the tags that precede them are known.

[xrandr]: https://www.x.org/wiki/Projects/XRandR/
[EDID]: http://read.pudn.com/downloads110/ebook/456020/E-EDID%20Standard.pdf
[Python]: https://www.python.org/

~~~~~{.python}
EDID_NAME_TAGS = [
    bytearray.fromhex('000000fc00'),
    bytearray.fromhex('000000fe00')
]

def name_from_edid(edid):
  names = []
  for i in range(0, 6):
    offset = 36 + i * 18
    tag = edid[offset : offset + 5]
    val = edid[offset + 5 : offset + 18]
    if tag in EDID_NAME_TAGS:
      names += [val.decode('utf-8').strip()]
  return '-'.join(names)
~~~~~

For my ThinkPad's monitor, this returns `LG-Display-LP140QH1-SPB1`.

The rest is fairly trivial.  The [complete script] reads human-readable names
from the EDID blocks and then finds the corresponding files in
`$HOME/.color/icc`.  If such a file is found, it uses `dispwin` from `argyllcms`
to set the profile for that monitor.

    $ colorprof
    [DP1] checking: /home/jasper/.color/icc/SyncMaster.icc
    [DP1] running: dispwin -d 2 /home/jasper/.color/icc/SyncMaster.icc
    [eDP1] checking: /home/jasper/.color/icc/LG-Display-LP140QH1-SPB1.icc
    [eDP1] running: dispwin -d 1 /home/jasper/.color/icc/LG-Display-LP140QH1-SPB1.icc

[complete script]: https://github.com/jaspervdj/dotfiles/blob/master/bin/colorprof
