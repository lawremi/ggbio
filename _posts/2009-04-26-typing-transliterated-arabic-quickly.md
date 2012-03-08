---
layout: post
title: Typing transliterated Arabic quickly
excerpt: Use text-replacement software to automate Arabic transliteration.
category: blog
tags:
- arabic
- automation
- ijmes
- texter
- transliteration
- typinator
---

Since Arabic doesn't use the Latin alphabet, and lots of the letters don't have Latin equivalents (خ, ع, ق, ط, for example), transliteration is necessary to show Arabic words and sounds in English writing. There is an easy way to type transliterated Arabic quickly, though, using macros to locate hidden Unicode characters used by many of the standard transliteration systems.

Unfortunately, there is no universally standard system for transliteration, and most systems use letters that aren't found on normal keyboards. One of the rising systems in the Middle East, nicknamed [Franco Arab](http://en.wikipedia.org/wiki/Arabic_Chat_Alphabet) in Egypt, is my least favorite. It only uses standard English letters, meaning it's useful for texting, e-mailing, and other things where it's difficult to write in real Arabic script. Biggest problem: it's ugly and hard to read.

For example, the name Great Britain (بريطانيا العظمى) uses several non-Latin letters. Written in Franco it looks like this: *bri6ania al3o'6ma*. For readers unfamiliar with Arabic (or even those who are, like me), it's always hard to remember what the random uppercase letters and numbers mean.

Fortunately, there are better systems. Here's Great Britain written using the [IJMES](http://www8.georgetown.edu/departments/history/ijmes/) (International Journal of Middle East Studies) [system](http://www8.georgetown.edu/departments/history/ijmes/Translit_Chart.pdf), also used in the Encyclopedia of Islam: *Brīṭānīyā al-ʿuẓmá*. Much easier to read.

Since the nonstandard Latin letters use Unicode glyphs, you need to use a font that has a full set of Unicode glyphs, like Times, Arial, Helvetica, and other standard fonts. You also have to hunt down all the special characters either in Word's Insert Special Character dialog or in the Glyphs panel in InDesign.

You can speed up the process of hunting for and inserting special characters by using a text substitution app like [Texter](http://lifehacker.com/software/texter/lifehacker-code-texter-windows-238306.php) for Windows (free, [open source](http://github.com/adampash/texter/tree/master)) or [Typinator](http://www.ergonis.com/products/typinator/) for Mac (not free). These programs can replace abbreviations that you type with preset phrases. For example, if you wanted to quickly type today's date you could set up a shortcut that would replace %date with the full date.

I set up a list of text replacements in my copy of Typinator that automatically change certain combinations of characters into IJMES standard transliterated rules. Here's my list of text transformation rules (all with the prefix -ij, short for IJMES):

* -ij' = &#x2BF;
* -ij\` = &#x2BE;
* -ija = &#x101;
* -ijd = &#x1E0D;
* -ijh = &#x1E25;
* -iji = &#x12B;
* -ijs = &#x1E63;
* -ijt = &#x1E6D;
* -iju = &#x16B;
* -ijz = &#x1E93;

Here's a (very) quick example of this in action:

<p><object width="400" height="300"><param name="allowfullscreen" value="true" /><param name="allowscriptaccess" value="always" /><param name="movie" value="http://vimeo.com/moogaloop.swf?clip_id=4337233&amp;server=vimeo.com&amp;show_title=1&amp;show_byline=1&amp;show_portrait=0&amp;color=&amp;fullscreen=1" /><embed src="http://vimeo.com/moogaloop.swf?clip_id=4337233&amp;server=vimeo.com&amp;show_title=1&amp;show_byline=1&amp;show_portrait=0&amp;color=&amp;fullscreen=1" type="application/x-shockwave-flash" allowfullscreen="true" allowscriptaccess="always" width="400" height="300"></embed></object></p>

You could set up similar rules for transliteration with different systems (even Franco), or even different languages. Typing IJMES transliterated words for academic papers just got infinitely easier.

<p class="confirmation warning">You will probably only want to use IJMES transliteration in print because of font encoding issues on different platforms and browsers. For online text you'll have to stick with Franco or something like it.</p>