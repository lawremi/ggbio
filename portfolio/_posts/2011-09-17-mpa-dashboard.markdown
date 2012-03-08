---
layout: portfolio
title: MPA Dashboard
category: portfolio
permalink: /portfolio/mpa-dashboard/
thumb: /images/mpadash-thumb.png
full: /images/mpadash-full.png
link: http://andrewheiss.com/mpadash/
type: web
details:
- Drupal-based digital signage
- No manual updating required
---

This dashboard runs on a Mac Mini connected to a large plasma screen in the BYU MPA lounge. It pulls in information from different iCal calendars and RSS feeds and displays at-a-glance information about upcoming internships and jobs, Marriott School events, activities, conferences, student and faculty birthdays, and class deadlines.

Because the dashboard pulls information from other websites and services, it requires almost no maintenance. It's magic. :)

I based the design loosely on [Panic's incredible status board](http://www.panic.com/blog/2010/03/the-panic-status-board/). It runs on Drupal 6 with a bunch of modules that make the magic work. I use [Feeds](http://drupal.org/project/feeds) to aggregate and parse the RSS and iCal feeds, [Views](http://drupal.org/project/views) to filter and display the content in individual blocks, and [Views Slideshow](http://drupal.org/project/views_slideshow) to loop through all the different blocks and regions. 