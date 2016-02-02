---
title: Hello Hakyll!
subtitle: Moving from Wordpress to Hakyll
published: 2016-02-01
modified: 2016-02-02
tags: haskell, blogging, web design
inline: True
---

# Why Hakyll?

I've decided to switch blogging platforms from Wordpress to [Hakyll](https://jaspervdj.be/hakyll/), a website generation platform that can be customized using Haskell. All previous posts will remain on [Wordpress site](http://holdenlee.wordpress.com/), but the site will no longer be updated.

The following two blog posts articulate some of my reasons for switching to Hakyll:

* [Why switch from Wordpress to Jekyll?](http://karpathy.github.io/2014/07/01/switching-to-jekyll/)
* [Why switch from Jekyll to Hakyll?](http://mark.reid.name/blog/switching-to-hakyll.html)

The main reason is that I want to have complete control over the content:

1. I want to edit blog posts as text (markdown) files on my computer, rather than inside an editor in the browser. Firstly, it's simpler to edit them as text files. Relying on a formatting toolbar for editing posts (lists, bold, quotes, etc.) can be frustrating: often the bulleted lists get corrupted and I have to mess with the html to fix them). In Markdown, I can do all the basic formatting very simply, without having to click something external to the text, or work directly with html.
2. Having blog posts as simple files means it is easy to import and export material that I write, or convert it to a different format as I wish. I can organize blog posts as I do my own file system. See [Gwern](http://www.gwern.net/About#long-site), especially the quote by Julian Assange, on why a simple format, good organization, and control over content help writing survive.
3. I have complete control in the sense that I can create arbitrary scripts to turn the text files into published posts. For example, I can add footnotes, enable latex macros, enable comments, and automatically build a hierarchical [sitemap](/sitemap.html) of all posts. I can make arbitrary macros for common patterns (ex. links to wikipedia pages) if I wanted to. Moreover, I can write the scripts in Haskell!
4. There are many beautiful blogs and websites written in Haskell, for example, [Chris Olah's blog](http://colah.github.io) and [Gwern's site](http://www.gwern.net/). Much of the design on this blog is copied from Chris Olah's blog and [oinkina](https://github.com/oinkina).

# Next steps

I plan to write a post on how I set up this blog. In the meantime I'm still working on setting up some features and improving the design. Any comments on the site (or bug reports, or pointers) are welcome.

I'm currently using TiddlyWiki for my [website](http://holdenlee.github.io). Right now I have mixed feelings about TiddlyWiki - it's a easy-to-update wiki with many great features, but doesn't offer the simplicity and control that a Hakyll site does. I may change it in the future, but for time being I'll keep it as it is.
