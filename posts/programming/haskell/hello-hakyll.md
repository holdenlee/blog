---
title: Hello Hakyll!
subtitle: Setting up this blog
published: 2016-01-30
modified: 2016-01-30
tags: haskell, blogging, web design
---

# Why Hakyll?

I've decided to switch blogging platforms from Wordpress to Hakyll, a website generation platform that can be customized using Haskell. All previous posts will remain on [Wordpress site](http://holdenlee.wordpress.com/), but it will no longer be updated.

The following two blog posts articulate some of my reasons for switching to Hakyll:

* [Why switch from Wordpress to Jekyll?](http://karpathy.github.io/2014/07/01/switching-to-jekyll/)
* [Why switch from Jekyll to Hakyll?](http://mark.reid.name/blog/switching-to-hakyll.html)

The main reason is that I want to have complete control over the content:

1. I want to edit blog posts as text (markdown) files on my computer, rather than inside an editor in the browser. Firstly, it's simpler to edit them as text files. For me, using a formatting toolbar for editing posts (lists, bold, quotes, etc.) has hurt me more than it's helped: often the bulleted lists get corrupted and I have to mess with the html t fix them). In Markdown, I can do all the basic formatting very simply, without having to click something external to the text, or work with something clunky like html.
2. Having blog posts as simple files means it is easy to import and export material that I write, or convert it to a different format as I wish. I can organize blog posts as I do my own file system. See [Gwern](http://www.gwern.net/About#long-site), especially the quote by Julian Assange, on why a simple format, good organization, and control over content help one's writing survive.
3. I have complete control in the sense that I can create arbitrary scripts to turn the text files into published posts. For example, I can add footnotes, enable latex macros, enable comments, and automatically build a hierarchical sitemap of all posts. I can make arbitrary macros for common patterns (ex. links to wikipedia pages) if I wanted to. Moreover, I can write the scripts in Haskell!
4. There are many beautiful blogs and websites written in Haskell, for example, [Chris Olah's blog](http://colah.github.io) and [Gwern's site](http://www.gwern.net/).

In the rest of this post, I'll detail how I set up the blog. This is still very much a work in progress! Many people have written excellent tutorials already, so I will give links to their articles rather than re-explaining.

It took me a long time to set up the blog, mainly because there were a lot of features I wanted to add, and I had to look for instructions on many different sites. I hope this post will be a useful "directory" on how to add various features.


