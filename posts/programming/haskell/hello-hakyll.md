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
4. There are many beautiful blogs and websites written in Haskell, for example, [Chris Olah's blog](http://colah.github.io) and [Gwern's site](http://www.gwern.net/). Much of the design on this blog is copied from Chris Olah's blog and 

In the rest of this post, I'll detail how I set up the blog. This is still very much a work in progress! Many people have written excellent tutorials already, so I will give links to their articles rather than re-explaining.

It took me a long time to set up the blog, mainly because there were a lot of features I wanted to add, and I had to look for instructions on many different sites. I hope this post will be a useful "directory" on how to add various features.

# Setting up Hakyll

1. First install the [Haskell Platform](https://www.haskell.org/downloads) if you haven't already.
2. Next install [Hakyll](https://jaspervdj.be/hakyll/tutorials/01-installation.html) using cabal. Initialize a default Hakyll website (the command below initializes the site in directory "my-site").
    ```
    cabal install hakyll
	hakyll-init my-site
	```
3. Read through the Hakyll tutorials to understand how it works.

On Windows, when trying to run ./site in Cygwin, I got the error [commitBuffer: invalid argument (invalid character)](https://jaspervdj.be/hakyll/tutorials/faq.html). I solved this by running in Windows Powershell.
```bash
chcp 65001
./site build
```

# Site design using Bootstrap.js

In Hakyll, the content of the site is kept separate from the site design. When you write posts, just write in Markdown and optionally specify some metadata like tags and date edited. In site.hs you'll specify how to "wrap" html, etc. around those posts to create the actual pages on the site. Thus, Hakyll is very modular - you can easily change the site layout independently of the content.

I chose a minimal theme (basically just to get the navigation bar). There's much more you can do with Bootstrap; see the page for info.

1. [Get bootstrap](http://getbootstrap.com/getting-started/).
2. Unzip the files into the blog directory. (I unzipped them directly into folders css/, js/, and fonts/, but you can also put them as subdirectories in bootstrap/ if you prefer.)
3. Modify the starter template, and saved it as default.html. The content of the pages will go into `$body$`. Note that I added a footer.
    ```html
    <!DOCTYPE html>
    <html>
 
 	<head>
 	<meta charset="utf-8">
 	<meta http-equiv="X-UA-Compatible" content="IE=edge">
 	<meta name="viewport" content="width=device-width, initial-scale=1">
 	<!-- The above 3 meta tags *must* come first in the head; any other head content must come *after* these tags -->
 
 	<meta name="description" content="Holden Lee's Blog">
 	<meta name="author" content="Holden Lee">
     
 	<title>$title$</title>
 
 	<link href="/css/bootstrap.min.css" rel="stylesheet">
 
 	<link href="/css/blog.css" rel="stylesheet">
 	<link href="/css/default.css" rel="stylesheet">
 	</head>
 
 	<body>
 
 	<!-- Navigation bar. navbar-inverse is black, navbar-default is white.-->
 	<!-- To make a button active (pressed), use <li class="active"> -->
 	<div id="header">
 	<nav class="navbar navbar-inverse navbar-fixed-top">
     <div class="container">
     <div class="navbar-header">
     <button type="button" class="navbar-toggle collapsed" data-toggle="collapse" data-target="#navbar" aria-expanded="false" aria-controls="navbar">
     <span class="sr-only">Toggle navigation</span>
     <span class="icon-bar"></span>
     <span class="icon-bar"></span>
     <span class="icon-bar"></span>
     </button>
     <a class="navbar-brand" href="/">Holden's Blog</a>
     </div>
     <div id="navbar" class="collapse navbar-collapse">
     <ul class="nav navbar-nav">
     <li><a href="/">Home</a></li>
     <li><a href="sitemap.html">Sitemap</a></li>
     <li><a href="about.html">About</a></li>
 	<!-- TODO: make this part a for loop over main pages -->
     </ul>
     </div>
     </div>
 	</nav>
 	</div>
 
 	$body$
 
 	<!-- Footer -->
 	<div id="footer">
 	<div class="container">
     Built with
     <a href="http://jaspervdj.be/hakyll">Hakyll</a> 
     using 
     <a href="http://www.getbootstrap.com">Bootstrap</a>, 
     <a href="http://www.mathjax.org">MathJax</a>, 
     <a href="http://www.disqus.com">Disqus</a>,
     <a href="http://highlightjs.org/">Highlight.js</a>, and 
     <a href="http://ignorethecode.net/blog/2010/04/20/footnotes/">Footnotes.js</a>
 	</div>
 	</div>
 	</body>
 
 	</html>
 
 	<!-- jQuery-->
 	<script src="https://ajax.googleapis.com/ajax/libs/jquery/1.11.0/jquery.min.js"></script>
 
 	<script src="http://code.jquery.com/jquery-1.10.1.min.js"></script>
 
 	<script src="/js/bootstrap.min.js"></script>
    ```

# Template html files

How do we turn a post written in Markdown into actual webpage? First, we read the metadata (title, subtitle, tags, date) from the Markdown file and display them in a uniform manner. Create the file templates/post.html:

```html
<div class="container">
  <div id="content">
    <div class="page header">
      <h1>$title$</h1>
    </div>
    <div class="info">
      $if(subtitle)$ <div class="subtitle"><p>$subtitle$</p></div> $endif$
      $if(date)$ 
        <p>Posted: $date$ 
          $if(modified)$, Modified: $modified$ $endif$
	</p>
      $endif$
      $if(tags)$ <p>Tags: $tags$</p> $endif$
    </div>
    
  </div>

  <div class="blog-main">
    $body$
  </div>
</div>
```
Every page will be (further) wrapped by default.html which we defined above. Finally, I created stylesheets css/blog.css and css/default.css to specify how I want the headings, subtitles, etc. to look. (Actually, I mostly stole them from other sites.)

I made a pattern to match the top level pages besides the main page. For convenience, I treat them the same way as posts are treated, except that they don't have a subtitle, date, tags, etc. I hardcode their URLs into the navbar (though if you want to, you can loop over them instead and hence not have to change the html if you add new top-level pages).

# Tags and Categories

[Javran's post](http://javran.github.io/posts/2014-03-01-add-tags-to-your-hakyll-blog.html) gives a tutorial on adding tags.

You can make categories the same way that you do tags. The difference is that each page is in only one category, determined by the subfolder that it is in.

I wanted hierarchical categories, which is tricky to do. My code for this is not elegant right now, but it works to give a [sitemap](sitemap.html).

# LaTeX and MathJax

There are two things we have to do: parse text in between dollar signs as LaTeX, and then render the LaTeX in the actual page using MathJax. I got the following material from [JD Reaver's post](http://jdreaver.com/posts/2014-06-22-math-programming-blog-hakyll.html) and [Travis Athoughies's post](http://travis.athougies.net/posts/2013-08-13-using-math-on-your-hakyll-blog.html).

1. Tell Pandoc to parse text between dollar signs as math:
    ```haskell
	pandocMathCompiler =
		let mathExtensions = [Ext_tex_math_dollars, Ext_tex_math_double_backslash,
			Ext_latex_macros]
			defaultExtensions = writerExtensions defaultHakyllWriterOptions
			newExtensions = foldr S.insert defaultExtensions mathExtensions
			writerOptions = defaultHakyllWriterOptions {
			                  writerReferenceLinks = True,
                              writerHtml5 = True,
                              writerHighlight = True,
                              writerExtensions = newExtensions,
                              writerHTMLMathMethod = MathJax ""
                            }
        in pandocCompilerWith defaultHakyllReaderOptions writerOptions
    ```
1. In order for a web page to render LaTeX, it must link to the MathJax script.
    ```html
	<script type="text/javascript"
        src="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"></script>
    ```
	Thus, to add MathJax functionality to all pages, add this add the end of default.html.
1. The [MathJax documentation](https://docs.mathjax.org/en/v2.5-latest/tex.html#defining-tex-macros) describes how to add packages and macros (so you can write `\R` for `\mathbb{R}`, for example). To do this, add a configuration file `MathJax/config/local/local.js`. The format is described in the link. Now modify the html code above to link to your configuration file. An inconvenience here is that you must give the full, not relative, URL.
    ```haskell
	<script type="text/javascript"
        src="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML,file:///C:/Users/holden-lee/Dropbox/website/test/MathJax/config/local/local"></script>
	```
1. TODO: script


# Code highlighting

Get [highlight.js](https://highlightjs.org/download/) for all languages you will use, and put it in the folder highlight/ in the blog directory. Include the new css/js files in the pattern matches, and add the following script to default.html, changing the style as you'd like.
```html
<link rel="stylesheet" href="/highlight/styles/tomorrow-night-bright.css">
<script src="/highlight/highlight.pack.js"></script>
<script>hljs.initHighlightingOnLoad();</script>
```

To highlight code in posts, put name/abbreviation of the language at the start of the code block like this:
```markdown
 ```haskell
    main = putStrLn "Hello World!"
 ```
```

# Disqus comments

# Github.io integration

# Social media buttons

# Additional features

There are many cool features I haven't covered. I would like to add some of these in the future.

* [Auto-generated table of contents for each post]() (cf. Wikipedia)
* [Shortcuts
* Analytics
