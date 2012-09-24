<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
	<head>
	  <script src="http://ajax.googleapis.com/ajax/libs/jquery/1/jquery.js"></script>
	  <script src="/ggbio/galleria/galleria-1.2.6.min.js"></script>
		<meta http-equiv="Content-type" content="text/html; charset=utf-8" />
		<meta name="description" content="Andrew Heiss is a student and web designer and developer from Utah who is working on his MPA in international nonprofit management at BYU." />
		<meta name="author" content="Tengfei Yin" />
		<title>Support | ggbio</title>
		<link rel="icon" type="image/vnd.microsoft.icon" href="/ggbio/favicon.ico" />
		<link rel="icon" type="image/png" href="/ggbio/favicon.png" />
		<link rel="apple-touch-icon" href="/ggbio/css/images/apple-touch-icon.png" />
		<!-- <link rel="alternate" type="application/atom+xml" title="Blog Posts" href="http://feeds.andrewheiss.com/AndrewHeiss" /> -->
		<!-- <link rel="openid2.provider" href="https://www.google.com/accounts/o8/ud?source=profiles" /> -->
		<!-- <link rel="openid2.local_id" href="http://www.google.com/profiles/andrewheiss" /> -->
		<link rel="stylesheet" href="/ggbio/css/all.css" type="text/css" media="screen" />
		<link rel="stylesheet" href="/ggbio/css/print.css" type="text/css" media="print" />
		<!--[if lte IE 6]>
		<link href="/css/ie6fix.css" rel="stylesheet" type="text/css" media="screen" />
		<![endif]-->
		<link href="/ggbio/favicon.ico" rel="icon"
		type="image/x-icon" />
<script type="text/javascript">
  var _gaq = _gaq || [];
  _gaq.push(['_setAccount', 'UA-29985442-1']);
  _gaq.push(['_trackPageview']);

  (function() {
    var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
    ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
    var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
  })();
</script>
	</head>
	<body>
		<div id="header">
			<div class="row">
				<div class="column grid_9">
					<h1 class="title"><a href="/">ggbio</a></h1>
					<h2 class="minibio">An extended grammar
					of graphics for sequence data</h2>
				</div>
			</div>

			<div id="navbar-wrapper">
				<div class="row">
  <div class="column grid_9">
    <ul id="navbar">
      <li><a href="/ggbio/">Home</a></li>
      <!-- <li><a href="/ggbio/blog/">Blog</a></li> -->
      <li><a href="/ggbio/tutorial">Tutorial</a></li>
      <li><a href="/ggbio/support">Support</a></li>
      <li><a href="/ggbio/archive">Case Study</a></li>
      <li><a href="/ggbio/about">About</a></li>
      <!-- <li><a href="/projects/">Projects</a></li> -->
      <!-- <li><a href="/ggbio/portfolio/">Portfolio</a></li> -->
    </ul>
  </div>
</div>

			</div>
		</div>
		<div id="wrapper">
			<ul>
<li><a href="#install">Installation</a></li>
<li><a href="#FAQ">FAQ</a></li>
<li><a href="#bug">Bug report</a></li>
</ul>

<h2>Installation <a id = "install"></a></h2>

<h4>For R 2.15 with git version <em>ggbio</em>(latest)</h4>

<p><em>This is for developers, notice I am not using bioc-devel svn as major version control system. If you want to play with latest</em> <strong>ggbio</strong> <em>you have to stick with this method.</em></p>

<p>Install of developmental version of <em>ggbio</em> require developmental R now. 
Simply open <em>R</em> session and run the following code:</p>

<pre><code>source(&quot;http://www.tengfei.name/projects/ggbio/utils/installer.R&quot;)
</code></pre>

<p>This will install ggplot2/ggbio github version, and bioc-dev version 
denpendencies.</p>

<p>For all users, make sure to install R package <em>RCurl</em> to use this command</p>

<p>For Windows users: if you came across buiding errors, please 
install <a href="http://cran.r-project.org/bin/windows/Rtools/">Rtools</a>, then run the
command line again.</p>

<h4>For R 2.15 with bioc-dev version <em>ggbio</em></h4>

<pre><code>source(&quot;http://bioconductor.org/biocLite.R&quot;)
biocLite(&quot;ggbio&quot;)
</code></pre>

<p><strong>This is currently now working due to unsycronized ggbio in bioc-dev</strong></p>

<h2>FAQ<a id = "FAQ"></a></h2>

<ul>
<li>Test question</li>
</ul>
<blockquote>
<p>a
b</p>
</blockquote>
<ul>
<li>test 2</li>
</ul>
<blockquote>
<p>3
4</p>
</blockquote>
<h2>Bug report<a id = "bug"></a></h2>

<p>For any issues please contact yintengfei at gmail.com or file a issue in
<strong>ggbio</strong> <a href="https://github.com/tengfei/ggbio/issues">github issue page</a>.</p>

		</div>
		<div id="footer-wrapper">
  <div id="footer-inner-wrapper">
    <div id="footer" class="row">
      <div class="column grid_2 online">
	<!-- <h3>Find me online:</h3> -->
      </div>
      <div class="column grid_4 online">
	<ul>
	  <li><a href="http://www.tengfei.name"
		 title="Tengfei Yin" rel="me">Tengfei Yin</a></li>
	</ul>
      </div>
      <div class="column grid_3 copyright">
	<p>&copy; 2012&ndash;now Tengfei Yin</p>
	<p>Powered by <a href="http://github.com/mojombo/jekyll" title="mojombo's jekyll at master - GitHub">Jekyll</a> &amp; <a href="https://www.nearlyfreespeech.net/">NFSN</a></p>
      </div>
      <!-- <div id="print-message" class="column grid_6"> -->
      <!-- 	<p>You've printed out "Support." <br />Please visit http://www.andrewheiss.com for more great posts and articles like this.</p> -->
      <!-- </div> -->
    </div>
  </div>
</div>

		<script src="/ggbio/script/jquery.min.js" type="text/javascript" charset="utf-8"></script>
		<!-- <script src="/script/lazyload.min.js" type="text/javascript" charset="utf-8"></script> -->
		<script src="/ggbio/script/captify.tiny.js" type="text/javascript" charset="utf-8"></script>
		<script type="text/javascript" charset="utf-8">
			$(function() {
				$('img.captify').captify({});
				      
			    // $("img").lazyload({
			    //    placeholder : "/ggbio/css/images/white.gif",
			    //    effect      : "fadeIn"
			    // });
			});
		</script>
		
		
		<script type="text/javascript">

  var _gaq = _gaq || [];
  _gaq.push(['_setAccount', 'UA-527449-5']);
  _gaq.push(['_trackPageview']);

  (function() {
    var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
    ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
    (document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0]).appendChild(ga);
  })();

</script>
	</body>
</html>
