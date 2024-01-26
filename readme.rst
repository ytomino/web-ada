very very simple CGI library for gcc-Ada (GNAT)
===============================================

Introduction
------------

Web
+++

It contains the simple subprograms for CGI, e.g. getting the environment
variables, parsing QUERY_STRING, the form data, and the cookies, writing the
HTTP headers, etc.

Web.HTML
++++++++

It contains the helpers for writing escaped text into HTML, etc.

Web.Producers
+++++++++++++

It's a page producer to write HTML from the template file.
Special tags in the template file will be notified to the callback procedure.

``<?TAG/>``
 You can replace it to any HTML written in callback.

``<?TAG>CONTENTS</?TAG>``
 You can use ``CONTENTS`` as a sub-template for recursive or ignore.
 The sub-template is just as or part of ``CONTENTS``.
 
 example::
 
  <?TAG>XXX<?SUBA>AAA</?SUBA><?SUBB>BBB</?SUBB>YYY</?TAG>
 
 It can select all (``XXX`` to ``YYY``), extract ``SUBA`` (``AAA``) or ``SUBB``
 (``BBB``), repeat these, or ignore all.

``<a ?TAG>``
 Attribute-style special tag.

``<a ?TAG ?? href="">``
 Any attributes after ``??`` will be removed.
 It's usable to preview with your web browser.
 
 example::
 
  <link rel="stylesheet" type="text/css" ?APPCSS ?? href="sample.css" />
 
 It uses ``sample.css`` when this template is directly opened in the browser,
 and it uses another style sheet (replacing ``APPCSS``) on output by CGI.

Web.RSS
+++++++

It's a very very simple RSS 2.0 writer.

Web.Lock_Files
++++++++++++++

It's a filesystem-based interprocess mutex.
There are the simple subprograms and the RAII-style object.
