# Overview

Quickdist creates and updates Quicklisp distributions from a directory of local projects.  It maintains distinfo.txt, releases.txt, systems.txt and source archive - all that is needed for a distribution.  Currently it requires external utilities to work: /bin/tar, /usr/bin/md5sum, /usr/bin/sha1sum.

# API

A few dynamic variables and one function are exported.

Dynamic variables determine the layout of files on disk and on the web server.  Such are the defaults:

```lisp
(defparameter *distinfo-template*
  "name: {name}
version: {version}
distinfo-subscription-url: {base-url}/{name}.txt
release-index-url: {base-url}/{name}/{version}/releases.txt
system-index-url: {base-url}/{name}/{version}/systems.txt
")
(defparameter *distinfo-file-template* "{dists-dir}/{name}.txt")
(defparameter *dist-dir-template*      "{dists-dir}/{name}/{version}")
(defparameter *archive-dir-template*   "{dists-dir}/{name}/archive")
(defparameter *archive-url-template*   "{base-url}/{name}/archive")
```

The only exported function is `quickdist`.

```lisp
quickdist (&key name (version :today) base-url projects-dir dists-dir)
```

`name`, `version`, `base-url` and `dists-dir` provide values for the templates.  A special default version `:today` is resolved to the current date in `YYYYMMDD` format.  `projects-dir` is the directory each subdirectory of which is treated as a separate project to be included in the distribution.

# Example

Suppose you have some projects in `~/projects/`, you want to publish them from `~/dists/` and you name the distribution `quickdist`.  Then after loading quickdist and hunchentoot:

```lisp
cl-user> (quickdist:quickdist :name "quickdist" :base-url "http://localhost:4242/" :projects-dir "~/projects" :dists-dir "~/dists")
Processing {project1}...
Processing {project2}...
...
nil
cl-user> (push (hunchentoot:create-folder-dispatcher-and-handler "/" "~/dists/") hunchentoot:*dispatch-table*)
(#<CLOSURE (lambda # :in hunchentoot:create-prefix-dispatcher) {100A30DEAB}> hunchentoot:dispatch-easy-handlers)
cl-user> (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 4242))
#<hunchentoot:easy-acceptor (host *, port 4242)>
cl-user> (ql-dist:install-dist "http://localhost:4242/quickdist.txt")
127.0.0.1 - [2012-11-30 10:46:29] "get /quickdist.txt http/1.1" 200 241 "-" "quicklisp-client/2012112500 SBCL/1.1.0"
; Fetching #<url "http://localhost:4242/quickdist.txt">
; 0.24KB
==================================================
241 bytes in 0.00 seconds (0.00KB/sec)
Installing dist "quickdist" version "20121130".
Press Enter to continue.

127.0.0.1 - [2012-11-30 10:46:31] "get /quickdist/20121130/releases.txt http/1.1" 200 295 "-" "quicklisp-client/2012112500 SBCL/1.1.0"
; Fetching #<url "http://localhost:4242/quickdist/20121130/releases.txt">
; 0.29KB
==================================================
295 bytes in 0.00 seconds (0.00KB/sec)
127.0.0.1 - [2012-11-30 10:46:31] "get /quickdist/20121130/systems.txt http/1.1" 200 132 "-" "quicklisp-client/2012112500 SBCL/1.1.0"
; Fetching #<url "http://localhost:4242/quickdist/20121130/systems.txt">
; 0.13KB
==================================================
132 bytes in 0.00 seconds (128.91KB/sec)
#<ql-dist:dist quickdist 20121130>
cl-user> (ql:quickload :symbol-namestring) ; for example
To load "symbol-namestring":
  Load 1 ASDF system:
    named-readtables
  Install 1 Quicklisp release:
    symbol-namestring
127.0.0.1 - [2012-11-30 10:47:23] "get /quickdist/archive/symbol-namestring-20120812.tgz http/1.1" 200 14842 "-" "quicklisp-client/2012112500 SBCL/1.1.0"
; Fetching #<url "http://localhost:4242/quickdist/archive/symbol-namestring-20120812.tgz">
; 14.49KB
==================================================
14,842 bytes in 0.00 seconds (14494.14KB/sec)
; Loading "symbol-namestring"
[package symbol-namestring]
(:symbol-namestring)
cl-user> (ql-dist:uninstall (ql-dist:find-dist "quickdist")) ; no longer want this dist
t
```
