# isidorus
# Introduction

Isidorus is an Open Source Topic Map engine developed using sbcl and elephant. Isidorus supports import and export of XTM 1.0 and 2.0, full versioning, merge semantics, an Atom-based RESTful API and Topic Map querying — with more to come.

Isidorus supports amongst others:
- Enforcements of constraints (TMCL)
- Json-import / export and a AJAX front end for data curation
- Enhanced querying

Isidorus is licensed under L-LGPL (Lisp-LGPL).

# Download
The main site hosting Isidorus remains https://common-lisp.net/project/isidorus/

The command for an initial anonymous checkout of the HEAD revision of Isidorus' trunk is:

svn checkout svn://common-lisp.net/project/isidorus/svn/trunk/ isidorus

This here is a copy of the source which you can get with:

git clone https://github.com/mwkuster/isidorus.git


# Installation

Isidorus has quite a few dependencies, most of which can be easily installed via asdf. The file docs/install_isidorus.txt in the source tree gives detailed instructions where to get and how to install all the required libraries. Isidorus comes with an isidorus.asd file, too, so once all the prerequisites are in place, loading Isidorus is as simple as:

```
(asdf:operate 'asdf:load-op 'isidorus)
```



