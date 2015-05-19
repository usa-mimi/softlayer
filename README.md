# softlayer.el
Emacs lisp for SoftLayer

# Installation

`softlayer.el` and `request.el` is in a directory on your load-path, and add the following to your ~/.emacs or ~/.emacs.d/init.el:

```
(setq sl-user "SLUSER")
(setq sl-api-key "API-KEY")

(require 'softlayer)
```

# Usage

Enable it by running M-x softlayer

# Examples

|command|description|
|:--|:--|
|vl|List display of the virtual machine|
|\C-m |Detailed view of the virtual machine|




