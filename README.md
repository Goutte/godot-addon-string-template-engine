String Template Engine Addon for Godot
--------------------------------------

[![MIT](https://img.shields.io/github/license/Goutte/godot-addon-string-template-engine.svg)](https://github.com/Goutte/godot-addon-string-template-engine)
[![Release](https://img.shields.io/github/release/Goutte/godot-addon-string-template-engine.svg)](https://github.com/Goutte/godot-addon-string-template-engine/releases)


A [Godot](https://godotengine.org/) `^4.5` addon that adds a `StringEngine` class.

The goal is to have something like Twig, Jinja2, etc.
Within the bounds of Godot and its goals, as it remains a Godot addon.

> _"A string template engine is basically a glorified sprintf"_

It is useful to generate:

- shader code
- dynamic roleplay
- html and stuff
- _tell us how **you** use it in the comments_


Disclaimer
----------

The feature set of this addon is slim, as the addon is young,
and none of the core devs have plans to push it towards its end goal.

We add what we need, when we can.  **Contributions are welcome.**

> I actually started this for the puzzle.

Features
--------

- [x] vanilla gdscript
- [x] tested
- [x] extensible
	- [x] add custom statements
- [x] variables
- [x] expressions
	- [x] integers and floats
	- [x] grouping with parentheses
	- [x] arithmetic (+ - * /)
	- [x] comparison (== != < <= > >=)
	- [x] combination (and or nand xor)
	- [ ] strings
	- [ ] filters
- [ ] statements
	- [x] print ({{ }})
	- [x] if else
	- [ ] while
	- [ ] for
	- [ ] set
	- [ ] extends
	- [ ] block
	- [x] verbatim
- [ ] whitespace handling
	- [ ] using delimiters ({{- -}} {{~ ~}})
	- [ ] using engine options
		- [ ] clear lines of whitespaces due to statements
- [ ] error handling
	- [ ] allow a "no assert()" mode
	- [ ] test error handling
	- [ ] show where in the template
- [ ] localization utils
- [ ] syntax highlighting


Usage Example
-------------

```gdscript
# TODO (meanwhile, see tests)
```


Install
-------

The installation is as usual, through the Assets Library.
You can also simply copy the `addons/` files of this project into yours, it should work.

Then, enable the plugin in `Scene > Project Settings > Plugins`.


Documentation
-------------

Please see the [addons' README](./addons/goutte.string-engine/README.md).


-----

> 🦊 _Feedback and contributions are welcome!_
