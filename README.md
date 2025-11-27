![Icon](icon.svg)

String Template Engine Addon for Godot
--------------------------------------

[![MIT](https://img.shields.io/github/license/Goutte/godot-addon-string-template-engine.svg)](https://github.com/Goutte/godot-addon-string-template-engine)
[![Release](https://img.shields.io/github/release/Goutte/godot-addon-string-template-engine.svg)](https://github.com/Goutte/godot-addon-string-template-engine/releases)

A [Godot](https://godotengine.org/) `^4.5` addon that adds a `StringEngine` class.

The goal is to have something like [Twig], Jinja2, etc.
Within the bounds of Godot and its goals, as it remains a Godot addon.

[Twig]: https://twig.symfony.com/

> _"A string template engine is basically a glorified sprintf"_

It is useful to generate:

- shader code
- dynamic roleplay
- html and stuff
- _tell us how **you** use it in the comments_


Disclaimer
----------

The feature set of this addon is slim, as the addon is young,
and none-one so far has plans to push it towards its end goal.

We add what we need, when we can.  **Contributions are welcome.**

Features
--------

- [x] vanilla gdscript
- [x] tested
- [x] extensible
	- [x] customize some symbols
	- [x] add custom statements
	- [x] add custom filters
- [x] variables
- [x] comments `{# … #}`
- [x] expressions
	- [x] integers and floats
	- [x] grouping with parentheses
	- [x] arithmetic `+ - * /`
	- [x] comparison `== != < <= > >=`
	- [x] combination `and or nand xor`
	- [x] strings `"Bonjour!"`
	- [ ] concatenation `~`
	- [x] arrays
	- [ ] arrays' indices  `foo[4]`
	- [x] objects
	- [ ] objects' properties
	- [ ] objects' methods
	- [x] filters `|`
- [ ] statements
	- [x] print `{{ … }}`
	- [x] if else `{% if … %} … {% else %} … {% endif %}`
	- [x] while `{% while … %} … {% endwhile %}`
	- [ ] for
	- [x] set
	- [ ] extends
	- [ ] block
	- [x] verbatim (STILL BUGGY)
- [ ] filters (see [Twig Filters](https://twig.symfony.com/doc/3.x/filters/index.html))
	- [x] abs
	- [ ] batch
	- [ ] capitalize
	- [ ] column
	- [ ] convert_encoding
	- [ ] country_name
	- [ ] currency_name
	- [ ] currency_symbol
	- [ ] data_uri
	- [ ] date
	- [ ] date_modify
	- [ ] default
	- [ ] escape
	- [ ] filter
	- [ ] find
	- [ ] first
	- [ ] format
	- [ ] format_currency
	- [ ] format_date
	- [ ] format_datetime
	- [ ] format_number
	- [ ] format_time
	- [ ] join
	- [ ] json_encode
	- [ ] keys
	- [ ] language_name
	- [ ] last
	- [ ] length
	- [ ] locale_name
	- [x] lower / lowercase
	- [ ] map
	- [ ] merge
	- [ ] nl2br
	- [ ] number_format
	- [ ] plural
	- [ ] raw
	- [ ] reduce
	- [ ] replace
	- [ ] reverse
	- [ ] round
	- [ ] shuffle
	- [ ] singular
	- [ ] slice
	- [ ] slug
	- [ ] sort
	- [ ] spaceless
	- [ ] split
	- [ ] striptags
	- [ ] timezone_name
	- [ ] title
	- [ ] trim
	- [ ] u
	- [x] upper / uppercase
	- [ ] url_encode
- [ ] whitespace handling
	- [ ] using delimiters `{{- -}} {{~ ~}}`
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
