# Documentation

> Missing something ?  _Contribute it !_


## Templates

As a template designer, there are some rules you need to be aware of.
Here's a list of the features available to you.


### Comments

Let's start with the cluttering chatter we all love and hate: comments.

```twig
{# Any text in here will be absent from the rendered template #}
```


### Printing

Printing expressions is done by wrapping them inbetween the `{{` and `}}` delimiters, like so:

```twig
{{ … }}
```

Let's say you provided the variable `name` to your template for render, with the value `"World"`.

```twig
Hello {{ name }}!
```
Rendering this template will yield:

```twig
Hello World!
```

You can print any expression, not just variables:

```twig
{{ 666 + 666 * 999 + 999 }}

{# outputs 666999 #}
```


### Control the Flow


#### If/Else

Using if/else statements you can decide to show some part or another of the template:

```twig
Your name is {{ person.name }}.

{% if current_year - person.birthday.year >= 16 %}
You are allowed to vote, citizen.
{% else %}
You are not allowed to vote yet, hatchling.
{% endif %}
```


### Filters


#### abs

The `abs` filter returns the absolute value.

```twig
{# number = -7 #}

{{ number|abs }}

{# outputs 7 #}
```


#### lowercase

The `lowercase` (`lower` for short) lowercases a string.

```twig
{{ "MON ŒUF A CRIÉ."|lower }}
{# outputs mon œuf a crié. #}
```

> Internally, we use `String.to_lower()`.


#### round

The `round` filter rounds a number to a given precision:

```twig
{{ 2.55|round }}
{# outputs 3 #}

{{ 2.55|round(1, 'floor') }}
{# outputs 2.5 #}
```

The round filter takes two optional arguments;
the first one specifies the precision _(default is `0`)_
and the second the rounding method _(default is `common`)_:

- `common` rounds either up or down (rounds the value up to precision decimal places away from zero, when it is half way there -- making 1.5 into 2 and -1.5 into -2);
- `ceil` always rounds up;
- `floor` always rounds down.


#### uppercase

The `uppercase` (`upper` for short) uppercases a string.

```twig
{{ "Mon œuf a crié."|upper }}
{# outputs MON ŒUF A CRIÉ. #}
```

> Internally, we use `String.to_upper()`.


### Operator Precedence

We're tring to conform to the [operator precedence of Twig 4](https://twig.symfony.com/doc/3.x/templates.html#operators).
