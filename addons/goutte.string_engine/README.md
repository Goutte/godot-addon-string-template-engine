# Documentation

## Templates

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
{{ 666 * 999 + 666 + 999 }}
```

### Control the Flow

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
