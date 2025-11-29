# Documentation

## Templates

### Comments

Let's start with the cluttering chatter we all love and hate: comments.

```twig
{# Any text in here will be absent from the rendered template #}
```

### Printing

Printing expressions is done by wrapping them in the `{{` and `}}` symbols, like so:

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
