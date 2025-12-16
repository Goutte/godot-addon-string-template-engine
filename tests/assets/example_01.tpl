Hello {{ name ~ "-san" }} !

{# The Devil's Equation can be read upside down. #}
{{ 666 + 666 * 999 + 999 == 666999 }}

{{
	(
		-666
		*
		+999
	)
	|
	abs
}}

{#
An amazing comment. 🤩
With a second line !
#}

{{
	"
	A multiline string
	Should be yellow
	As a sun of spring
	Or rotten marrow
	"
}}

{% set answer = -42|abs %}

{% if true and answer > 10 %}
I am the truth.
{% else %}
I am the trump.
{% endif %}
