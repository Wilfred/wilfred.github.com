---
layout: page
title: Archives
permalink: /archives/
tags: header
---

<table>
<tbody>
{% for post in site.posts %}
<tr>
<td><a href="{{ post.url }}">{{ post.title }}</a></td>
<td>{{ post.date | date_to_string }}</td>
</tr>
{% endfor %}
</tbody>
</table>
