--- 
layout: post
title: "Hubot for Smartphone Messaging"
---

[Hubot](https://hubot.github.com/) is a fantastic chat bot for
automating parts of your life. It's widely used by developer teams,
but I wanted to run an instance when exchanging messages with my
friends and family. I tested several chat apps, and eventually got Hubot
working with Telegram. Here's what I've learnt about running Hubot with
smartphone messaging.

## WhatsApp

In my social circles, [WhatsApp](https://www.whatsapp.com/) is the
clear winner by usage. To my disappointment, there's no official API
available for WhatsApp.

In spite of this, there's [OpenWhatsApp](http://openwhatsapp.org/) and
[WhatsAPI](https://github.com/venomous0x/WhatsAPI). WhatsApp is
basically a modified XMPP protocol, and intrepid open source
developers have reverse-engineered it.

However, WhatsApp has a history of raising legal objections
([2012](http://www.h-online.com/security/news/item/WhatsApp-threatens-legal-action-against-API-developers-1716912.html),
[2014](https://github.com/github/dmca/blob/master/2014-02-12-WhatsApp.md))
to third-party implementations. Whilst I believe this is entirely
legal in the UK, I wasn't interested in writing code that could result
in legal threats. To make matters worse, if WhatsApp don't support an
official API, they could in principle change it at any moment.

## Kik

I started by looking at popular alternatives to WhatsApp, so I decided
to try [Kik](http://kik.com/). Kik provides a great client on both
Android and iOS with all the group chat features we've come to expect
from WhatsApp.

Kik has usernames, rather than finding people through phone numbers,
but account creation is very lightweight.

Kik [does provide an API](http://dev.kik.com/). However, it's entirely
targeted at developers building webapps within their client. Whilst
[there exist spam bots on the Kik platform](http://www.forbes.com/sites/parmyolson/2014/08/20/kik-porn-bot-spammers/)
there doesn't seem to be an API for building your own chat bots. This
is ironic when all new Kik users get the official Kik bot added as a
default contact.

## Jabber / XMPP

After two failures with proprietary services, I tried setting up Jabber
instead. This is can be labour intensive.

I installed the excellent [ejabberd](https://www.ejabberd.im/) on a
personal server. Ubuntu's packages work out of the box, but they're
v2.1.10, which dates from late 2011.

Jabber is an extremely extensible protocol: most of the features you
will want are optional extras. As a result, you need to ensure your
server and clients all support the
[XMPP extensions](http://xmpp.org/xmpp-protocols/xmpp-extensions/) you
plan to use. This can be tricky: it's not always clear which
extensions different clients and servers support. I was also caught
out by some extensions which require no support from the server at
all, just client support.

Finding good smartphone Jabber clients was harder than setting up a
server, especially when you require group chat (called 'conferences'
in XMPP). For Android,
[Conversations](https://github.com/siacs/Conversations) is excellent.
For iOS, I never found a good client that supported group chat and
push
notifications.
[It turns out this is almost impossible to implement on iOS](https://chatsecure.org/blog/fixing-the-xmpp-push-problem/).

Jabber is a chat protocol designed for computers, not
smartphones. Jabber has poor support for contact discovery (WhatsApp
just uses phone numbers) and fairly long usernames. Its support for
sharing images is weak, requiring both clients to be online at the
same time. The general Jabber design seems to be centred around users
logging in, chatting, then logging out. A smartphone-centric design is
different: I expect to be always connected, receive push
notifications, and for my client to sync when I've been out of
service.

If I were looking for a chat protocol for a team sitting in front of
laptops, I would seriously look at Jabber. As a WhatsApp alternative,
I don't think it's suitable today.

## Telegram

Telegram is another popular WhatsApp alternative, but includes a
[documented, open API](https://core.telegram.org/api). There are
established open source implementations, and even the official clients
are open source!

The Telegram clients on Android and iOS are both excellent, providing
almost exactly the same functionality of WhatsApp.

Setting up a Hubot instance is a little time-consuming, but works very
well. There's a
[Hubot adapter for Telegram](https://github.com/zenitraM/hubot-tg/)
and I've written
[a step-by-step setup guide](https://github.com/zenitraM/hubot-tg/pull/4). You
will need a fresh telephone number for your bot
([Twilio](http://twilio.com/) is ideal for this), the latest version
of [tg](https://github.com/vysheng/tg) and then you can configure your
Hubot as normal.

## Final Thoughts

Smartphone messaging has yet to consolidate:
[there are many competing protocols](http://blogs.wsj.com/digits/2014/02/20/why-facebook-bought-whatsapp-in-a-single-chart/)
with no clear winners yet. Protocols tend to compete on availability
of clients (Android/iOS/Windows Phone/Blackberry/web) and featureset,
so API availability is not a differentiating factor. However, hackable
protocols will grow a larger ecosystem, which is a great help when
developing a new chat app.

So, I'd recommend Telegram today. If you're using chat bots on
your smartphone, I'd love to hear from you. 

Once you're up and running, the whole Hubot ecosystem is available to
you. There are
[~800 Hubot packages on npm](https://www.npmjs.com/search?q=hubot) and
[~450 scripts included in hubot-scripts](https://github.com/github/hubot-scripts/tree/master/src/scripts). Hubot
is easy to extend if there's something missing. Give it a try!
