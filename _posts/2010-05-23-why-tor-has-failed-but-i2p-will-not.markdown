--- 
layout: post
title: Why Tor Has Failed But I2P Will Not
tags: 
- Essays
---

Tor is a fantastic onion routing system that has introduced a lot of
ideas about strong anonymity and has fostered a good quantity of
academic papers. Originally a US Naval Research Laboratory project, it
is now a public open source project. In this essay I explore the
weaknesses of Tor and how newer projects improve upon this.

<h3>Tor provides a leaky abstraction</h3>

The original aim of the Tor software was to enable TCP traffic to be
transmitted anonymously, enabling already existing Internet
applications to simply use Tor as a proxy service. This has never
worked properly as the applications that users want have not been
designed with Tor in mind.

Early in Tor's life even simple protocols would not be fully
anonymised due to DNS requests not taking place over the Tor
connection. This would enable an eavesdropper to infer much about the
hosts being accessed. Today Tor users generally use Privoxy to prevent
DNS requests leaking, but there are still many application components
that do not obey the proxy settings.

A user attempting to browse anonymously needs to ensure that his/her
identity is not compromised through
JavaScript, <a href="http://hackademix.net/2007/09/26/cross-browser-proxy-unmasking/">Flash</a>,
persistent data in his browser (cookies, flash cookies or DOMStorage)
or simple <a href="http://panopticlick.eff.org/">browser
fingerprinting</a>. This is almost intractably difficult for the user
with current browsers, and a correctly configured browser would not
provide a satisfying browsing experience (without full JavaScript many
sites are simply unusable).

<h3>Tor is slow</h3>

The vast majority of users will never go to the trouble of setting up
an intermediate node or exit node. This is a combination of apathy in
the face of the additional hassle, and fear of legal
retaliation. However a node that takes part in Tor circuits will have
more data passing in and out, making traffic analysis more
difficult. Therefore users undermine the amount of anonymity they can
achieve by only running a client.

As a result, Tor client traffic seems to be growing faster than
network capacity is growing. This may ultimately be self-limiting once
speed drops to a point where new users lose interest due to the
latency being too high to browse. However some users are running
Bittorrent over Tor, which is much less latency sensitive but consumes
substantial bandwidth. Doing so is considered impolite but there is no
systematic way of preventing the network being overloaded by
peer-to-peer traffic.

[Bittorrent over Tor does not provide anonymity
anyway.](http://www-sop.inria.fr/members/Arnaud.Legout/Projects/bluebear.html)

Tor desperately needs a P2P-style tit-for-tat approach to bandwidth so
that all users act as relays. The
developers <a href="https://www.torproject.org/faq#EverybodyARelay">defend
their 'why not make everyone a relay' in their FAQ</a>, but being a
relay needs to be the default out of the box and relays should get
preferential treatment. This will enable the network to scale far more
effectively, so the P2P users are contributing to the network rather
than abusing it.

<h3>There are too few exit nodes, and many are up to something</h3>

Anyone can configure an exit node to only permit certain types of
traffic. Some exit
nodes <a href="https://web.archive.org/web/20130313084604/http://www.teamfurry.com/wordpress/2007/11/19/on-tor/">only
accept traffic on ports that correspond to unencrypted protocols</a>
or <a href="http://www.f-secure.com/weblog/archives/00001321.html">change
SSL certificates</a>. This is downright fishy, and an experienced Tor
user would blacklist these exit nodes. Nonetheless, this only catches
the most flagrant exit nodes.

The anonymity in the Tor network is dependent on the number of
nodes. Traffic analysis attacks are quite possible, depending on how
many nodes a single entity has under its control. As a result it is
crucial that users are strongly encouraged to run relays and exit
nodes to dilute the ability for a motivated organisation to control a
large portion of the network.

If an enthusiastic user does decide to run an exit node, he/she will
face problems as a result. Although the user is unlikely to face
problems from the authorities, an exit node will rapidly have its IP
address banned from a wide range of websites that enable user
discussion or user generated content. This is a substantial
inconvenience.

Exit nodes have in the past recorded the data passing through
them. Anything that isn't end-to-end encrypted can and will be
read. One researcher has
even <a href="http://www.wired.com/politics/security/news/2007/09/embassy_hacks?currentPage=1">published
the data he sniffed from his exit node</a>. His research demonstrated
that many Tor users don't understand the extent that Tor protects
them. Tor therefore forces users to make the bizarre choice between
non-anonymous Internet use with only their ISP logging traffic or
somewhat anonymous Internet use with a complete stranger logging their
traffic.

This further underlines the point that security is extremely difficult
to add retroactively. We need onion routing applications that both the
relay data on the anonymised network and run protocols designed
specifically for this environment, in one simple package.

<h2>Solutions</h2>

Lessons have been learnt from Tor, and newer projects solve much of
the problems Tor fix. I2P offers a new platform for writing anonymous
applications, and Anomos builds a Bittorrent style P2P network
intended to replace standard Bittorrent usage.

<h3>I2P: The <a href="http://www.i2p2.de/">Invisible Internet Project</a></h3>

I2P is a garlic routing network layer that developers must
specifically target. The user does not need to configure applications
to ensure anonymity, since all I2P applications have been written with
this layer in mind.

I2P does not offer access to the Internet at large (unless a dedicated
soul runs a gateway, which is extremely rare) but rather enables
developers to build protocols that offer strong anonymity. Already a
number of services exist, including eepsites (websites over I2P,
analogous to Tor's hidden services), I2PSnark (a port of Bittorrent)
and I2P-Messenger (anonymous instant messaging).

Unlike Tor, I2P nodes all act as relays. The network also favours long
lived nodes so a user may have to wait for a little while for his/her
system to become a well-used node before his/her traffic is sent at an
acceptable speed. This is highly reminiscent of the Bittorrent
philosophy. Using this approach seems to be paying dividends in terms
of scaling -- I2P throughput increased by a factor of 5 during
2009. This bodes well for the future.

Who is developing I2P? The developers all work under pseudonyms. This
may be for fear of legal repercussions, but it's a sign of faith in
the anonymity provided by the project.

<h3>Anomos: Anonymous P2P</h3>

While I2P offers a Bittorrent
service, <a href="http://anomos.info/">Anomos</a> goes a step further
and is building a onion routing network designed exclusively for
Bittorrent. In stark contrast to Tor, Anomos is just one application
that handles both the underlying anonymity network and the file
transfer protocol.

Anomos therefore scales, is resistant to user misconfiguration and
offers a familiar application interface -- it looks like a normal
Bittorrent client.

<h2>Conclusion</h2>

Neither I2P nor Anomos have yet received the scrutiny that Tor has,
and there are doubtless bugs and weaknesses yet to be
exposed. Nonetheless, their fundamental design offers a number
improvements over Tor, so I believe they have much potential.

These new projects are seeing slow but steady growth. I believe that
file sharing will drive adoption of these new anonymity
technologies. We are already seeing users attempting to use Bittorrent
with Tor today. It could only take a few large public fines (or a more
widespread adoption of the 'three strike then you're disconnected'
proposals) alongside a little more press coverage of these newer tools
to massively increase adoption. This would be a real game-changer for
media companies who would be unable to even threaten those pirating on
a large scale. The future looks fascinating.
