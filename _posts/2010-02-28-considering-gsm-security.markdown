--- 
layout: post
title: Considering GSM Security
tags: 
- Essays
---

How secure is your phone? In this essay I consider the current state of GSM security, the strength of its cryptography, the privacy losses and touch on a few notable past security failures.

Do we even want strong cryptography for telephony? It may seem like "yes" is the obvious answer to this question, but in practice things are not so simple. The police and security services want access and are entitled to it by law. This makes designing a secure communications platform very difficult.

<h3>Walking in through the back</h3>
In light of this, any telephone system needs to give access to appropriate third parties. This has gone catastrophically wrong in the past. Backdoor functionality is rarely publicised<a href="#foot1">[1]</a> and so frequently does not get proper scrutiny.

In 2005 the Greek Prime Minister, the Mayor of Athens and 100 other officials learnt that their phones had been bugged. Vodafone Greece was compromised and the <a href="http://spectrum.ieee.org/telecom/security/the-athens-affair">perpetrators used the wiretapping facilities of Vodafone's systems</a>. The malicious software implanted was only discovered when it generated errors and Ericsson was asked to fix it. Ericsson's AXE switch had a lawful intercept feature, and an ingenious rootkit was installed that exploited this mechanism.

<h3>Crypto strength</h3>
Embedded devices are more limited in power, but with over 4 billion phones out there, engineers can get huge economies of scale for dedicated cryptography chips. This still leaves the question of how well designed the algorithm used is and how large the keys used are.

The algorithms used in GSM to encrypt the over the air traffic are A5/1, A5/2 and A5/3. It's worth noting that this only covers the traffic between the handset and the base station, and the data is often not encrypted beyond that. Furthermore, although A5/3 is the strongest encryption algorithm of these, any A5/3-capable handset will downgrade if the base station does not offer 3G coverage.

A major problem for cryptanalysis of GSM is the lack of documentation. A5/1 had to be reverse-engineered by cryptanalysts before they could examine it. Today there are projects such as <a href="http://openbsc.gnumonks.org/">OpenBSC</a> which implement GSM stacks to enable research.

I only talk briefly about the algorithms here, for further reading look at <a href="http://cryptome.org/gsm-a5-files.htm">Cryptome's GSM files</a>.

<h3>A5/1</h3>
A5/1 is the compulsory GSM cryptography algorithm. The standards body chose a new algorithm rather than tried and tested algorithms. Ross Anderson discusses its weakness and political motivations in <a href="http://groups.google.com/groups?selm=2ts9a0%2495r%40lyra.csx.cam.ac.uk">more depth on the comp.risks newsgroup</a>.

A5/1 is broken. To briefly summarise its cryptographic weaknesses, it has an effective key length of 40 bytes, which is simply too short. Specialist hardware <a href="http://www.gcomtech.com/index.php?app=ccp0&amp;ns=prodshow&amp;ref=gsm-intercept">can be purchased</a> (as do community projects <a href="http://securitytube.net/Airprobe,-Monitoring-GSM-traffic-with-USRP-%28HAR-2009%29-video.aspx">[1]</a> <a href="http://www.engadget.com/2010/05/10/meganets-dominator-i-snoops-on-four-gsm-convos-at-once-fits-in/">[2]</a>) which will break the encryption in real time. <a href="https://web.archive.org/web/20120326100525/http://www.reflextor.com/trac/a51">A project to create rainbow tables</a> (research paper <a href="&lt;a href=">here</a>) has finished, making decryption much faster on a general purpose workstation.

<h3>A5/2</h3>
A5/2 is <a href="http://cryptodox.com/A5/2">simpler and weaker than A5/1</a>, and <a href="http://events.ccc.de/congress/2009/Fahrplan/attachments/1519_26C3.Karsten.Nohl.GSM.pdf">some attacks on A5/1 have been proposed</a> which force phones to downgrade to A5/2. A5/2 is no longer supported by handsets released since 2006 (but <a href="http://laforge.gnumonks.org/weblog/2010/11/12/#20101112-history_of_a52_withdrawal">took quite some time to get removed</a>).

<h3>A5/3</h3>
A5/3, which uses the Kasumi cipher, is a substantial improvement. Kasumi is public and based on well established cipher called MISTY1. Sadly, A5/3 modifies MISTY1 and in doing so weakens it. To quote Bruce Schneier 'attacks only get better over time'. A related key attack was found at the end of 2009 (<a href="http://eprint.iacr.org/2010/013.pdf">research paper here</a>), which uses four related keys to derive the full 128-bit key in only 2^32 time. At this point in time this is insufficient to mount a practical attack, provided that keys have been generated using good quality sources of entropy so related keys cannot be obtained. This is a major blow to A5/3 and places doubts on its long-term security. Sadly it is unlikely that the standards will change until we reach 4G mobile phones.

<h3>Alternatives available</h3>
What voice call options are available to the security conscious user? VoIP offers us an opportunity to select the encryption protocol ourselves, and avoid dubious untested protocols.  The most popular option is <a href="http://en.wikipedia.org/wiki/ZRTP">ZRTP</a>, a key exchange and encryption protocol intended for voice calls. This has the major advantages that it is a public, IETF Internet Draft that anyone can read, and is built on widely known protocols such as SRTP and AES.

As for Skype, reports vary. Skype is also undocumented (although as a business move this prevents 3rd party Skype clients). Skype has been claimed to be <a href="http://www.techradar.com/news/internet/skype-hits-back-at-eu-voip-monitoring-agency-537860">backdoor-free</a>, although a number of public sector organisations have claimed they can access it, including the <a href="http://www.h-online.com/security/news/item/Speculation-over-back-door-in-Skype-736607.html">Austrian police</a>. The CSO at Skype has <a href="http://www.zdnet.de/mobiles_internet_drahtloses_arbeiten_telefonieren_uebers_internet_wie_sicher_ist_skype_wirklich_story-39001620-39151472-1.htm">refused to confirm or deny</a> (German article) whether they can listen in.

<h3>Spoofing</h3>
There's also the issue of spoofing. If a mobile phone can successfully convince the receiver that it is a different phone, it can access voicemail without encountering security measures. There are products such as <a href="http://blog.brickhousesecurity.com/spoofcard/">Spoofcard</a> that allow you to impersonate any caller ID you wish.

Ideally an attacker also wishes to spoof a number to the base station such that he can receive calls or texts intended for someone else. Whilst details are scarce, it seems that <a href="http://www.pcworld.com/businesscenter/article/163515/nokia_we_dont_know_why_criminals_want_our_old_phones.html">this has been achieved by some users</a> using certain older Nokia handsets.

<h3>Bugging</h3>
Finally, a mobile phone has a wealth of information about you. Any GSM model will register with its local base station when switched on, so phone records later will show which cell you are in. In rural areas a cell can have a radius of up to 35km, but in cities it can be as small as 100m. The CIA <a href="http://www.wired.com/politics/security/magazine/15-07/st_cia">were caught out</a> by this in 2007. Today, many phones have even more accurate positioning using GPS or Skyhook technologies.

Some phones will actually act as <a href="http://www.schneier.com/blog/archives/2006/12/remotely_eavesd_1.html&lt;br &gt;&lt;/a&gt;">bugs on demand</a>. This 'roving bug' technique allows law enforcement to remotely install sound recording software on a handset. Again details are scarce, but I would imagine rooted Linux based phones would be immune to this.

<a name="foot1">&nbsp;</a>

(1) An exception to this is Cisco. To their credit, they <a href="http://www.cisco.com/en/US/docs/ios/12_2sb/feature/guide/ht_ssi.html">document their lawful intercept system</a>. This enables researches to <a href="http://www.blackhat.com/html/bh-dc-10/bh-dc-10-archives.html#Cross">examine the methodology</a>.
