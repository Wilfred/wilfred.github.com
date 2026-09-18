1

Are you familiar with the idea of auto research by Karpathy? Do you
have any relevant skills?

2

Okay let's apply that strategy. The goal, the metric I want to be reducing execution time as measured in instructions executed as reported by perf.  Explore lots of ideas for making difftastic faster.
- Look at relevant papers
- See the wiki on the GitHub repository
- Look at other claude code sessions I have
- Look at open issues or closed issues
- Git commit history and just explore ideas you think seem relevant based on literature or other discussions
 
Writen to a log Markdown file as you work. Recording all the ideas
that you've tried so far and the outcome of them. Keep going, try to
explore as many issues as you can until I've exhausted my Claude
Code allowance for the current time period.

Consider both micro-optimisations, macro-optimisations, and
algorithm changes. Success is anything that reduces the instructions
executed without changing the output on input files.

3

Ensure that your log file is periodically committed and pushed to the
branch so I can view progress through the GitHub web UI.

4

Write a plan.md file with your approach, sufficient for me to resume
this process in another session. Commit and push. Next, summarise
all the Claude code sessions that are relevant, so resuming sessions
don't need to query old sessions again. Commit that as a separate
file and note in the plan

5 (Codex session)

Review plan.md. Perform autoresearch, focusing on large inputs and
ways of improving performance there. Keep updating the log file as
you research.

6

OK, let's do a second investigation, but purely focused on performance
of typing_1.ml and slow_1.rs (plus their paired files). Continue
adding the existing logs as you work.
