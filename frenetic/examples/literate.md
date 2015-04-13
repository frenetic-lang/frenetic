Guide to writing literate NetCore
=================================

This is a literate NetCore program. The NetCore executable parses any
file with the extension `.md` as literate NetCore. In literate
NetCore, lines that begin with *four* spaces are code, and all other
lines are ignored. For example, the following block is code:

    let l = learn in 
    monitorSwitch() + monitorPolicy(l)

Frankly, I think that four spaces is unnecessarily verbose. But, we're
just using the syntax of Github's markdown:

https://help.github.com/articles/github-flavored-markdown

This has the advantage of easy formatting on Github itself.