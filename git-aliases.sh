#!/usr/bin/env sh
git config --global alias.a add
git config --global alias.b branch
git config --global alias.c checkout
git config --global alias.d diff
git config --global alias.dl '!f() { git diff "HEAD~${1:-1}" HEAD; }; f'
git config --global alias.dt difftool
git config --global alias.dtl '!f() { git difftool "HEAD~${1:-1}" HEAD; }; f'
git config --global alias.i init
git config --global alias.l log
git config --global alias.m merge
git config --global alias.n clone
git config --global alias.o commit
git config --global alias.ps push
git config --global alias.pl pull
git config --global alias.s status
git config --global alias.cp cherry-pick

