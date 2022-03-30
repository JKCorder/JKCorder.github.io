--- 
title: "Political Science and Applied Statistics using R"
author: "J.K. Corder"
site: bookdown::bookdown_site
documentclass: book
bibliography: [PSCI6920.bib, packages.bib]
# added quotes around .csl
# need to modify this .cls to add a space between references or hanging indent
csl: "american-political-science-association.csl"
# url: your book url like https://bookdown.org/yihui/bookdown
# cover-image: path to the social sharing image like images/cover.jpg
description: |
  This is the companion text for PSCI 6920, Political Analysis II, offered at Western Michigan University
link-citations: true
---

# About {-}

This text is a draft of the companion text for my graduate methods course, PSCI 6920, Political Analysis II, offered at Western Michigan University.  This text is open source and you may suggest edits and obtain the original RMarkdown files from github.

The idea of this text is to combine some insights from econometrics, recent work in political science, and programming tools in R.  The recent political science works spans American politics and comparative politics, with particular attention to work that introduces or builds on new approaches to statistical modeling.  The technical approach doesn't rely on matrix algebra or calculus, but the complexity in the modeling is about at the level of @greene2000.

A second motivation for constructing the chapters is to produce a series of *tidyverse*-inspired examples that translate applications in the political science literature into a contemporary programming context. Some of the chapters rely on data and programming from the Harvard Dataverse.  Some chapters rely on and update sample data and programming from methods and econometrics texts.  Other chapter rely on older data, but introduce new modeling or programming approaches to reproduce the estimates.  

This text is a series of chapters, each a self-contained R Notebook.  The Notebook format makes it easier to work through the chapters and replicate or run each chunk of code. Any file dependencies (other things you need to download) are identified within each code chunk). Be sure to download and run _common.R to load the required libraries and set a few options.


