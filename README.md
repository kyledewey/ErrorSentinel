# ErrorSentinel

## Background and Motivation
ErrorSentinel is a tool that allows for programmable spreadsheet autocorrect.
It was created upon observing that people often use spreadsheets to store
often sizable amounts of data in the biological domain.  Given that
traditional spreadsheet programs (e.g., Excel) are not designed for this
purpose, this leads to all sorts of issues with the data's quality.
Specifically, I identified these issues:

- Spreadsheet programs usually optimistically assume that input data is correct.
  This makes it easy to accidentally introduce typos.  This also makes it easy
  to input data that is human-readable but not machine-readable (e.g., "around 4"
  versus "4"), requiring downstream cleanup for automated analysis.
- Spreadsheet programs will occasionally assume that data is of one type
  when it is, in fact, of another.  This can lead to
  [incorrect, silent data manipulation by the program](http://www.ncbi.nlm.nih.gov/pubmed/15214961).

A solution to this problem is a programmable autocorrect.  This way, users
can specify what "correct" means, and any data that doesn't match the user's
specification is rejected.  Additionally, for post-hoc curation, we can allow
for curators to define custom autocorrect rules, so that changes can be suggested
or even automatically applied as per the user's desires.  ErrorSentinel is an
implementation of this general idea.

## Building and Running ErrorSentinel

As long as you have an Internet connection and the [`sbt`](http://www.scala-sbt.org/)
tool installed, you should need only do:

```console
sbt compile
```

From there, ErrorSentinel can be run like so:

```console
sbt "run-main sentinel.utils.interactive.ErrorSentinel xml/myProject.xml"
```

...where `xml/myProject.xml` is a project file.  Project files contain the
following information:

- What CSV sheets are being used
- What language definitions (e.g., user-defined code) are being used
- The associations between user-defined code and sheets.  That is,
  which code triggers on which portions of a sheet.  Usually, for each column,
  there is a bit of code defining what is valid for the column.

