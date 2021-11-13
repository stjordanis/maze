Layout Placeholders
===================

There are a few important and confusing placeholders used in the
layout files. They are documented here:

  - `{{ main }}`:
    - Points to the main website (home).
    - Has the value `/` while running `make site`.
    - Has the value `https://susam.in/` while running `make dist`.
    - On `https://susam.in/maze/` it points to `https://susam.in/`.
    - On `https://susam.github.io/maze/` it points `https://susam.github.io/`.
    - On local file system, it points to `https://susam.in/`.
    - Helps to choose between a short path without domain vs. a full
      URL with domain based on whether the files are deployed on a
      live web server or on a local filesystem.
    - Used to link to files on the main website from Maze pages.
  - `{{ site-url }}`:
    - Points to the canonical URL (primary URL) of the Maze website.
    - Always has the value `https://susam.in/maze/`.
    - All links to comment/subscribe app must use this because this
      app only runs on the web-server for `susam.in`. This app does
      not run on the mirror on GitHub.
    - Links in Feed must also use this URL, so that feed users reach
      the canonical website.
