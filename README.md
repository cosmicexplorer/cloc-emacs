cloc-emacs
==========

This is a small attempt at cloc integration for Emacs. The functionality is exposed through two functions: `cloc`, an interactive function which performs a search through file-visiting buffers whose filepaths match the given regex (or the current buffer, as desired), and `cloc-get-results-as-plists`, which does the same thing, but parses and organizes it all into a list of plists for easier analysis.

Example searches include:

- `\.cpp$`, for all C++ sources files
- `/foo/`, where `"/foo/"` is the name of a project directory
  - cloc will then count all code over all open buffers visiting files within the directory named foo.

# Setup:

```
;; do this only once ever
(package-install 'cloc)
;; put this in your .emacs init file
(require 'cloc)
```

# Usage:

- `M-x cloc`: Interactive function to run the executable "cloc" over file-visiting buffers with pathname specified by a regex. If a prefix argument or a blank regex is given, the current buffer is "cloc'd". cloc's entire summary output is given in the messages buffer.

- `cloc-get-results-as-plists`: Non-interactive function to get output of cloc results as a list of plists. Each plist contains as a property the number of files analyzed, the blank lines, the code lines, comment lines, etc. for a given language in the range of files tested. If PREFIX-GIVEN is set to true, this runs on the current buffer. If not, and REGEX is given, it will search file-visiting buffers for file paths matching the regex. If the regex is nil, it will prompt for a regex; putting in a blank there will default to the current buffer."

# Screenshot:

![cloc example usage](doc/cloc-screenshot.png)
