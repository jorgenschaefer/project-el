This is a very simple Emacs extension to define a project root for the
current file. This extension does nothing with the project root, but
it can be used by other extensions to work on the "current project"
without each and every one of them defining their own project root.

To use in an extension, simply use (project-root) to get the current
project root. This will either return a configured project root, use a
number of heuristics to find it, or ask the user.

To extend the heuristics, add new functions to
`project-guess-root-functions`, or customize `project-vc-root-marker`
and `project-vc-directory-marker`.

Finally, this library allows project-specific variables to be set and
read. To enable this functionality, set `project-root-file` to a
string. This will cause a file of that name to be created in the
project root. You can use `project-set` and `project-get` to set and
get variables from that file.
