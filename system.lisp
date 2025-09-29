;;; -*- Lisp -*-

(in-package "GEMINI")

(defparameter +new-system-prompt+
  "Perform these steps:
 0) Pay careful attention to the directory paths and filenames used below.  Avoid typos and do not be sloppy.
 1) Query the user for a case-sensitive project name like `Foo`.  Call this the `case-sensitive-system-name`.
 2) Convert the `case-sensitive-system-name` to a lower case string to get the `system-name`.
 3) Convert the `case-sensitive-system-name` to an upper case string to get the `package-name`.
 4) If the `~/quicklisp/` directory exists, list the directory contents.  After the tool returns the list, display the complete list of files to the user.
 5) If the `~/quicklisp/local-projects/` exists, list the directory contents.  After the tool returns the list, display the complete list of files to the user.
 6) Check for existence of directory of `~/quicklisp/local-projects/{case-sensitive-system-name}/`.  If it does not exist, create it.  This is the `project-root` directory.
 7) If project-root directory is not a git repository, make it be a git repository.
 8) Create a `{project-root}/src/` subdirectory.
 9) Create an appropriate `README.md` file in the project-root directory.
 10) Stage the `README.md` for git.
 11) Create `{project-root}/src/package.lisp` file.  
     * This file should have a comment line indicating the emacs major mode and file encoding (utf-8) followed by a blank line.
     * This file should have a defpackage form that defines a package named {system-name}.  
     * The package should shadowing-import `compose' from `function`.
     * The package should shadowing-import `let` and `named-lambda` from `named-let`.
     * The package should shadowing-import `defun`, `funcall`, `let*`, and `multiple-value-bind` from `series`.  
     * The :shadowing-import clauses should be first.
     * The package :use clause should be last.
     * The package should use `cl`, `alexandria`, `function`, `fold`, `named-let`, `promise`, and `series`.
     **Always use upper-case strings to name the packages, like the following: (defpackage \"MY-PACKAGE\" (:use \"CL\" \"ALEXANDRIA\")) **.
     **Always use upper-case strings to name the symbols**, like `(:shadowing-import-from \"SERIES\" \"DEFUN\" \"FUNCALL\" \"LET*\)
 12) Now create some lisp files in the `{project-root}/src/` directory.  Each file should have a comment line indicating the emacs major mode and file encoding (utf-8) followed by a blank line.  Each file should have an `in-package` form that uses the {package-name}.  **Always use upper case strings to name the package in the `in-package` form, for example `(in-package \"MY-PACKAGE\")**.  Each file should contain a comment describing the purpose of the file.  Each file should include a sample Lisp form appropriate for the file.
    a) `data.lisp` - purpose: basic data structures and classes.
    b) `generics.lisp` - purpose: to define signatures of generic functions.
    c) `macros.lisp` - purpose: base macros
    d) `misc.lisp` - purpose: miscellaneous low-level lisp functions.
    e) `vars.lisp` - purpose: to hold global variables, constants, and parameters
    f) `{system-name}.lisp` - purpose: entry point of program.
 13) Create a `{system-name}.asd` file in the `{project-root}` directory.
    * It should have a comment line indicating the emacs major mode and file encoding (utf-8) followed by a blank line.
    * It should *not* have an `in-package` form.  
    * It should have one defsystem form.
    * The defsystem form should not be package qualified.
    * The defsystem should define a system named by the string {system-name}.
    * The defsystem should have dependencies on `alexandria`, `function`, `fold`, `named-let`, `series`, and `str`.
    * The depended upon systems should be named with lower case strings.
    * It should have one module called `src`.
    * The `src` module should have the file components of the files created above, listed alphabetically.
    * The `package` file should have no dependecies.
    * All other files should at least depend on `package`.  
    * All files other than `package` and `macros` should depend on `macros`.
    * The `{system-name}` file should depend on the other lisp files.
 14) Stage all the lisp files and the system definition file.
 15) Commit.
")
(defun new-system ()
  "Create and initialize a new Common Lisp project."
  (let ((*include-thoughts* t))
    (invoke-gemini
     (list
      (part +new-system-prompt+)))))
