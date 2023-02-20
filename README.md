# latex-change-env

This package provides a way to modify LaTeX environments, macros, as
well as inline and display maths mode (seeing them as an environment of
sorts).  There exists a [blog post], which includes some moving pictures
that showcase the functionality of this package.

[blog post]: https://tony-zorman.com/posts/latex-change-env-0.3.html

## Installation

### MELPA

The package is on MELPA, so you can install it like any other package:

    M-x package-install RET latex-change-env RET

### Manual

Put `latex-change-env.el` somewhere inside of your `load-path` and
`require` it:

``` emacs-lisp
  (require 'latex-change-env)
```

## Configuration

Note that, in the sequel, "environment" shall usually refer to LaTeX
environments, macros, as well as inline and display maths mode—not just
to LaTeX environments themselves.

The entry point is the `latex-change-env` function, which—when invoked
from inside an environments—pops up a list of possible actions, as
defined by the `latex-change-env-options` variable.  There is also the
option to cycle through arguments in `latex-change-env-cycle`.  This
then depends on [`math-delimiters`] for convenient integration of
math-style cycles.

An example initial configuration of the package may look like:

``` emacs-lisp
  (with-eval-after-load 'latex
    (require 'latex-change-env)
    (bind-key (kbd "C-c r") #'latex-change-env LaTeX-mode-map))
```

or, using `use-package`:

``` emacs-lisp
  (use-package latex-change-env
    :after latex
    :bind (:map LaTeX-mode-map ("C-c r" . latex-change-env)))
```

By default, the default values of the `latex-change-env-options`
variable are

  - Delete the current environment.

  - Modify the current environment; the completion list is provided by
    `LaTeX-enviroment-list`.

  - Convert the current environment to display math.

Note that, while the second option could be implemented to incorporate
the third option as well, it was a conscious decision not to do that.
The goal is to make switching to display math as prominent as possible.
This fits quite well with the authors particular sensibilities.

What exactly we mean by "inline" and "display maths" is controlled by
the `latex-change-env-math-{inline,display}` variables.  By default,
these are set to `$…$` and `\[...\]` respectively.  If the excellent
[`math-delimiters`] package is also used, customizing them along the
lines of

``` emacs-lisp
  (setq latex-change-env-math-display math-delimiters-display)
  (setq latex-change-env-math-inline  math-delimiters-inline )
```

keeps consistency with that package.

This package is not compatible with plain tex, as it depends on
AUCTeX—but you are already using that anyways.

[`math-delimiters`]: https://github.com/oantolin/math-delimiters

### Labels

There is primitive label handling incorporated in `latex-change-env`;
this is controlled by the `latex-change-env-labels` variable.  It is an
alist, containing environment names with their associated label prefix.
If an environment is switched to another one and both are present in the
alist, the label will be renamed according to that scheme.  For
example—using the default value of `latex-change-env-labels`—switching
from an `equation` environment to a `lemma`:

``` tex
% BEFORE
\begin{theorem} \label{thm:test}
  hi!
\end{theorem}

% AFTER
\begin{lemma} \label{lem:test}
  hi!
\end{lemma}
```

We also automatically remove the label when switching to display math or
any of the `*`-type environments (`equation*`, `align*`, and the like).

Further, if one deletes a label by switching to display math or an
environment that does not have an associated label in
`latex-change-env-labels`, and later switches back to an environment
that does, the label will be restored _if the contents of the
environment didn't change_.

Lastly, when a label changes and the user has customised
`latex-change-env-edit-project-labels` to `t` then an interactive
`query-replace` session is started in the current project to replace any
labels with their potentially updated version.
