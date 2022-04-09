# change-env

This package provides a way to modify LaTeX environments, as well as the
display math mode (seeing it as an environment of sorts).  Thus,
henceforth the world "environment" shall—in addition to
\begin--\end-style environments—also refer to display math.

To use this package, simply put `change-env.el` somewhere inside of your
`load-path` and `require` it:

``` emacs-lisp
  (with-eval-after-load 'latex
    (require 'change-env)
    (bind-key (kbd "C-c r") #'change-env LaTeX-mode-map))
```

or, using `use-package':

``` emacs-lisp
  (use-package change-env
    :after latex
    :bind (:map LaTeX-mode-map ("C-c r" . change-env)))
```

The entry point is the `change-env` function, which—when invoked from
inside an environments—pops up a list of possible actions, as defined by
the `change-env-options` variable.  By default, the possible options are

  - Delete the current environment.

  - Modify the current environment; the completion list is provided by
    `LaTeX-enviroment-list`.

  - Convert the current environment to display math.

Note that, while the second option could be implemented to incorporate
the third option as well, it was a conscious decision not to do that.
The goal is to make switching to display math as prominent as possible.
This fits quite well with the authors particular sensibilities.

What exactly we mean by "display math" is controlled by the
`change-env-display` variable.  By default, it is set to `\[...\]` style
display math.  If the excellent [`math-delimiters` package] is also
used, customizing it along the lines of

``` emacs-lisp
  (setq change-env-display math-delimiters-display)
```

keeps consistency with that package.

This package is not compatible with plain tex, as it depends on
AUCTeX—but you are already using that anyways.

[`math-delimiters` package]: https://github.com/oantolin/math-delimiters

## Labels

There is primitive label handling incorporated in `change-env`; this is
controlled by the `change-env-labels` variable.  It is an alist,
containing environment names with their associated label prefix.  If an
environment is switched to another one and both are present in the
alist, the label will be renamed according to that scheme.  For
example—using the default value of `change-env-labels`—switching from an
`equation` environment to a `lemma`:

``` tex
% BEFORE
\begin{equation} \label{eq:test}
  hi!
\end{equation}

% AFTER
\begin{lemma} \label{lem:test}
  hi!
\end{lemma}
```

We also automatically remove the label when switching to display math or
any of the `*`-type environments (`equation*`, `align*`, and the like).

Further, if one deletes a label by switching to display math or an
environment that does not have an associated label in
`change-env-labels`, and later switches back to an environment that
does, the label will be restored _if the contents of the environment
didn't change_.

+ TODO:
  + When we change a label like this, offer to rename it across the
    whole project.
