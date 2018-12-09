# Guidance

> Contributing to an open-source codebase for the first time can be intimidating --
huge files with functions that call other functions in other huge files.
>
> This doc aims to make contributing a little friendlier.

Given the nature of this project, you'll most likely want to start at [`docs.ex`](https://github.com/elixir-lang/ex_doc/blob/master/lib/mix/tasks/docs.ex#L230)

## docs.ex

> If you're unfamiliar with [`Mix.Task`s](https://hexdocs.pm/mix/Mix.Task.html)...

1. When a user runs `mix docs`, `run/3` is executed. As noted in the `@moduledoc`,
the `args` are from the command line. As we'll be generating docs from the task,
the first step is to compile our user's Mix project.

2. We guard against bad command line `args`

3. After setting up `options`, we generate docs in a number of formats and
log messages with where our user can find them in the project.

## ex_doc.ex

So what's this [`generator/3`](https://github.com/elixir-lang/ex_doc/blob/master/lib/mix/tasks/docs.ex#L264) anonymous function being called? Well, it [defaults](https://github.com/elixir-lang/ex_doc/blob/master/lib/mix/tasks/docs.ex#L240) to `ExDoc.generate_docs/3`, so let's open up [`ex_doc.ex`](https://github.com/elixir-lang/ex_doc/blob/master/lib/ex_doc.ex#L18) to see what that function does...

4. `generate_docs/3` has guards to ensure arguments are a certain type.
5. We generate a `config`. This will be a [big struct](https://github.com/elixir-lang/ex_doc/blob/master/lib/ex_doc/config.ex#L18) which we feed `options` into.
6. But first, we once again need to setup our `options`.
7. Next, back in `generate_docs/3`, we call [two `ExDoc.Markdown` functions](https://github.com/elixir-lang/ex_doc/blob/master/lib/ex_doc/markdown.ex#L136) with side-effects.

## retriever.ex

8. 

# Guidelines

* TODO
