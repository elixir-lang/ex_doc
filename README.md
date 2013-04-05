# ExDoc

ExDoc is a tool to generate documentation for your Elixir projects. In case you are looking for documentation for Elixir itself, [check out Elixir's website](http://elixir-lang.org/).

Currently, it can only be invoked through the command line. You can use it as follow:

1. First clone and compile it:

        git clone https://github.com/elixir-lang/exdoc.git
        cd exdoc
        mix compile

2. Then you are ready to use it in your projects. First move into your project directory and ensure it is compiled:

        cd PATH_TO_YOUR_PROJECT
        mix compile

3. Next invoke the exdoc executable from your project:

        elixir -pa ebin PATH_TO_YOUR_EXDOC/bin/exdoc "PROJECT_NAME" "PROJECT_VERSION" -m "PROJECT_MODULE" -u "https://github.com/GITHUB_USER/GITHUB_REPO/blob/master/%{path}#L%{line}"

For example, here are some acceptable values:

    PROJECT_NAME    => Dynamo
    PROJECT_VERSION => 0.1
    PROJECT_MODULE  => Dynamo (the main module provided by the library)
    GITHUB_USER     => elixir-lang
    GITHUB_REPO     => dynamo

We have plans to integrate ExDoc with Mix, you can [join the discussion here](https://github.com/elixir-lang/exdoc/issues/16).

# License

ExDoc source code is released under Apache 2 License with snippets under MIT-LICENSE.

Check LICENSE file for more information.