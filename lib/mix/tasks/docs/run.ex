defmodule Mix.Tasks.Docs.Run do
  @moduledoc """
  Generates documentation and starts a local web server to serve it.

  This task starts a local HTTP server to serve the generated documentation.
  The server will redirect the root path to the main documentation page
  and serve static files from the `doc` directory.

  ## Usage

      mix docs.run
      mix docs.run --port 3000

  ## Options

    * `--port` - The port to run the server on (default: 8000)

  ## Examples

      # Start docs server on default port 8000
      mix docs.run

      # Start docs server on port 3000
      mix docs.run --port 3000

  The server will be available at `http://localhost:<port>/docs/` and will
  automatically redirect from the root path to the main documentation page.
  """

  @shortdoc "Start a local web server to preview documentation"

  use Mix.Task

  def run(args) do
    unless Code.ensure_loaded?(Bandit) do
      Mix.raise("Bandit is required to run the docs server.")
    end

    {opts, _, _} = OptionParser.parse(args, switches: [port: :integer], aliases: [p: :port])

    port = opts[:port] || 8000
    origin = "http://localhost:#{port}"

    Code.eval_string("""
    defmodule Router do
      @moduledoc false

      use Plug.Router

      def port, do: #{port}
      def origin, do: "#{origin}"

      plug(Plug.Logger)

      plug(:match)
      plug(:dispatch)

      plug(Plug.Static, at: "/docs", from: "doc", gzip: false)

      match _ do
        conn
        |> Plug.Conn.resp(:found, "")
        |> Plug.Conn.put_resp_header("location", "#{origin}/docs/readme.html")
      end
    end
    """)

    Mix.Task.run("docs")

    bandit = {Bandit, plug: Router, scheme: :http, port: port}
    {:ok, _} = Supervisor.start_link([bandit], strategy: :one_for_one)

    Mix.shell().info("Documentation server started at #{origin}")

    # unless running from IEx, sleep indefinitely so we can serve requests
    unless IEx.started?() do
      Process.sleep(:infinity)
    end
  end
end
