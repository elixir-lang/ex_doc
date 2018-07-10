children = [
  {Plug.Adapters.Cowboy, scheme: :http, plug: MyApp, options: [port: 4040]}
]

Supervisor.start_link(children, strategy: :one_for_one)
# example
# Starts a new interface
Plug.Adapters.Cowboy.http MyPlug, [], port: 80

# The interface above can be shutdown with
Plug.Adapters.Cowboy.shutdown MyPlug.HTTP
# example
# Starts a new interface
Plug.Adapters.Cowboy.https MyPlug, [],
  port: 443,
  password: "SECRET",
  otp_app: :my_app,
  keyfile: "priv/ssl/key.pem",
  certfile: "priv/ssl/cert.pem",
  dhfile: "priv/ssl/dhparam.pem"

# The interface above can be shutdown with
Plug.Adapters.Cowboy.shutdown MyPlug.HTTPS
# example
children = [
  {Plug.Adapters.Cowboy2, scheme: :http, plug: MyApp, options: [port: 4040]}
]

Supervisor.start_link(children, strategy: :one_for_one)
# example
# Starts a new interface
Plug.Adapters.Cowboy2.http MyPlug, [], port: 80

# The interface above can be shutdown with
Plug.Adapters.Cowboy2.shutdown MyPlug.HTTP
# example
# Starts a new interface
Plug.Adapters.Cowboy2.https MyPlug, [],
  port: 443,
  password: "SECRET",
  otp_app: :my_app,
  keyfile: "priv/ssl/key.pem",
  certfile: "priv/ssl/cert.pem",
  dhfile: "priv/ssl/dhparam.pem"

# The interface above can be shutdown with
Plug.Adapters.Cowboy2.shutdown MyPlug.HTTPS
# example
defmodule MyApp do
  use Plug.Builder

  plug Plug.Logger
  plug :hello, upper: true

  # A function from another module can be plugged too, provided it's
  # imported into the current module first.
  import AnotherModule, only: [interesting_plug: 2]
  plug :interesting_plug

  def hello(conn, opts) do
    body = if opts[:upper], do: "WORLD", else: "world"
    send_resp(conn, 200, body)
  end
end
# example
defmodule PlugWithCustomOptions do
  use Plug.Builder
  plug Plug.Logger

  def init(opts) do
    opts
  end
end
# example
defmodule PlugWithCustomCall do
  use Plug.Builder
  plug Plug.Logger
  plug Plug.Head

  def call(conn, opts) do
    conn
    |> super(opts) # calls Plug.Logger and Plug.Head
    |> assign(:called_all_plugs, true)
  end
end
# example
defmodule PlugUsingHalt do
  use Plug.Builder

  plug :stopper
  plug Plug.Logger

  def stopper(conn, _opts) do
    halt(conn)
  end
end
# example
{plug_name, options, guards}
# example
Plug.Builder.compile(env, [
  {Plug.Logger, [], true}, # no guards, as added by the Plug.Builder.plug/2 macro
  {Plug.Head, [], quote(do: a when is_binary(a))}
], [])
# example
plug Plug.Logger               # plug module
plug :foo, some_options: true  # plug function
# example
plug Plug.Session, ...
plug :fetch_session
plug Plug.CSRFProtection
# example
iex> decode("key1=value1, key2=value2")
%{"key1" => "value1", "key2" => "value2"}
# example
iex> decode("foo=bar")["foo"]
"bar"
# example
iex> decode("foo=bar&foo=baz")["foo"]
"baz"
# example
iex> decode("foo[bar]=baz")["foo"]["bar"]
"baz"
# example
iex> decode("foo[]=bar&foo[]=baz")["foo"]
["bar", "baz"]
# example
iex> encode(%{foo: "bar", baz: "bat"})
"baz=bat&foo=bar"
# example
iex> encode([foo: "bar", baz: "bat"])
"foo=bar&baz=bat"
# example
iex> encode([foo: "bar", foo: "bat"])
"foo=bar"
# example
iex> encode(%{foo: ["bar", "baz"]})
"foo[]=bar&foo[]=baz"
# example
iex> encode(%{foo: %{bar: "baz"}})
"foo[bar]=baz"
# example
unfetched = %Plug.Conn.Unfetched{aspect: :cookies}
# example
iex> content_type "x-sample/json; charset=utf-8"
{:ok, "x-sample", "json", %{"charset" => "utf-8"}}

iex> content_type "x-sample/json  ; charset=utf-8  ; foo=bar"
{:ok, "x-sample", "json", %{"charset" => "utf-8", "foo" => "bar"}}

iex> content_type "\r\n text/plain;\r\n charset=utf-8\r\n"
{:ok, "text", "plain", %{"charset" => "utf-8"}}

iex> content_type "text/plain"
{:ok, "text", "plain", %{}}

iex> content_type "x/*"
:error

iex> content_type "*/*"
:error
# example
iex> list("foo, bar")
["foo", "bar"]

iex> list("foobar")
["foobar"]

iex> list("")
[]

iex> list("empties, , are,, filtered")
["empties", "are", "filtered"]
# example
iex> media_type "text/plain"
{:ok, "text", "plain", %{}}

iex> media_type "APPLICATION/vnd.ms-data+XML"
{:ok, "application", "vnd.ms-data+xml", %{}}

iex> media_type "text/*; q=1.0"
{:ok, "text", "*", %{"q" => "1.0"}}

iex> media_type "*/*; q=1.0"
{:ok, "*", "*", %{"q" => "1.0"}}

iex> media_type "x y"
:error

iex> media_type "*/html"
:error

iex> media_type "/"
:error

iex> media_type "x/y z"
:error
# example
iex> params("foo=bar")
%{"foo" => "bar"}

iex> params("  foo=bar  ")
%{"foo" => "bar"}

iex> params("FOO=bar")
%{"foo" => "bar"}

iex> params("Foo=bar; baz=BOING")
%{"foo" => "bar", "baz" => "BOING"}

iex> params("foo=BAR ; wat")
%{"foo" => "BAR"}

iex> params("foo=\"bar\"; baz=\"boing\"")
%{"foo" => "bar", "baz" => "boing"}

iex> params("foo=\"bar;\"; baz=\"boing\"")
%{"foo" => "bar;", "baz" => "boing"}

iex> params("=")
%{}
# example
iex> token("foo")
"foo"

iex> token("foo-bar")
"foo-bar"

iex> token("<foo>")
false

iex> token(~s["<foo>"])
"<foo>"

iex> token(~S["<f\oo>\"<b\ar>"])
"<foo>\"<bar>"

iex> token("foo  ")
"foo"

iex> token("foo bar")
false
# example
# Send the chunked response headers
conn = send_chunked(conn, 200)

# Pipe the given list into a connection
# Each item is emitted as a chunk
Enum.into(~w(each chunk as a word), conn)
# example
config :plug, :statuses, %{
  404 => "Actually This Was Found",
  451 => "Unavailable For Legal Reasons"
}
# example
mix deps.clean --build plug
# example
put_status(conn, :not_found)                     # 404
put_status(conn, :actually_this_was_found)       # 404
put_status(conn, :unavailable_for_legal_reasons) # 451
# example
iex> conn.assigns[:hello]
nil
iex> conn = assign(conn, :hello, :world)
iex> conn.assigns[:hello]
:world
# example
~w(each chunk as a word)
|> Enum.reduce_while(conn, fn (chunk, conn) ->
  case Plug.Conn.chunk(conn, chunk) do
    {:ok, conn} ->
      {:cont, conn}
    {:error, :closed} ->
      {:halt, conn}
  end
end)
# example
iex> conn = %{conn | resp_headers: [{"content-type", "text/plain"}]}
iex> get_resp_header(conn, "content-type")
["text/plain"]
# example
iex> conn.assigns[:hello]
nil
iex> conn = merge_assigns(conn, hello: :world)
iex> conn.assigns[:hello]
:world
# example
iex> conn.private[:plug_hello]
nil
iex> conn = merge_private(conn, plug_hello: :world)
iex> conn.private[:plug_hello]
:world
# example
iex> conn = merge_resp_headers(conn, [{"content-type", "text/plain"}, {"X-1337", "5P34K"}])
# example
iex> conn.private[:plug_hello]
nil
iex> conn = put_private(conn, :plug_hello, :world)
iex> conn.private[:plug_hello]
:world
# example
{:ok, body, conn} = Plug.Conn.read_body(conn, length: 1_000_000)
# example
Plug.Conn.send_file(conn, 200, "README.md")
# example
secret_key_base = "072d1e0157c008193fe48a670cce031faa4e..."
encrypted_cookie_salt = "encrypted cookie"
encrypted_signed_cookie_salt = "signed encrypted cookie"

secret = KeyGenerator.generate(secret_key_base, encrypted_cookie_salt)
sign_secret = KeyGenerator.generate(secret_key_base, encrypted_signed_cookie_salt)

data = "José"
encrypted = MessageEncryptor.encrypt(data, secret, sign_secret)
decrypted = MessageEncryptor.decrypt(encrypted, secret, sign_secret)
decrypted # => {:ok, "José"}
# example
defmodule MyApp do
  use Plug.Builder

  if Mix.env == :dev do
    use Plug.Debugger, otp_app: :my_app
  end

  plug :boom

  def boom(conn, _) do
    # Error raised here will be caught and displayed in a debug page
    # complete with a stacktrace and other helpful info.
    raise "oops"
  end
end
# example
use Plug.Debugger, style:
  [primary: "#c0392b", logo: "data:image/png;base64,..."]
# example
txmt://open/?url=file://__FILE__&line=__LINE__
# example
defmodule AppRouter do
  use Plug.Router
  use Plug.ErrorHandler

  plug :match
  plug :dispatch

  get "/hello" do
    send_resp(conn, 200, "world")
  end

  def handle_errors(conn, %{kind: _kind, reason: _reason, stack: _stack}) do
    send_resp(conn, conn.status, "Something went wrong")
  end
end
# example
iex> Plug.HTML.html_escape("foo")
"foo"

iex> Plug.HTML.html_escape("<foo>")
"&lt;foo&gt;"

iex> Plug.HTML.html_escape("quotes: \" & \'")
"quotes: &quot; &amp; &#39;"
# example
iex> Plug.HTML.html_escape_to_iodata("foo")
"foo"

iex> Plug.HTML.html_escape_to_iodata("<foo>")
[[[] | "&lt;"], "foo" | "&gt;"]

iex> Plug.HTML.html_escape_to_iodata("quotes: \" & \'")
[[[[], "quotes: " | "&quot;"], " " | "&amp;"], " " | "&#39;"]
# example
Plug.Head.call(conn, [])
# example
GET /index.html
Sent 200 in 572ms
# example
plug Plug.Logger, log: :debug
# example
Plug.MethodOverride.call(conn, [])
# example
plug Plug.Parsers, parsers: [:urlencoded, :multipart]

plug Plug.Parsers, parsers: [:urlencoded, :json],
                   pass: ["text/*"],
                   json_decoder: Jason
# example
plug Plug.Parsers,
     parsers: [
       :url_encoded,
       {:multipart, length: 20_000_000} # Increase to 20MB max upload
     ]
# example
defmodule CacheBodyReader do
  def read_body(conn, opts, verified_providers, verifiers) do
    {:ok, body, conn} = Plug.Conn.read_body(conn, opts)
    conn = update_in(conn.assigns[:raw_body], &[body | (&1 || [])])
    {:ok, body, conn}
  end
end
# example
plug Plug.Parsers,
  parsers: [:urlencoded, :json],
  pass: ["text/*"],
  body_reader: {CacheBodyReader, :read_body, []},
  json_decoder: Jason
# example
config :logger, :console, metadata: [:request_id]
# example
plug Plug.RequestId
# example
defmodule AppRouter do
  use Plug.Router

  plug :match
  plug :dispatch

  get "/hello" do
    send_resp(conn, 200, "world")
  end

  match _ do
    send_resp(conn, 404, "oops")
  end
end
# example
AppRouter.call(conn, AppRouter.init([]))
# example
get "/hello", private: %{an_option: :a_value} do
  send_resp(conn, 200, "world")
end
# example
get "/hello" do
  send_resp(conn, 200, "world")
end
# example
get "/hello/:name" do
  send_resp(conn, 200, "hello #{name}")
end
# example
get "/hello/*_rest" do
  send_resp(conn, 200, "matches all routes starting with /hello")
end

get "/hello/*glob" do
  send_resp(conn, 200, "route after /hello: #{inspect glob}")
end
# example
match "/hello" do
  send_resp(conn, 200, "world")
end
# example
defmodule AppRouter do
  use Plug.Router

  plug :match
  plug Plug.Parsers, parsers: [:json],
                     pass:  ["application/json"],
                     json_decoder: Jason
  plug :dispatch

  post "/hello" do
    IO.inspect conn.body_params # Prints JSON POST body
    send_resp(conn, 200, "Success!")
  end
end
# example
defmodule AppRouter do
  use Plug.Router

  if Mix.env == :dev do
    use Plug.Debugger
  end

  use Plug.ErrorHandler

  plug :match
  plug :dispatch

  get "/hello" do
    send_resp(conn, 200, "world")
  end

  defp handle_errors(conn, %{kind: _kind, reason: _reason, stack: _stack}) do
    send_resp(conn, conn.status, "Something went wrong")
  end
end
# example
match "/foo/bar", via: :get do
  send_resp(conn, 200, "hello world")
end
# example
defp match("GET", ["foo", "bar"], conn) do
  send_resp(conn, 200, "hello world")
end
# example
match "/foo/bar/:baz" when size(baz) <= 3, via: :get do
  send_resp(conn, 200, "hello world")
end
# example
forward "/users", to: UserRouter
# example
forward "/foo/:bar/qux", to: FooPlug
# example
forward "/foo/bar", to: :foo_bar_plug, host: "foobar."
forward "/baz", to: BazPlug, init_opts: [plug_specific_option: true]
# example
match "/foo/bar", via: :get do
  send_resp(conn, 200, "hello world")
end

match "/baz", to: MyPlug, init_opts: [an_option: :a_value]
# example
plug Plug.SSL, rewrite_on: [:x_forwarded_proto]
# example
plug :put_secret_key_base

def put_secret_key_base(conn, _) do
  put_in conn.secret_key_base, "-- LONG STRING WITH AT LEAST 64 BYTES --"
end
# example
# Use the session plug with the table name
plug Plug.Session, store: :cookie,
                   key: "_my_app_session",
                   encryption_salt: "cookie store encryption salt",
                   signing_salt: "cookie store signing salt",
                   key_length: 64,
                   log: :debug
# example
{sid :: String.t, data :: map, timestamp :: :erlang.timestamp}
# example
# Create an ETS table when the application starts
:ets.new(:session, [:named_table, :public, read_concurrency: true])

# Use the session plug with the table name
plug Plug.Session, store: :ets, key: "sid", table: :session
# example
plug Plug.Session, store: :ets, key: "_my_app_session", table: :session
# example
plug Plug.Static, from: "priv/app/path"
# example
plug Plug.Static, from: {:app_name, "priv/app/path"}
# example
defmodule MyPlug do
  use Plug.Builder

  plug Plug.Static,
    at: "/public",
    from: :my_app,
    only: ~w(images robots.txt)
  plug :not_found

  def not_found(conn, _) do
    send_resp(conn, 404, "not found")
  end
end
# example
use ExUnit.Case, async: true
use Plug.Test
# example
config :plug, :validate_header_keys_during_test, true
# example
conn(:get, "/foo?bar=10")
conn(:get, "/foo", %{bar: 10})
conn(:post, "/")
conn("patch", "/", "") |> put_req_header("content-type", "application/json")
# example
conn = conn(:get, "/foo?bar=10")
pushes = Plug.Test.sent_pushes(conn)
assert {"/static/application.css", [{"accept", "text/css"}]} in pushes
assert {"/static/application.js", [{"accept", "application/javascript"}]} in pushes
# example
(Plug.Conn.t, Plug.opts) :: Plug.Conn.t
# example
def json_header_plug(conn, opts) do
  Plug.Conn.put_resp_content_type(conn, "application/json")
end
# example
defmodule JSONHeaderPlug do
  import Plug.Conn

  def init(opts) do
    opts
  end

  def call(conn, _opts) do
    put_resp_content_type(conn, "application/json")
  end
end
# example
defmodule MyPlug do
  import Plug.Conn

  def init(options) do
    # initialize options

    options
  end

  def call(conn, _opts) do
    conn
    |> put_resp_content_type("text/plain")
    |> send_resp(200, "Hello world")
  end
end
# example
$ iex -S mix
iex> c "path/to/file.ex"
[MyPlug]
iex> {:ok, _} = Plug.Adapters.Cowboy.http MyPlug, []
{:ok, #PID<...>}
# example
def deps do
  [{:cowboy, "~> 1.0.0"},
   {:plug, "~> 1.0"}]
end
# example
def application do
  [applications: [:cowboy, :plug]]
end
# example
def hello_world_plug(conn, _opts) do
  conn
  |> put_resp_content_type("text/plain")
  |> send_resp(200, "Hello world")
end
# example
defmodule MyPlug do
  def init([]), do: false
  def call(conn, _opts), do: conn
end
# example
%Plug.Conn{host: "www.example.com",
           path_info: ["bar", "baz"],
           ...}
# example
conn = put_resp_content_type(conn, "text/plain")
conn = send_resp(conn, 200, "ok")
conn
# example
defmodule MyRouter do
  use Plug.Router

  plug :match
  plug :dispatch

  get "/hello" do
    send_resp(conn, 200, "world")
  end

  forward "/users", to: UsersRouter

  match _ do
    send_resp(conn, 404, "oops")
  end
end
# example
plug Plug.Logger
plug :match
plug :dispatch
# example
$ mix new my_app --sup
# example
defmodule MyApp do
  use Application

  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  def start(_type, _args) do
    children = [
      # Define workers and child supervisors to be supervised
      Plug.Adapters.Cowboy.child_spec(scheme: :http, plug: MyRouter, options: [port: 4001])
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: MyApp.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
# example
defmodule MyPlugTest do
  use ExUnit.Case, async: true
  use Plug.Test

  @opts MyRouter.init([])

  test "returns hello world" do
    # Create a test connection
    conn = conn(:get, "/hello")

    # Invoke the plug
    conn = MyRouter.call(conn, @opts)

    # Assert the response and status
    assert conn.state == :sent
    assert conn.status == 200
    assert conn.resp_body == "world"
  end
end