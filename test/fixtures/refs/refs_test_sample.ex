modules = [
  {RefsTest.Public, "This is a public module."},
  {RefsTest.Hidden, false}
]

for {module, moduledoc} <- modules do
  contents =
    quote do
      @moduledoc unquote(moduledoc)

      @type a_type() :: any()
      @typep a_typep() :: any()
      @opaque an_opaque() :: {any()}

      @callback a_callback(any()) :: boolean
      @macrocallback a_macrocallback(any()) :: boolean

      defmacro macro_public(x),
        do: quote(do: unquote(x))

      defmacrop macro_private(x),
        do: quote(do: unquote(x))

      @spec function_public(a_type()) :: a_typep()
      def function_public(x),
        do: function_private(x)

      defp function_private(x),
        do: macro_private(x)
    end

  Module.create(module, contents, Macro.Env.location(__ENV__))
end
