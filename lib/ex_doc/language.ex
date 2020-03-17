defmodule ExDoc.Language do
  @type mode() :: :regular | :custom_link

  @callback text_to_ref(String.t(), mode(), config :: term()) :: {:ok, term()} | :error
end

defmodule ExDoc.Language.Elixir do
  @behaviour ExDoc.Language

  def text_to_ref("c:" <> text, mode, config) do
    with {call, {:function, module, name, arity}} <- text_to_ref(text, mode, config) do
      {call, {:callback, module, name, arity}}
    end
  end

  def text_to_ref("t:" <> text, mode, config) do
    with {call, {:function, module, name, arity}} <- text_to_ref(text, mode, config) do
      {call, {:type, module, name, arity}}
    end
  end

  def text_to_ref("mix help " <> name, _mode, _config), do: mix_task(name)
  def text_to_ref("mix " <> name, _mode, _config), do: mix_task(name)

  def text_to_ref(text, mode, config) do
    if not String.contains?(text, [" ", "(", ")"]) do
      case Code.string_to_quoted(text) do
        {:ok, {:__aliases__, _, _} = module} ->
          {:module, module(module)}

        {:ok, {:/, _, [{{:., _, [module, name]}, _, []}, arity]}}
        when is_atom(name) and is_integer(arity) ->
          {:remote, {:function, module(module), name, arity}}

        {:ok, {:/, _, [{name, _, _}, arity]}}
        when is_atom(name) and is_integer(arity) ->
          {:local, {:function, config.current_module, name, arity}}

        {:ok, erlang_module} when is_atom(erlang_module) and mode == :custom_link ->
          {:module, erlang_module}

        _ ->
          :no_ref
      end
    else
      :no_ref
    end
  end

  defp module({:__aliases__, _, _} = module), do: Module.concat([Macro.to_string(module)])
  defp module(module) when is_atom(module), do: module

  defp mix_task(name) do
    if name =~ ~r/^[a-z][a-z0-9]*(\.[a-z][a-z0-9]*)*$/ do
      parts = name |> String.split(".") |> Enum.map(&Macro.camelize/1)
      {:module, Module.concat([Mix, Tasks | parts])}
    else
      :no_ref
    end
  end
end

defmodule ExDoc.Language.Erlang do
  @behaviour ExDoc.Language

  @impl true
  def text_to_ref(_code, _mode, _config) do
    # erlang code blocks are already auto-linked
    :no_ref
  end
end
