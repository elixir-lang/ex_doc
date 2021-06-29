defmodule ExDoc.Language.SpecAST do
  @moduledoc false

  @doc """
  Coverts AST into string.

  * `format_fun` accepts AST and returns an iodata.

  * `ref_fun` accepts a ref and returns it's string representation.
    `ref` can be one of:

      * `{:type, name, arity}`

      * `{:remote_type, module, name, arity}`

  """
  # Notes:
  #
  # We traverse the AST collecting identifiers (types and vars),
  # we replace every one of them with a placeholder that has the
  # same length.
  #
  # For example:
  #
  #   t(a) :: Keyword.t(a) | atom()
  #
  # becomes:
  #
  #   _(_) :: _________(_) | ____()
  #
  # but importantly we have collected the identifiers in order:
  #
  #   t, a, Keyword.t, a, atom
  #
  # Afterwards, we simply convert the placeholders back and pop
  # from our collected identifiers. Variables stay as is but
  # for types, we call the format_fun.
  def to_string(ast, format_fun, ref_fun) do
    {ast, acc} = walk(ast, [])
    acc = Enum.reverse(acc)
    {ast, acc} = dont_process_fun_name(ast, acc)
    string = ast |> format_fun.() |> IO.iodata_to_binary()
    init!(acc)

    Regex.replace(~r/_+/, string, fn _placeholder ->
      case pop!() do
        {:same, name} ->
          "#{name}"

        other ->
          ref_fun.(other)
      end
    end)
  end

  @id {__MODULE__, :placeholders}

  defp init!(items) do
    Process.put(@id, items)
  end

  defp pop!() do
    [head | tail] = Process.get(@id)
    Process.put(@id, tail)
    head
  end

  defp dont_process_fun_name(ast, acc) do
    [head | tail] = acc

    case head do
      # this is e.g. `f` in `f(a) :: a`,
      # mark it as same (no processing)
      {:type, name, _arity} ->
        {ast, [{:same, name} | tail]}

      # this is e.g. `+` in `a + b :: a`,
      # skip it altogether
      {:operator, _} ->
        {ast, tail}
    end
  end

  defp walk(quoted, acc) do
    Macro.prewalk(quoted, acc, fn
      {:"::", _, _} = ast, acc ->
        {ast, acc}

      {:|, _, _} = ast, acc ->
        {ast, acc}

      {:when, _, _} = ast, acc ->
        {ast, acc}

      {var, meta, context}, acc when is_atom(var) and is_atom(context) ->
        ast = {placeholder(var), meta, context}
        identifier = {:same, var}
        {ast, [identifier | acc]}

      {{:., meta, [module, name]}, _, args}, acc ->
        ast = {placeholder("#{Macro.to_string(module)}.#{name}"), meta, args}
        identifier = {:remote_type, module, name, length(args)}
        {ast, [identifier | acc]}

      {name, meta, args} = ast, acc when is_atom(name) and is_list(args) ->
        arity = length(args)

        if operator?({name, arity}) do
          identifier = {:operator, name}
          {ast, [identifier | acc]}
        else
          ast = {placeholder(name), meta, args}
          identifier = {:type, name, arity}
          {ast, [identifier | acc]}
        end

      other, acc ->
        {other, acc}
    end)
  end

  defp placeholder(atom) when is_atom(atom) do
    atom |> Atom.to_string() |> placeholder()
  end

  defp placeholder(binary) when is_binary(binary) do
    "_"
    |> String.duplicate(byte_size(binary))
    |> String.to_atom()
  end

  unary_operators = [:@, :+, :-, :!, :^, :not, :~~~, :&]

  binary_operators =
    [:., :*, :/, :+, :-, :++, :--, :.., :<>, :+++, :---, :^^^, :in, :"not in"] ++
      [:|>, :<<<, :>>>, :<<~, :~>>, :<~, :~>, :<~>, :<|>, :<, :>, :<=, :>=] ++
      [:==, :!=, :=~, :===, :!==, :&&, :&&&, :and, :||, :|||, :or, :=]

  @operators Enum.map(unary_operators, &{&1, 1}) ++ Enum.map(binary_operators, &{&1, 2})

  defp operator?(type) do
    type in @operators
  end
end
