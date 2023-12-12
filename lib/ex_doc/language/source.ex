defmodule ExDoc.Language.Source do
  @moduledoc false

  @doc """
  Get abstract code for a module
  """
  def get_abstract_code(module) do
    with {^module, binary, _file} <- :code.get_object_code(module),
         {:ok, {_, [{:abstract_code, {_vsn, abstract_code}}]}} <-
           :beam_lib.chunks(binary, [:abstract_code]) do
      abstract_code
    else
      _ -> nil
    end
  end

  @doc """
  Get the basedir of a module

   The basedir is the cwd of the Elixir/Erlang compiler when compiling the module.
   All `-file` attributes in the module is relative to this directory.
  """
  def get_basedir(abst_code, module) do
    ## We look for the first -file attribute to see what the source file that
    ## was compiled is called.
    filename =
      Enum.find_value(abst_code, fn
        {:attribute, _anno, :file, {filename, _line}} ->
          filename

        _ ->
          nil
      end)

    ## The first -file attribute will be either relative or absolute
    ## depending on whether the compiler was called with an absolute
    ## or relative path.
    if Path.type(filename) == :relative do
      ## If the compiler was called with a relative path, then any other
      ## relative -file attribute will be relative to the same directory.
      ## We use `module_info(:compile)[:source]` to get an absolute path
      ## to the source file and calculate the basedir from that

      module.module_info(:compile)[:source]
      |> String.Chars.to_string()
      |> Path.absname()
      |> Path.split()
      |> Enum.reverse()
      |> Enum.split(Path.split(filename) |> Enum.count())
      |> elem(1)
      |> Enum.reverse()
      |> Path.join()
    else
      ## If an absolute path was used, then any relative -file attribute
      ## is relative to the directory of the source file
      Path.dirname(filename)
    end
  end

  def get_module_location(abst_code, source_basedir, module) do
    find_ast(abst_code, source_basedir, fn
      {:attribute, anno, :module, ^module} ->
        {anno_file(anno), anno_line(anno)}

      _ ->
        nil
    end)
  end

  def get_function_location(module_data, {name, arity}) do
    find_ast(module_data.private.abst_code, module_data.source_basedir, fn
      {:function, anno, ^name, ^arity, _} -> {anno_file(anno), anno_line(anno)}
      _ -> nil
    end)
  end

  # Returns a map of {name, arity} => spec.
  def get_specs(abst_code, source_basedir) do
    filtermap_ast(abst_code, source_basedir, fn
      {:attribute, _anno, :spec, {name, _types}} = spec ->
        {name, spec}

      _ ->
        nil
    end)
    |> Map.new()
  end

  def get_type_from_module_data(module_data, name, arity) do
    find_ast(module_data.private.abst_code, module_data.source_basedir, fn
      {:attribute, anno, type, {^name, _, args} = spec} = attr ->
        if type in [:opaque, :type] and length(args) == arity do
          %{
            type: type,
            spec: spec,
            attr: attr,
            source_file: anno_file(anno),
            source_line: anno_line(anno)
          }
        end

      _ ->
        nil
    end)
  end

  def get_callbacks(abst_code, source_basedir) do
    filtermap_ast(abst_code, source_basedir, fn
      {:attribute, _anno, :callback, {name, _types}} = callback ->
        {name, callback}

      _ ->
        nil
    end)
    |> Map.new()
  end

  def get_optional_callbacks(module, type) do
    optional_callbacks =
      type == :behaviour &&
        try do
          module.behaviour_info(:optional_callbacks)
        rescue
          FunctionClauseError -> :undefined
        end

    case optional_callbacks do
      :undefined -> []
      _ -> optional_callbacks
    end
  end

  def find_ast(ast, source_basedir, fun) do
    filtermap_ast(ast, source_basedir, fun) |> hd()
  end

  def filtermap_ast(ast, source_basedir, fun) do
    Enum.reduce(ast, {nil, []}, fn
      {:attribute, _anno, :file, {filename, _line}}, {_file, acc} ->
        {Path.join(source_basedir, filename), acc}

      entry, {file, acc} ->
        anno = :erl_anno.set_file(file, elem(entry, 1))

        if entry = fun.(put_elem(entry, 1, anno)) do
          {file, [entry | acc]}
        else
          {file, acc}
        end

      _, file_acc ->
        file_acc
    end)
    |> elem(1)
  end

  def anno_line(line) when is_integer(line), do: abs(line)
  def anno_line(anno), do: anno |> :erl_anno.line() |> abs()

  def anno_file(anno) do
    case :erl_anno.file(anno) do
      :undefined ->
        nil

      file ->
        String.Chars.to_string(file)
    end
  end
end
