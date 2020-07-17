defmodule ExDoc.Utils.Code do
  @moduledoc false

  # TODO: this is vendored from Elixir v1.11.0.
  #       Remove and use Code.fetch_docs/1 in the future.

  def fetch_docs(module) when is_atom(module) do
    case :code.get_object_code(module) do
      {_module, bin, beam_path} ->
        case fetch_docs_from_beam(bin) do
          {:error, :chunk_not_found} ->
            app_root = Path.expand(Path.join(["..", ".."]), beam_path)
            path = Path.join([app_root, "doc", "chunks", "#{module}.chunk"])
            fetch_docs_from_chunk(path)

          other ->
            other
        end

      :error ->
        case :code.which(module) do
          :preloaded ->
            path = Path.join([:code.lib_dir(:erts), "doc", "chunks", "#{module}.chunk"])
            fetch_docs_from_chunk(path)

          _ ->
            {:error, :module_not_found}
        end
    end
  end

  def fetch_docs(path) when is_binary(path) do
    fetch_docs_from_beam(String.to_charlist(path))
  end

  @docs_chunk 'Docs'

  defp fetch_docs_from_beam(bin_or_path) do
    case :beam_lib.chunks(bin_or_path, [@docs_chunk]) do
      {:ok, {_module, [{@docs_chunk, bin}]}} ->
        load_docs_chunk(bin)

      {:error, :beam_lib, {:missing_chunk, _, @docs_chunk}} ->
        {:error, :chunk_not_found}

      {:error, :beam_lib, {:file_error, _, :enoent}} ->
        {:error, :module_not_found}
    end
  end

  defp fetch_docs_from_chunk(path) do
    case File.read(path) do
      {:ok, bin} ->
        load_docs_chunk(bin)

      {:error, _} ->
        {:error, :chunk_not_found}
    end
  end

  defp load_docs_chunk(bin) do
    :erlang.binary_to_term(bin)
  rescue
    _ ->
      {:error, {:invalid_chunk, bin}}
  end

  def get_abstract_code(module) when is_atom(module) do
    {^module, beam} = get_module_and_beam(module)

    case :beam_lib.chunks(beam, [:abstract_code]) do
      {:ok, {_, [{:abstract_code, {_vsn, abstract_code}}]}} -> abstract_code
      _otherwise -> []
    end
  end

  # Borrowed from Elixir's Code.Typespec module
  def fetch_definitions(module, module_visibility \\ :public) when is_atom(module) do
    with {module, binary} <- get_module_and_beam(module),
         {:ok, {_, [debug_info: {:debug_info_v1, backend, data}]}} <-
           :beam_lib.chunks(binary, [:debug_info]) do
      case data do
        {:elixir_v1, info, _attributes} when is_map(info) ->
          definitions =
            for {{function, arity}, def_kind, _, _} <- info.definitions do
              kind_visibility = kind_visibility(def_kind)
              ref_visibility = ref_visibility(kind_visibility, module_visibility)
              {{function, arity}, ref_visibility}
            end

          {:ok, definitions}

        _ ->
          case backend.debug_info(:erlang_v1, module, data, []) do
            {:ok, abstract_code} ->
              exports =
                for {:attribute, _, :export, exports} <- abstract_code do
                  exports
                end
                |> List.flatten()

              definitions =
                for {:function, _, function, arity, _} <- abstract_code do
                  kind_visibility = if {function, arity} in exports, do: :public, else: :private
                  ref_visibility = ref_visibility(kind_visibility, module_visibility)

                  {{function, arity}, ref_visibility}
                end

              {:ok, definitions}

            _ ->
              :error
          end
      end
    else
      _ -> :error
    end
  end

  defp get_module_and_beam(module) when is_atom(module) do
    case :code.get_object_code(module) do
      {^module, beam, _filename} -> {module, beam}
      :error -> :error
    end
  end

  def ref_visibility(doc_or_visibility)
  def ref_visibility(:hidden), do: :hidden
  def ref_visibility(_), do: :public

  def ref_visibility(kind_visibility, module_visibility)

  def ref_visibility(_, :private),
    do: raise(ArgumentError, "module_visibility cannot be :private. Choose :hidden instead")

  def ref_visibility(:private, _), do: :private
  def ref_visibility(:hidden, _), do: :hidden
  def ref_visibility(_, :hidden), do: :hidden
  def ref_visibility(_, _), do: :public

  defp kind_visibility(kind_or_def_kind)
       when kind_or_def_kind in [
              :def,
              :function,
              :defmacro,
              :macro,
              :type,
              :opaque,
              :defcallback,
              :callback,
              :defmacrocallback,
              :macrocallback
            ],
       do: :public

  defp kind_visibility(kind_or_def_kind)
       when kind_or_def_kind in [
              :defp,
              :defmacrop,
              :typep
            ],
       do: :private
end
