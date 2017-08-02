defmodule ExDoc.Markdown.MockMarkdownProcessor.MockMarkdownProcessorError do
    @moduledoc false

    defexception message: "You're using a mock markdown processor"
  end

defmodule ExDoc.Markdown.MockMarkdownProcessor do
  @moduledoc false

  # To be used in only in testing

  def to_html(_, _) do
    # When we get an exception, we know this processor's been used
    # This seems to be the easiest way to ensure we're choosing the right processor
    # without instrumenting the code 
    raise ExDoc.Markdown.MockMarkdownProcessor.MockMarkdownProcessorError
  end
end