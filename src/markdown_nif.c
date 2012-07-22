#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "erl_nif.h"

#include "markdown.h"
#include "html.h"
#include "buffer.h"

#define OUTPUT_SIZE 64

static ERL_NIF_TERM to_markdown_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  struct sd_markdown *markdown;
  struct sd_callbacks callbacks;
  struct html_renderopt options;

  ErlNifBinary markdown_binary;
  ErlNifBinary output_binary;
  struct buf *input, *output;

  if(!enif_inspect_binary(env, argv[0], &markdown_binary)){
    return enif_make_badarg(env);
  }

  input = bufnew(markdown_binary.size);
  bufput(input, markdown_binary.data, markdown_binary.size);
  enif_release_binary(&markdown_binary);

  output = bufnew(OUTPUT_SIZE);

  sdhtml_renderer(&callbacks, &options, 0);
  markdown = sd_markdown_new(MKDEXT_AUTOLINK, 16, &callbacks, &options);

  sd_markdown_render(output, input->data, input->size, markdown);
  sd_markdown_free(markdown);

  enif_alloc_binary(output->size, &output_binary);
  memcpy(output_binary.data, output->data, output->size);

  bufrelease(input);
  bufrelease(output);

  return enif_make_binary(env, &output_binary);
}

static ErlNifFunc nif_funcs[] =
{
    {"to_html", 1, to_markdown_nif}
};

ERL_NIF_INIT(Elixir-Markdown,nif_funcs,NULL,NULL,NULL,NULL);

