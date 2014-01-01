#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "erl_nif.h"

#include "markdown.h"
#include "html.h"
#include "buffer.h"

#define OUTPUT_SIZE 64

struct render_data {
  struct sd_callbacks callbacks;
  void *options;
};

static ERL_NIF_TERM render_term(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[], struct render_data *data) {
  struct sd_markdown *markdown;

  ErlNifBinary markdown_binary;
  ErlNifBinary output_binary;
  struct buf *input, *output;

  if(!enif_inspect_binary(env, argv[0], &markdown_binary)){
    return enif_make_badarg(env);
  }

  if (markdown_binary.size <= 0){
    const char *empty_string = "";
    const int empty_len = strlen(empty_string);
    enif_alloc_binary(empty_len, &output_binary);
    strncpy(output_binary.data, empty_string, empty_len);
    return enif_make_binary(env, &output_binary);
  }

  input = bufnew(markdown_binary.size);
  bufput(input, markdown_binary.data, markdown_binary.size);
  enif_release_binary(&markdown_binary);

  output = bufnew(OUTPUT_SIZE);

  unsigned int extensions = MKDEXT_AUTOLINK | MKDEXT_FENCED_CODE | MKDEXT_TABLES;
  markdown = sd_markdown_new(extensions, 16, &data->callbacks, data->options);
  sd_markdown_render(output, input->data, input->size, markdown);
  sd_markdown_free(markdown);

  enif_alloc_binary(output->size, &output_binary);
  memcpy(output_binary.data, output->data, output->size);

  bufrelease(input);
  bufrelease(output);

  return enif_make_binary(env, &output_binary);
}

static ERL_NIF_TERM to_markdown_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  struct render_data data;
  struct html_renderopt options;

  data.options = &options;
  sdhtml_renderer(&data.callbacks, data.options, 0);

  return render_term(env, argc, argv, &data);
}

static ErlNifFunc nif_funcs[] =
{
    {"to_html", 1, to_markdown_nif}
};

ERL_NIF_INIT(Elixir.Markdown,nif_funcs,NULL,NULL,NULL,NULL);

