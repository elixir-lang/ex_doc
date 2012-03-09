#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "erl_nif.h"

#include "markdown.h"
#include "html.h"
#include "buffer.h"

#define OUTPUT_SIZE 64 

struct sd_callbacks callbacks;
struct sd_markdown *markdown;
struct html_renderopt options;

static ERL_NIF_TERM to_markdown_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  ErlNifBinary markdown_binary;
  ErlNifBinary output_binary;
  struct buf *input, *output;

  if(!enif_inspect_binary(env, argv[0], &markdown_binary)){
    enif_make_badarg(env);
  }
  input = bufnew(markdown_binary.size);
  bufputs(input, markdown_binary.data);

  output = bufnew(OUTPUT_SIZE);

  sdhtml_renderer(&callbacks, &options, 0);
  markdown = sd_markdown_new(0, 16, &callbacks, &options);
  sd_markdown_render(output, input->data, input->size, markdown);

  enif_alloc_binary(sizeof(char)*(output->size), &output_binary);
  memset(output_binary.data, 0, output->size);
  strcpy(output_binary.data, output->data); 

  bufrelease(input);
  bufrelease(output);

  return enif_make_binary(env, &output_binary);
}

static ErlNifFunc nif_funcs[] =
{
    {"to_html", 1, to_markdown_nif}
};

ERL_NIF_INIT(markdown,nif_funcs,NULL,NULL,NULL,NULL);

