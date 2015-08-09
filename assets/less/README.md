# Styles for ExDoc

## Document Structure (see `module_template.eex`)

```
body
  section#sidebar [.in_search]
    h1#full_list_header
    h2#sub_list_header
    div#nav
    div#search [.loading] > input#search_field
    ul#full_list
      li.node [.collpased, .search_uncollapsed, .found]
        a.toggle
        a.object_link
        span.node_name
      li.docs [.collpased, .search_uncollapsed, .found]
        a.toggle
        a.object_link
        span.node_name
        ...
    div.no_results

  section#content
    div.breadcrumbs
    h1
      small
    ul.summary_links
      li > a
      ...
    section.docstring#moduledoc
    a.view_source

    section.details_list#summary_details
      h1
      table.summary
        tr
          td.summary_signature > a
          td.summary_synopsis > p
        ...

    section.details_list#types_details
      h1
      div.type_detail
        p.typespec > a
      ...

    section.details_list#functions_details
      h1
      section.detail
        div.detail_header
          span.signature > strong
        div.detail_header_links
          span.detail_type
          a.detail_link
          a.to_top_link
          ul.spec
            li > a
            ...
        section.docstring
        a.view_source
      ...

```
