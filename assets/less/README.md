# Styles for ExDoc

## Document Structure (see `module_template.eex`)

```
body
  section#sidebar [.in-search]
    h1#full_list_header
    h2#sub_list_header
    div#nav
    div#search [.loading] > input#search_field
    ul#full_list
      li.node [.collapsed, .search-uncollapsed, .found]
        a.object-link
        span.node-name
      li.docs [.collapsed, .search-uncollapsed, .found]
        a.object-link
        span.node-name
        ...
    div.no-results

  section
    div.breadcrumbs
    h1
      small
    ul.summary-links
      li > a
      ...
    section.docstring#moduledoc
    a.view-source

    section.details-list#summary_details
      h1
      table.summary
        tr
          td.summary-signature > a
          td.summary-synopsis > p
        ...

    section.details-list#types_details
      h1
      div.type-detail
        p.typespec > a
      ...

    section.details-list#functions_details
      h1
      section.detail
        div.detail-header
          span.signature > strong
        div.detail-header-links
          span.detail-type
          a.detail-link
          a.to-top-link
          ul.spec
            li > a
            ...
        section.docstring
        a.view-source
      ...

```
