defmodule ExDoc.Translation do
  @locale_spanish_international "es"

  def translate(config, text), do: t(config.language, text)

  def t(locale, text) when is_atom(text),
    do: t(locale, Atom.to_string(text))

  # Translations for Locale Spanish International

  def t(@locale_spanish_international, "Anchor for this section"), do: "Ancla para esta sección"
  def t(@locale_spanish_international, "API Reference"), do: "Referencia de la API"

  def t(@locale_spanish_international, "Bring up this help dialog"),
    do: "Abrir este diálogo de ayuda"

  def t(@locale_spanish_international, "Built using"), do: "Construido con"
  def t(@locale_spanish_international, "Callbacks"), do: "Devoluciones de llamada"
  def t(@locale_spanish_international, "Cancel search"), do: "Cancelar búsqueda"

  def t(@locale_spanish_international, "Collapse/expand sidebar"),
    do: "Contraer/expandir barra lateral"

  def t(@locale_spanish_international, "Contributing"), do: "Como contribuir"
  def t(@locale_spanish_international, "Cover"), do: "Portada"

  def t(@locale_spanish_international, "Disable tooltips"), do: "Deshabilitar tooltips"

  def t(@locale_spanish_international, "Display keyboard shortcuts"),
    do: "Mostrar combinaciones de teclas"

  def t(@locale_spanish_international, "Documentation"), do: "Documentación"
  def t(@locale_spanish_international, "Enable tooltips"), do: "Habilitar tooltips"
  def t(@locale_spanish_international, "Example"), do: "Ejemplo"
  def t(@locale_spanish_international, "Examples"), do: "Ejemplos"
  def t(@locale_spanish_international, "Exceptions"), do: "Excepciones"
  def t(@locale_spanish_international, "Focus search bar"), do: "Enfocar barra de búsqueda"
  def t(@locale_spanish_international, "Function"), do: "Función"
  def t(@locale_spanish_international, "Functions"), do: "Funciones"
  def t(@locale_spanish_international, "Go to"), do: "Ir a"
  def t(@locale_spanish_international, "Go to a HexDocs package"), do: "Ir a un paquete HexDocs"

  def t(
        @locale_spanish_international,
        "Here are some tips when performing a full-text search:"
      ),
      do: "Aquí están algunos consejos para realizar una búsqueda textual:"

  def t(@locale_spanish_international, "Invalid search:"), do: "Búsqueda no válida:"

  def t(@locale_spanish_international, "Jump to"), do: "Saltar a"

  def t(@locale_spanish_international, "Keyboard Shortcuts"),
    do: "Combinaciones de Teclas"

  def t(@locale_spanish_international, "License"), do: "Licenciamiento"
  def t(@locale_spanish_international, "Link"), do: "Enlace"
  def t(@locale_spanish_international, "Link to this"), do: "Enlace a esto"

  def t(@locale_spanish_international, "Link to this callback"),
    do: "Enlace a esta devolución de llamada"

  def t(@locale_spanish_international, "Link to this function"),
    do: "Enlace a esta función"

  def t(@locale_spanish_international, "Link to this section"),
    do: "Enlace a esta sección"

  def t(@locale_spanish_international, "Link to this type"),
    do: "Enlace a este tipo"

  def t(
        @locale_spanish_international,
        "\"main\" cannot be set to \"index\", otherwise it will recursively link to itself"
      ),
      do:
        "\"main\" no se puede configurar como \"index\", ya que de esta manera se establecerá un link recursivo a el mismo"

  def t(
        @locale_spanish_international,
        "image format not recognized, allowed formats are: .jpg, .png"
      ),
      do: "No se reconoce el formato de la imagen, los formatos permitidos son: .jpg, .png"

  def t(
        @locale_spanish_international,
        "file format not recognized, allowed format is: .md"
      ),
      do: "No se reconoce el formato del archivo, el formato permitido es: .md"

  def t(@locale_spanish_international, "Mix Tasks"), do: "Tareas de Mix"
  def t(@locale_spanish_international, "Modules"), do: "Módulos"

  def t(@locale_spanish_international, "Multiple words (such as"),
    do: "Palabras múltiples (tales como"

  def t(@locale_spanish_international, "Option"), do: "Opción"
  def t(@locale_spanish_international, "Options"), do: "Opciones"
  def t(@locale_spanish_international, "Page not found"), do: "Página no encontrada"
  def t(@locale_spanish_international, "Pages"), do: "Páginas"
  def t(@locale_spanish_international, "Search"), do: "Buscar"
  def t(@locale_spanish_international, "Search..."), do: "Buscar..."

  def t(@locale_spanish_international, "Search results for"),
    do: "Resultados de la búsqueda para"

  def t(@locale_spanish_international, "Search the documentation"),
    do: "Buscar en la documentación"

  def t(
        @locale_spanish_international,
        "Sorry, but the page you were trying to get to, does not exist."
      ),
      do: "Lo sentimos, pero la página a la que estás intentando acceder no existe."

  def t(@locale_spanish_international, "Sorry, we couldn't find anything for"),
    do: "Lo sentimos, no se pudo encontrar nada para"

  def t(@locale_spanish_international, "Summary"), do: "Resumen"
  def t(@locale_spanish_international, "Table of contents"), do: "Tabla de contenidos"
  def t(@locale_spanish_international, "This"), do: "Este"

  def t(@locale_spanish_international, "This is deprecated"), do: "Esto quedó obsoleto"

  def t(@locale_spanish_international, "This callback is deprecated"),
    do: "Esta devolución de llamada quedó obsoleta"

  def t(@locale_spanish_international, "This function is deprecated"),
    do: "Esta función quedó obsoleta"

  def t(@locale_spanish_international, "This type is deprecated"), do: "Este tipo quedó obsoleto"

  def t(
        @locale_spanish_international,
        "Multiple words (such as <code>foo bar</code>) are searched as <code>OR</code>"
      ),
      do: "Multiples palabras (tales como <code>foo bar</code> son buscadas como <code>O</code>)"

  def t(
        @locale_spanish_international,
        "Use <code>*</code> anywhere (such as <code>fo*</code>) as wildcard"
      ),
      do: "Utilice <code>*</code> en cualquier parte (por ejemplo: <code>fo*</code>) como comodín"

  def t(
        @locale_spanish_international,
        "Use <code>+</code> before a word (such as <code>+foo</code>) to make its presence required"
      ),
      do:
        "Utilice <code>+</code> antes de una palabra (por ejemplo: <code>+foo</code>) para hacer la palabra requerida"

  def t(
        @locale_spanish_international,
        "Use <code>-</code> before a word (such as <code>-foo</code>) to make its absence required"
      ),
      do:
        "Utilice <code>-</code> antes de una palabra (por ejemplo: <code>-foo</code>) para hacer la palabra no requerida"

  def t(
        @locale_spanish_international,
        "Use <code>WORD^NUMBER</code> (such as <code>foo^2</code>) to boost the given word"
      ),
      do:
        "Utilice <code>PALABRA^NÚMERO</code> (por ejemplo: <code>foo^2</code>) para enfatizar la palabra indicada"

  def t(
        @locale_spanish_international,
        "Use <code>WORD~NUMBER</code> (such as <code>foo~2</code>) to do a search with edit distance on word"
      ),
      do:
        "Utilice <code>PALABRA~NÚMERO</code> (por ejemplo: <code>foo~2</code>) para hacer una busqueda con distancia de edicion de la palabra"

  def t(
        @locale_spanish_international,
        "To quickly go to a module, type, or function, use the autocompletion feature in the sidebar search."
      ),
      do:
        "Para rápidamente acceder a un módulo, tipo o función, usa la opción de autocompletado en la búsqueda de la barra lateral"

  def t(@locale_spanish_international, "Toggle night mode"), do: "Alternar modo noche"
  def t(@locale_spanish_international, "Toggle sidebar"), do: "Alternar barra lateral"
  def t(@locale_spanish_international, "Top"), do: "Ir a la parte superior"
  def t(@locale_spanish_international, "Type"), do: "Tipo"
  def t(@locale_spanish_international, "Types"), do: "Tipos"
  def t(@locale_spanish_international, "Use"), do: "Usa"
  def t(@locale_spanish_international, "View Source"), do: "Ver Código Fuente"

  def t(
        @locale_spanish_international,
        "You may want to try searching this site using the sidebar"
      ),
      do: "Quizás quieras intentar buscar este sitio usando la barra lateral"

  def t(@locale_spanish_international, "annotation"), do: "anotación"

  def t(@locale_spanish_international, "anywhere (such as"),
    do: "en cualquier lugar (tal como"

  def t(@locale_spanish_international, "author"), do: "autor"
  def t(@locale_spanish_international, "autocomplete"), do: "autocompletar"

  def t(@locale_spanish_international, "before a word (such as"),
    do: "antes de una palabra (tal como"

  def t(@locale_spanish_international, "behaviour"), do: "comportamiento"
  def t(@locale_spanish_international, "callback"), do: "devolución de llamada"
  def t(@locale_spanish_international, "context"), do: "contexto"
  def t(@locale_spanish_international, "contexts"), do: "contextos"
  def t(@locale_spanish_international, "deprecated"), do: "obsoleto"
  def t(@locale_spanish_international, "designed by"), do: "diseñado por"
  def t(@locale_spanish_international, "detail"), do: "detalle"
  def t(@locale_spanish_international, "details"), do: "detalles"
  def t(@locale_spanish_international, "example"), do: "ejemplo"
  def t(@locale_spanish_international, "examples"), do: "ejemplos"
  def t(@locale_spanish_international, "exception"), do: "excepción"
  def t(@locale_spanish_international, "exceptions"), do: "excepciones"
  def t(@locale_spanish_international, "footer"), do: "pie de página"
  def t(@locale_spanish_international, "extras"), do: "extras"
  def t(@locale_spanish_international, "function"), do: "función"
  def t(@locale_spanish_international, "functions"), do: "funciones"
  def t(@locale_spanish_international, "group"), do: "grupo"
  def t(@locale_spanish_international, "header"), do: "cabecera"
  def t(@locale_spanish_international, "headers"), do: "cabeceras"
  def t(@locale_spanish_international, "heading"), do: "encabezamiento"
  def t(@locale_spanish_international, "help"), do: "ayuda"
  def t(@locale_spanish_international, "is deprecated."), do: "está obsoleto."
  def t(@locale_spanish_international, "impl"), do: "implementación"
  def t(@locale_spanish_international, "list"), do: "lista"
  def t(@locale_spanish_international, "link"), do: "enlace"
  def t(@locale_spanish_international, "loading"), do: "cargando"
  def t(@locale_spanish_international, "macro"), do: "macro"
  def t(@locale_spanish_international, "module"), do: "módulo"
  def t(@locale_spanish_international, "modules"), do: "módulos"
  def t(@locale_spanish_international, "name"), do: "nombre"
  def t(@locale_spanish_international, "night mode"), do: "modo noche"
  def t(@locale_spanish_international, "node"), do: "nodo"
  def t(@locale_spanish_international, "nodes"), do: "nodos"
  def t(@locale_spanish_international, "opaque"), do: "opaco"
  def t(@locale_spanish_international, "option"), do: "opción"
  def t(@locale_spanish_international, "optional"), do: "opcional"
  def t(@locale_spanish_international, "options"), do: "opciones"

  def t(@locale_spanish_international, "or using our page"),
    do: "o utilizar nuestra página"

  def t(@locale_spanish_international, "project"), do: "proyecto"
  def t(@locale_spanish_international, "protocol"), do: "protocolo"
  def t(@locale_spanish_international, "search"), do: "buscar"
  # def t(@locale_spanish_international, "section"), do: "sección"

  # def t(@locale_spanish_international, "selected disabled"),
  #   do: "seleccionado deshabilitado"

  def t(@locale_spanish_international, "sidebar"), do: "barra lateral"
  def t(@locale_spanish_international, "since " <> version_number), do: "desde " <> version_number
  def t(@locale_spanish_international, "spec"), do: "especificación"
  def t(@locale_spanish_international, "struct"), do: "estructura"
  def t(@locale_spanish_international, "summary"), do: "resumen"
  def t(@locale_spanish_international, "synopsis"), do: "sinopsis"
  def t(@locale_spanish_international, "tag"), do: "etiqueta"
  def t(@locale_spanish_international, "tags"), do: "etiquetas"
  def t(@locale_spanish_international, "task"), do: "tarea"
  def t(@locale_spanish_international, "tasks"), do: "tareas"
  def t(@locale_spanish_international, "title"), do: "título"

  def t(@locale_spanish_international, "to find what you were looking for."),
    do: "para encontrar lo que buscas."

  def t(@locale_spanish_international, "type"), do: "tipo"
  def t(@locale_spanish_international, "types"), do: "tipos"
  def t(@locale_spanish_international, "version"), do: "versión"
  def t(@locale_spanish_international, "(such as"), do: "(tal como"
  def t(@locale_spanish_international, ") are searched as"), do: ") se buscan como"
  def t(@locale_spanish_international, ") as wildcard"), do: ") como comodín"
  def t(@locale_spanish_international, "redirects to"), do: "redirige a"
  def t(@locale_spanish_international, "warning"), do: "advertencia"
  def t(@locale_spanish_international, "which does not exist"), do: "el cual no existe"

  def t(@locale_spanish_international, ") to boost the given word"),
    do: ") para promover la palabra dada"

  def t(@locale_spanish_international, ") to do a search with edit distance on word"),
    do: ") para hacer una búsqueda con el nivel de similaridad de palabra indicado"

  def t(@locale_spanish_international, ") to make its absence required"),
    do: ") para hacer su ausencia obligatoria"

  def t(@locale_spanish_international, ") to make its presence required"),
    do: ") para hacer su presencia obligatoria"

  # More locales ...

  # Catch-all clause
  def t(_, text), do: text
end
