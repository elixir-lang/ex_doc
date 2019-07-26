defmodule ExDoc.Translation.Es do
  @moduledoc false

  # Translations for Locale Spanish International
  def t("Anchor for this section"), do: "Ancla para esta sección"
  def t("API Reference"), do: "Referencia de la API"
  def t("Bring up this help dialog"), do: "Abrir este diálogo de ayuda"
  def t("Basic Type"), do: "Tipo Básico"
  def t("Built using"), do: "Construido con"
  def t("Built-in Type"), do: "Tipo Incorporado"
  def t("Callbacks"), do: "Devoluciones de llamada"
  def t("Cancel search"), do: "Cancelar búsqueda"
  def t("Collapse/expand sidebar"), do: "Contraer/expandir barra lateral"
  def t("Cover"), do: "Portada"
  def t("Disable tooltips"), do: "Deshabilitar tooltips"
  def t("Display keyboard shortcuts"), do: "Mostrar combinaciones de teclas"
  def t("Documentation"), do: "Documentación"
  def t("Enable tooltips"), do: "Habilitar tooltips"
  def t("Example"), do: "Ejemplo"
  def t("Examples"), do: "Ejemplos"
  def t("Exceptions"), do: "Excepciones"
  def t("Focus search bar"), do: "Enfocar barra de búsqueda"
  def t("Function"), do: "Función"
  def t("Functions"), do: "Funciones"
  def t("Go to"), do: "Ir a"
  def t("Go to a HexDocs package"), do: "Ir a un paquete HexDocs"

  def t("Here are some tips when performing a full-text search:"),
    do: "Aquí están algunos consejos para realizar una búsqueda textual:"

  def t("Invalid search:"), do: "Búsqueda no válida:"
  def t("Jump to..."), do: "Saltar a"
  def t("Keyboard Shortcuts"), do: "Atajos de Teclado"
  def t("Link"), do: "Enlace"
  def t("Link to this"), do: "Enlace a esto"
  def t("Link to this callback"), do: "Enlace a esta devolución de llamada"
  def t("Link to this function"), do: "Enlace a esta función"
  def t("Link to this section"), do: "Enlace a esta sección"
  def t("Link to this type"), do: "Enlace a este tipo"
  def t("Literal"), do: "Literal"

  def t("\"main\" cannot be set to \"index\", otherwise it will recursively link to itself"),
    do:
      "\"main\" no se puede configurar como \"index\", ya que de esta manera se establecerá un link recursivo a el mismo"

  def t("image format not recognized, allowed formats are: .jpg, .png"),
    do: "no se reconoce el formato de la imagen, los formatos permitidos son: .jpg, .png"

  def t("file format not recognized, allowed format is: .md"),
    do: "no se reconoce el formato del archivo, el formato permitido es: .md"

  def t("Mix Tasks"), do: "Tareas de Mix"
  def t("Modules"), do: "Módulos"
  def t("Option"), do: "Opción"
  def t("Options"), do: "Opciones"
  def t("Page not found"), do: "Página no encontrada"
  def t("Pages"), do: "Páginas"
  def t("Search"), do: "Buscar"
  def t("Search..."), do: "Buscar..."
  def t("Search results for"), do: "Resultados de la búsqueda para"
  def t("Search the documentation"), do: "Buscar en la documentación"

  def t("Sorry, but the page you were trying to get to, does not exist."),
    do: "Lo sentimos, pero la página a la que estás intentando acceder no existe."

  def t("Sorry, we couldn't find anything for"), do: "Lo sentimos, no se pudo encontrar nada para"
  def t("Summary"), do: "Resumen"
  def t("Table of contents"), do: "Tabla de contenidos"
  def t("This is deprecated"), do: "Esto quedó obsoleto"
  def t("This callback is deprecated"), do: "Esta devolución de llamada quedó obsoleta"
  def t("This function is deprecated"), do: "Esta función quedó obsoleta"
  def t("This type is deprecated"), do: "Este tipo quedó obsoleto"

  def t("Multiple words (such as <code>foo bar</code>) are searched as <code>OR</code>"),
    do: "Multiples palabras (tales como <code>foo bar</code> son buscadas como <code>O</code>)"

  def t("Use <code>*</code> anywhere (such as <code>fo*</code>) as wildcard"),
    do: "Utilice <code>*</code> en cualquier parte (por ejemplo: <code>fo*</code>) como comodín"

  def t(
        "Use <code>+</code> before a word (such as <code>+foo</code>) to make its presence required"
      ),
      do:
        "Utilice <code>+</code> antes de una palabra (por ejemplo: <code>+foo</code>) para hacer la palabra requerida"

  def t(
        "Use <code>-</code> before a word (such as <code>-foo</code>) to make its absence required"
      ),
      do:
        "Utilice <code>-</code> antes de una palabra (por ejemplo: <code>-foo</code>) para hacer la palabra no requerida"

  def t("Use <code>WORD^NUMBER</code> (such as <code>foo^2</code>) to boost the given word"),
    do:
      "Utilice <code>PALABRA^NÚMERO</code> (por ejemplo: <code>foo^2</code>) para enfatizar la palabra indicada"

  def t(
        "Use <code>WORD~NUMBER</code> (such as <code>foo~2</code>) to do a search with edit distance on word"
      ),
      do:
        "Utilice <code>PALABRA~NÚMERO</code> (por ejemplo: <code>foo~2</code>) para hacer una búsqueda con distancia de edición de la palabra"

  def t(
        "To quickly go to a module, type, or function, use the autocompletion feature in the sidebar search."
      ),
      do:
        "Para rápidamente acceder a un módulo, tipo o función, usa la opción de autocompletado en la búsqueda de la barra lateral"

  def t("Toggle night mode"), do: "Alternar modo noche"
  def t("Toggle sidebar"), do: "Alternar barra lateral"
  def t("Top"), do: "Ir a la parte superior"
  def t("Type"), do: "Tipo"
  def t("Types"), do: "Tipos"
  def t("View Source"), do: "Ver Código Fuente"

  def t(
        "You may want to try searching this site using the sidebar to find what you were looking for."
      ),
      do:
        "Quizás quieras intentar buscar este sitio usando la barra lateral para encontrar lo que buscas."

  def t("annotation"), do: "anotación"
  def t("author"), do: "autor"
  def t("autocomplete"), do: "autocompletar"
  def t("behaviour"), do: "comportamiento"
  def t("callback"), do: "devolución de llamada"
  def t("context"), do: "contexto"
  def t("contexts"), do: "contextos"
  def t("deprecated"), do: "obsoleto"
  def t("designed by"), do: "diseñado por"
  def t("detail"), do: "detalle"
  def t("details"), do: "detalles"
  def t("example"), do: "ejemplo"
  def t("examples"), do: "ejemplos"
  def t("exception"), do: "excepción"
  def t("exceptions"), do: "excepciones"
  def t("footer"), do: "pie de página"
  def t("extras"), do: "extras"
  def t("function"), do: "función"
  def t("functions"), do: "funciones"
  def t("group"), do: "grupo"
  def t("header"), do: "cabecera"
  def t("headers"), do: "cabeceras"
  def t("heading"), do: "encabezamiento"
  def t("help"), do: "ayuda"
  def t("impl"), do: "implementación"
  def t("list"), do: "lista"
  def t("link"), do: "enlace"
  def t("loading"), do: "cargando"
  def t("macro"), do: "macro"
  def t("module"), do: "módulo"
  def t("modules"), do: "módulos"
  def t("name"), do: "nombre"
  def t("night mode"), do: "modo noche"
  def t("node"), do: "nodo"
  def t("nodes"), do: "nodos"
  def t("opaque"), do: "opaco"
  def t("option"), do: "opción"
  def t("optional"), do: "opcional"
  def t("options"), do: "opciones"
  def t("project"), do: "proyecto"
  def t("protocol"), do: "protocolo"
  def t("search"), do: "buscar"
  def t("sidebar"), do: "barra lateral"
  def t("since " <> version_number), do: "desde " <> version_number
  def t("spec"), do: "especificación"
  def t("struct"), do: "estructura"
  def t("summary"), do: "resumen"
  def t("synopsis"), do: "sinopsis"
  def t("tag"), do: "etiqueta"
  def t("tags"), do: "etiquetas"
  def t("task"), do: "tarea"
  def t("tasks"), do: "tareas"
  def t("title"), do: "título"
  def t("type"), do: "tipo"
  def t("types"), do: "tipos"
  def t("version"), do: "versión"

  # Catch-all clause
  def t(text),
    do: text

  # Text with arguments
  def t(
        "You may want to try searching this site using the sidebar or using our page <a href=\"api-reference.html\">\#{api_reference}</a> to find what you were looking for.",
        api_reference: api_reference
      ),
      do:
        "Quizás quieras intentar buscar este sitio usando la barra lateral o utilizar nuestra página <a href=\"api-reference.html\">#{
          api_reference
        }</a> para encontrar lo que buscas."

  def t("warning: \#{filename} redirects to \#{redirect_to}, which does not exist",
        filename: filename,
        redirect_to: redirect_to
      ),
      do: "advertencia #{filename} redirige a #{redirect_to} el cual no existe"

  # Catch-all clause
  def t(text, args),
    do: ExDoc.Translation.replace(text, args)
end
