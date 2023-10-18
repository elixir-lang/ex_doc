import { onDocumentReady } from '../helpers'
import { fixBlockquotes } from '../content'
import { initialize as initMakeup } from '../makeup'

onDocumentReady(() => {
  initMakeup()
  fixBlockquotes()
})
