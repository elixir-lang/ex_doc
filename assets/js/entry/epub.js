import { onDocumentReady } from '../helpers'
import { initialize as initMakeup } from '../makeup'

onDocumentReady(() => {
  initMakeup()
})
