let init = false
let toastTimer = null
let toast = null

export function showToast (message) {
  if (!init) {
    init = true
    toast = document.getElementById('toast')
    toast?.addEventListener('click', () => {
      clearTimeout(toastTimer)
      toast.classList.remove('show')
    })
  }

  if (toast) {
    clearTimeout(toastTimer)
    toast.innerText = message
    toast.classList.add('show')

    toastTimer = setTimeout(() => {
      toast.classList.remove('show')
      toastTimer = setTimeout(function () { toast.innerText = '' }, 1000)
    }, 5000)
  }
}
