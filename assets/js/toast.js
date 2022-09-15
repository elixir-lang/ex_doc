let toastTimer = null
const toast = document.getElementById('toast')
toast.addEventListener('click', (event) => {
  clearTimeout(toastTimer)
  event.target.classList.remove('show')
})

export function showToast (message) {
  if (toast) {
    clearTimeout(toastTimer)
    toast.innerText = message
    toast.classList.add('show')

    toastTimer = setTimeout(() => {
      toast.classList.remove('show')
      // wait for transition animation
      toastTimer = setTimeout(function () { toast.innerText = '' }, 1000)
    }, 5000)
  }
}
