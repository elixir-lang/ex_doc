let url = window.location.href
console.log(url)
const cheatsheet = document.getElementById('content');
console.log(cheatsheet);
if (url.indexOf('cheatsheet.html') >= 0) {
  cheatsheet.classList.remove('content-inner')
  cheatsheet.classList.add('content-outer')
}