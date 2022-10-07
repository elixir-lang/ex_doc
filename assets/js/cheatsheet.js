let urlCheat = window.location.href
console.log(urlCheat)
const cheatsheet = document.getElementById('content');
console.log(cheatsheet);
if (urlCheat.indexOf('cheatsheet.html') >= 0) {
//  cheatsheet.classList.remove('content-inner')
  cheatsheet.classList.add('content-cheatsheet')
}