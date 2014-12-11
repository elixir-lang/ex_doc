function fixOutsideWorldLinks() {
  $('a').each(function() {
    if (window.location.host != this.host) this.target = '_parent';
  });
}

function generateTOC() {
  if ($('#filecontents').length === 0) return;
  var _toc = $('<ol class="top"></ol>');
  var show = false;
  var toc = _toc;
  var counter = 0;
  var tags = ['h2', 'h3', 'h4', 'h5', 'h6'];
  var i;
  if ($('#filecontents h1').length > 1) tags.unshift('h1');
  for (i = 0; i < tags.length; i++) { tags[i] = '#filecontents ' + tags[i]; }
  var lastTag = parseInt(tags[0][1], 10);
  $(tags.join(', ')).each(function() {
    if (this.id == "filecontents") return;
    show = true;
    var thisTag = parseInt(this.tagName[1], 10);
    if (this.id.length === 0) {
      var proposedId = $(this).text().replace(/[^a-z0-9-]/ig, '_');
      if ($('#' + proposedId).length > 0) { proposedId += counter; counter++; }
      this.id = proposedId;
    }
    if (thisTag > lastTag) { 
      for (i = 0; i < thisTag - lastTag; i++) { 
        var tmp = $('<ol/>'); toc.append(tmp); toc = tmp; 
      } 
    }
    if (thisTag < lastTag) { 
      for (i = 0; i < lastTag - thisTag; i++) toc = toc.parent(); 
    }
    toc.append('<li><a href="#' + this.id + '">' + $(this).text() + '</a></li>');
    lastTag = thisTag;
  });
  if (!show) return;
  html = '<div id="toc"><p class="title"><a class="hide_toc" href="#"><strong>Table of Contents</strong></a> <small>(<a href="#" class="float_toc">left</a>)</small></p></div>';
  $('#content').prepend(html);
  $('#toc').append(_toc);
  $('#toc .hide_toc').toggle(function() { 
    $('#toc .top').slideUp('fast');
    $('#toc').toggleClass('hidden');
    $('#toc .title small').toggle();
  }, function() {
    $('#toc .top').slideDown('fast');
    $('#toc').toggleClass('hidden');
    $('#toc .title small').toggle();
  });
  $('#toc .float_toc').toggle(function() { 
    $(this).text('float');
    $('#toc').toggleClass('nofloat');
  }, function() {
    $(this).text('left');
    $('#toc').toggleClass('nofloat');
  });
}

$(fixOutsideWorldLinks);
$(generateTOC);