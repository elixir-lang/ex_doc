;$(function () {
    // Fix outside world links
    function fixOutsideWorldLinks() {
      $('a').each(function() {
        if (window.location.host != this.host) this.target = '_parent';
      });
    }

    fixOutsideWorldLinks();

    // Setup Highlight.js
    hljs.configure({
        tabReplace: '    ', // 4 spaces
        languages: [] // disable auto-detect
    });

    hljs.initHighlighting();
});
