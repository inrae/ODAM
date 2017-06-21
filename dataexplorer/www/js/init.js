/*! highlight.js v9.10.0 | BSD3 License | git.io/hljslicense */
$(document).on("shiny:value", function(event) {
  if (event.target.id === "sessioninfo") { hljs.highlightBlock( document.getElementById("sessioninfo" ) ); }
});
hljs.initHighlightingOnLoad();
