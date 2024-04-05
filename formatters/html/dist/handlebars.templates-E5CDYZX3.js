(()=>{(function(){var d=Handlebars.template,y=Handlebars.templates=Handlebars.templates||{};y["autocomplete-suggestions"]=d({1:function(e,l,a,p,r){var o,n,s=l??(e.nullContext||{}),u=e.hooks.helperMissing,i="function",c=e.escapeExpression,t=e.lookupProperty||function(f,m){if(Object.prototype.hasOwnProperty.call(f,m))return f[m]};return'      <a href="'+c((n=(n=t(a,"link")||(l!=null?t(l,"link"):l))!=null?n:u,typeof n===i?n.call(s,{name:"link",hash:{},data:r,loc:{start:{line:15,column:15},end:{line:15,column:23}}}):n))+'" class="autocomplete-suggestion" data-index="'+c((n=(n=t(a,"index")||r&&t(r,"index"))!=null?n:u,typeof n===i?n.call(s,{name:"index",hash:{},data:r,loc:{start:{line:15,column:69},end:{line:15,column:79}}}):n))+`" tabindex="-1">
        <div class="title">
`+((o=t(a,"if").call(s,l!=null?t(l,"deprecated"):l,{name:"if",hash:{},fn:e.program(2,r,0),inverse:e.program(4,r,0),data:r,loc:{start:{line:17,column:10},end:{line:21,column:17}}}))!=null?o:"")+`
`+((o=t(a,"each").call(s,l!=null?t(l,"labels"):l,{name:"each",hash:{},fn:e.program(6,r,0),inverse:e.noop,data:r,loc:{start:{line:23,column:10},end:{line:25,column:19}}}))!=null?o:"")+`          <div class="autocomplete-suggestion-preview-indicator autocomplete-suggestion-preview-indicator-open">
            <button type="button">
              <span class="ri-arrow-left-s-line" /> Close Preview
            </button>
          </div>
          <div class="autocomplete-suggestion-preview-indicator autocomplete-suggestion-preview-indicator-closed">
            <button type="button">
              Open Preview <span class="ri-arrow-right-s-line" />
            </button>
          </div>
        </div>

`+((o=t(a,"if").call(s,l!=null?t(l,"description"):l,{name:"if",hash:{},fn:e.program(8,r,0),inverse:e.noop,data:r,loc:{start:{line:38,column:8},end:{line:42,column:15}}}))!=null?o:"")+`      </a>
`},2:function(e,l,a,p,r){var o,n,s=e.lookupProperty||function(u,i){if(Object.prototype.hasOwnProperty.call(u,i))return u[i]};return'          <s><span class="header" translate="no">'+((o=(n=(n=s(a,"title")||(l!=null?s(l,"title"):l))!=null?n:e.hooks.helperMissing,typeof n=="function"?n.call(l??(e.nullContext||{}),{name:"title",hash:{},data:r,loc:{start:{line:18,column:49},end:{line:18,column:60}}}):n))!=null?o:"")+`</span></s>
`},4:function(e,l,a,p,r){var o,n,s=e.lookupProperty||function(u,i){if(Object.prototype.hasOwnProperty.call(u,i))return u[i]};return'          <span class="header" translate="no">'+((o=(n=(n=s(a,"title")||(l!=null?s(l,"title"):l))!=null?n:e.hooks.helperMissing,typeof n=="function"?n.call(l??(e.nullContext||{}),{name:"title",hash:{},data:r,loc:{start:{line:20,column:46},end:{line:20,column:57}}}):n))!=null?o:"")+`</span>
`},6:function(e,l,a,p,r){return'          <span class="label">'+e.escapeExpression(e.lambda(l,l))+`</span>
`},8:function(e,l,a,p,r){var o,n,s=e.lookupProperty||function(u,i){if(Object.prototype.hasOwnProperty.call(u,i))return u[i]};return`        <div class="description" translate="no">
          `+((o=(n=(n=s(a,"description")||(l!=null?s(l,"description"):l))!=null?n:e.hooks.helperMissing,typeof n=="function"?n.call(l??(e.nullContext||{}),{name:"description",hash:{},data:r,loc:{start:{line:40,column:10},end:{line:40,column:27}}}):n))!=null?o:"")+`
        </div>
`},compiler:[8,">= 4.3.0"],main:function(e,l,a,p,r){var o,n,s=l??(e.nullContext||{}),u=e.lookupProperty||function(i,c){if(Object.prototype.hasOwnProperty.call(i,c))return i[c]};return`<div class="triangle"></div>
<div class="autocomplete-container">
  <div class="autocomplete-suggestions">
    <div class="autocomplete-results">
      <span>
        Autocompletion results for <span class="bold">"`+e.escapeExpression((n=(n=u(a,"term")||(l!=null?u(l,"term"):l))!=null?n:e.hooks.helperMissing,typeof n=="function"?n.call(s,{name:"term",hash:{},data:r,loc:{start:{line:6,column:55},end:{line:6,column:63}}}):n))+`"</span>
      </span>
      <span class="press-return">
        Press <span class="bold">RETURN</span> for full-text search, <span class="bold">\u2192</span> for expand previews,
        <span class="bold">\u2190</span> for close previews
      </span>
    </div>
    <div>
`+((o=u(a,"each").call(s,l!=null?u(l,"suggestions"):l,{name:"each",hash:{},fn:e.program(1,r,0),inverse:e.noop,data:r,loc:{start:{line:14,column:6},end:{line:44,column:15}}}))!=null?o:"")+`    </div>
  </div>
</div>`},useData:!0}),y["modal-layout"]=d({compiler:[8,">= 4.3.0"],main:function(e,l,a,p,r){return`<div id="modal" class="modal" tabindex="-1">
  <div class="modal-contents">
    <div class="modal-header">
      <div class="modal-title"></div>
      <button class="modal-close" aria-label="close">\xD7</button>
    </div>
    <div class="modal-body">
    </div>
  </div>
</div>
`},useData:!0}),y["quick-switch-modal-body"]=d({compiler:[8,">= 4.3.0"],main:function(e,l,a,p,r){return`<div id="quick-switch-modal-body">
  <i class="ri-search-2-line" aria-hidden="true"></i>
  <input type="text" id="quick-switch-input" class="search-input" placeholder="Jump to..." autocomplete="off" spellcheck="false">
  <div id="quick-switch-results"></div>
</div>
`},useData:!0}),y["quick-switch-results"]=d({1:function(e,l,a,p,r){var o,n=l??(e.nullContext||{}),s=e.hooks.helperMissing,u="function",i=e.escapeExpression,c=e.lookupProperty||function(t,f){if(Object.prototype.hasOwnProperty.call(t,f))return t[f]};return'  <div class="quick-switch-result" data-index="'+i((o=(o=c(a,"index")||r&&c(r,"index"))!=null?o:s,typeof o===u?o.call(n,{name:"index",hash:{},data:r,loc:{start:{line:2,column:47},end:{line:2,column:57}}}):o))+`">
    `+i((o=(o=c(a,"name")||(l!=null?c(l,"name"):l))!=null?o:s,typeof o===u?o.call(n,{name:"name",hash:{},data:r,loc:{start:{line:3,column:4},end:{line:3,column:12}}}):o))+`
  </div>
`},compiler:[8,">= 4.3.0"],main:function(e,l,a,p,r){var o,n=e.lookupProperty||function(s,u){if(Object.prototype.hasOwnProperty.call(s,u))return s[u]};return(o=n(a,"each").call(l??(e.nullContext||{}),l!=null?n(l,"results"):l,{name:"each",hash:{},fn:e.program(1,r,0),inverse:e.noop,data:r,loc:{start:{line:1,column:0},end:{line:5,column:9}}}))!=null?o:""},useData:!0}),y["search-results"]=d({1:function(e,l,a,p,r){var o,n=e.lookupProperty||function(s,u){if(Object.prototype.hasOwnProperty.call(s,u))return s[u]};return"    Search results for <em>"+e.escapeExpression((o=(o=n(a,"value")||(l!=null?n(l,"value"):l))!=null?o:e.hooks.helperMissing,typeof o=="function"?o.call(l??(e.nullContext||{}),{name:"value",hash:{},data:r,loc:{start:{line:3,column:27},end:{line:3,column:36}}}):o))+`</em>
`},3:function(e,l,a,p,r){return`    Invalid search
`},5:function(e,l,a,p,r){var o,n=e.lookupProperty||function(s,u){if(Object.prototype.hasOwnProperty.call(s,u))return s[u]};return(o=n(a,"each").call(l??(e.nullContext||{}),l!=null?n(l,"results"):l,{name:"each",hash:{},fn:e.program(6,r,0),inverse:e.noop,data:r,loc:{start:{line:10,column:2},end:{line:21,column:11}}}))!=null?o:""},6:function(e,l,a,p,r){var o,n=e.lambda,s=e.escapeExpression,u=e.lookupProperty||function(i,c){if(Object.prototype.hasOwnProperty.call(i,c))return i[c]};return`    <div class="result">
      <h2 class="result-id">
        <a href="`+s(n(l!=null?u(l,"ref"):l,l))+`">
          <span translate="no">`+s(n(l!=null?u(l,"title"):l,l))+"</span> <small>("+s(n(l!=null?u(l,"type"):l,l))+`)</small>
        </a>
      </h2>
`+((o=u(a,"each").call(l??(e.nullContext||{}),l!=null?u(l,"excerpts"):l,{name:"each",hash:{},fn:e.program(7,r,0),inverse:e.noop,data:r,loc:{start:{line:17,column:8},end:{line:19,column:17}}}))!=null?o:"")+`    </div>
`},7:function(e,l,a,p,r){var o;return'          <p class="result-elem">'+((o=e.lambda(l,l))!=null?o:"")+`</p>
`},9:function(e,l,a,p,r){var o,n=e.lookupProperty||function(s,u){if(Object.prototype.hasOwnProperty.call(s,u))return s[u]};return((o=(n(a,"isArray")||l&&n(l,"isArray")||e.hooks.helperMissing).call(l??(e.nullContext||{}),l!=null?n(l,"results"):l,{name:"isArray",hash:{},fn:e.program(10,r,0),inverse:e.program(12,r,0),data:r,loc:{start:{line:23,column:2},end:{line:29,column:14}}}))!=null?o:"")+`
  <p>The search functionality is full-text based. Here are some tips:</p>

  <ul>
    <li>Multiple words (such as <code>foo bar</code>) are searched as <code>OR</code></li>
    <li>Use <code>*</code> anywhere (such as <code>fo*</code>) as wildcard</li>
    <li>Use <code>+</code> before a word (such as <code>+foo</code>) to make its presence required</li>
    <li>Use <code>-</code> before a word (such as <code>-foo</code>) to make its absence required</li>
    <li>Use <code>:</code> to search on a particular field (such as <code>field:word</code>). The available fields are <code>title</code>, <code>doc</code> and <code>type</code></li>
    <li>Use <code>WORD^NUMBER</code> (such as <code>foo^2</code>) to boost the given word</li>
    <li>Use <code>WORD~NUMBER</code> (such as <code>foo~2</code>) to do a search with edit distance on word</li>
  </ul>

  <p>To quickly go to a module, type, or function, use the autocompletion feature in the sidebar search.</p>
`},10:function(e,l,a,p,r){var o,n=e.lookupProperty||function(s,u){if(Object.prototype.hasOwnProperty.call(s,u))return s[u]};return"    <p>Sorry, we couldn't find anything for <em>"+e.escapeExpression((o=(o=n(a,"value")||(l!=null?n(l,"value"):l))!=null?o:e.hooks.helperMissing,typeof o=="function"?o.call(l??(e.nullContext||{}),{name:"value",hash:{},data:r,loc:{start:{line:24,column:48},end:{line:24,column:57}}}):o))+`</em>.</p>
`},12:function(e,l,a,p,r){var o,n=e.lookupProperty||function(s,u){if(Object.prototype.hasOwnProperty.call(s,u))return s[u]};return(o=n(a,"if").call(l??(e.nullContext||{}),l!=null?n(l,"value"):l,{name:"if",hash:{},fn:e.program(13,r,0),inverse:e.program(15,r,0),data:r,loc:{start:{line:25,column:2},end:{line:29,column:2}}}))!=null?o:""},13:function(e,l,a,p,r){var o,n=e.lookupProperty||function(s,u){if(Object.prototype.hasOwnProperty.call(s,u))return s[u]};return"    <p>Invalid search: "+e.escapeExpression((o=(o=n(a,"errorMessage")||(l!=null?n(l,"errorMessage"):l))!=null?o:e.hooks.helperMissing,typeof o=="function"?o.call(l??(e.nullContext||{}),{name:"errorMessage",hash:{},data:r,loc:{start:{line:26,column:23},end:{line:26,column:39}}}):o))+`.</p>
`},15:function(e,l,a,p,r){return`    <p>Please type something into the search bar to perform a search.</p>
  `},compiler:[8,">= 4.3.0"],main:function(e,l,a,p,r){var o,n=l??(e.nullContext||{}),s=e.lookupProperty||function(u,i){if(Object.prototype.hasOwnProperty.call(u,i))return u[i]};return`<h1>
`+((o=s(a,"if").call(n,l!=null?s(l,"value"):l,{name:"if",hash:{},fn:e.program(1,r,0),inverse:e.program(3,r,0),data:r,loc:{start:{line:2,column:2},end:{line:6,column:9}}}))!=null?o:"")+`</h1>

`+((o=(s(a,"isNonEmptyArray")||l&&s(l,"isNonEmptyArray")||e.hooks.helperMissing).call(n,l!=null?s(l,"results"):l,{name:"isNonEmptyArray",hash:{},fn:e.program(5,r,0),inverse:e.program(9,r,0),data:r,loc:{start:{line:9,column:0},end:{line:44,column:20}}}))!=null?o:"")},useData:!0}),y["settings-modal-body"]=d({1:function(e,l,a,p,r){var o,n=e.lookupProperty||function(s,u){if(Object.prototype.hasOwnProperty.call(s,u))return s[u]};return(o=n(a,"if").call(l??(e.nullContext||{}),l!=null?n(l,"description"):l,{name:"if",hash:{},fn:e.program(2,r,0),inverse:e.noop,data:r,loc:{start:{line:40,column:6},end:{line:53,column:13}}}))!=null?o:""},2:function(e,l,a,p,r){var o,n=e.lookupProperty||function(s,u){if(Object.prototype.hasOwnProperty.call(s,u))return s[u]};return`        <dl class="shortcut-row">
          <dd class="shortcut-description">
            `+e.escapeExpression(e.lambda(l!=null?n(l,"description"):l,l))+`
          </dd>
          <dt class="shortcut-keys">
`+((o=n(a,"if").call(l??(e.nullContext||{}),l!=null?n(l,"displayAs"):l,{name:"if",hash:{},fn:e.program(3,r,0),inverse:e.program(5,r,0),data:r,loc:{start:{line:46,column:12},end:{line:50,column:19}}}))!=null?o:"")+`          </dt>
        </dl>
`},3:function(e,l,a,p,r){var o,n=e.lookupProperty||function(s,u){if(Object.prototype.hasOwnProperty.call(s,u))return s[u]};return"              "+((o=e.lambda(l!=null?n(l,"displayAs"):l,l))!=null?o:"")+`
`},5:function(e,l,a,p,r){var o=e.lookupProperty||function(n,s){if(Object.prototype.hasOwnProperty.call(n,s))return n[s]};return"              <kbd><kbd>"+e.escapeExpression(e.lambda(l!=null?o(l,"key"):l,l))+`</kbd></kbd>
`},compiler:[8,">= 4.3.0"],main:function(e,l,a,p,r){var o,n=e.lookupProperty||function(s,u){if(Object.prototype.hasOwnProperty.call(s,u))return s[u]};return`<div id="settings-modal-content">
  <div id="settings-content">
    <label class="switch-button-container">
      <div>
        <span>Theme</span>
        <p>Use the documentation UI in a theme.</p>
      </div>
      <div>
        <select name="theme" class="settings-select">
          <option value="dark">Dark</option>
          <option value="light">Light</option>
          <option value="system">System</option>
        </select>
      </div>
    </label>
    <label class="switch-button-container">
      <div>
        <span>Show tooltips</span>
        <p>Show tooltips when mousing over code references.</p>
      </div>
      <div class="switch-button">
        <input class="switch-button__checkbox" type="checkbox" name="tooltips" />
        <div class="switch-button__bg"></div>
      </div>
    </label>
    <label class="switch-button-container">
      <div>
        <span>Run in Livebook</span>
        <p>Use Direct Address for \u201CRun in Livebook\u201D badges.</p>
      </div>
      <div class="switch-button">
        <input class="switch-button__checkbox" type="checkbox" name="direct_livebook_url" />
        <div class="switch-button__bg"></div>
      </div>
    </label>
    <input class="input" type="url" name="livebook_url" placeholder="Enter Livebook instance URL" aria-label="Enter Livebook instance URL" />
  </div>
  <div id="keyboard-shortcuts-content" class="hidden">
`+((o=n(a,"each").call(l??(e.nullContext||{}),l!=null?n(l,"shortcuts"):l,{name:"each",hash:{},fn:e.program(1,r,0),inverse:e.noop,data:r,loc:{start:{line:39,column:4},end:{line:54,column:13}}}))!=null?o:"")+`  </div>
</div>
`},useData:!0}),y["sidebar-items"]=d({1:function(e,l,a,p,r,o,n){var s,u=l??(e.nullContext||{}),i=e.hooks.helperMissing,c=e.lookupProperty||function(t,f){if(Object.prototype.hasOwnProperty.call(t,f))return t[f]};return((s=(c(a,"groupChanged")||l&&c(l,"groupChanged")||i).call(u,n[1],(s=o[0][0])!=null?c(s,"group"):s,{name:"groupChanged",hash:{},fn:e.program(2,r,0,o,n),inverse:e.noop,data:r,blockParams:o,loc:{start:{line:2,column:2},end:{line:6,column:19}}}))!=null?s:"")+`
`+((s=(c(a,"nestingChanged")||l&&c(l,"nestingChanged")||i).call(u,n[1],o[0][0],{name:"nestingChanged",hash:{},fn:e.program(7,r,0,o,n),inverse:e.noop,data:r,blockParams:o,loc:{start:{line:8,column:2},end:{line:10,column:21}}}))!=null?s:"")+`
  <li class="`+((s=(c(a,"isLocal")||l&&c(l,"isLocal")||i).call(u,(s=o[0][0])!=null?c(s,"id"):s,{name:"isLocal",hash:{},fn:e.program(9,r,0,o,n),inverse:e.noop,data:r,blockParams:o,loc:{start:{line:12,column:13},end:{line:12,column:62}}}))!=null?s:"")+`">
    <a href="`+e.escapeExpression(e.lambda((s=o[0][0])!=null?c(s,"id"):s,l))+".html"+((s=(c(a,"isLocal")||l&&c(l,"isLocal")||i).call(u,(s=o[0][0])!=null?c(s,"id"):s,{name:"isLocal",hash:{},fn:e.program(11,r,0,o,n),inverse:e.noop,data:r,blockParams:o,loc:{start:{line:13,column:29},end:{line:13,column:69}}}))!=null?s:"")+'" class="expand" aria-current="'+((s=(c(a,"isLocal")||l&&c(l,"isLocal")||i).call(u,(s=o[0][0])!=null?c(s,"id"):s,{name:"isLocal",hash:{},fn:e.program(13,r,0,o,n),inverse:e.program(15,r,0,o,n),data:r,blockParams:o,loc:{start:{line:13,column:100},end:{line:13,column:149}}}))!=null?s:"")+'" '+((s=(c(a,"isArray")||l&&c(l,"isArray")||i).call(u,(s=o[0][0])!=null?c(s,"headers"):s,{name:"isArray",hash:{},fn:e.program(3,r,0,o,n),inverse:e.program(5,r,0,o,n),data:r,blockParams:o,loc:{start:{line:13,column:151},end:{line:13,column:210}}}))!=null?s:"")+`>
`+((s=c(a,"if").call(u,(s=o[0][0])!=null?c(s,"nested_title"):s,{name:"if",hash:{},fn:e.program(17,r,0,o,n),inverse:e.program(19,r,0,o,n),data:r,blockParams:o,loc:{start:{line:14,column:6},end:{line:18,column:13}}}))!=null?s:"")+`    </a>

`+((s=(c(a,"isEmptyArray")||l&&c(l,"isEmptyArray")||i).call(u,(s=o[0][0])!=null?c(s,"headers"):s,{name:"isEmptyArray",hash:{},fn:e.program(3,r,0,o,n),inverse:e.program(21,r,0,o,n),data:r,blockParams:o,loc:{start:{line:21,column:4},end:{line:24,column:21}}}))!=null?s:"")+`
`+((s=(c(a,"isArray")||l&&c(l,"isArray")||i).call(u,(s=o[0][0])!=null?c(s,"headers"):s,{name:"isArray",hash:{},fn:e.program(24,r,0,o,n),inverse:e.program(28,r,0,o,n),data:r,blockParams:o,loc:{start:{line:26,column:4},end:{line:74,column:16}}}))!=null?s:"")+`  </li>
`},2:function(e,l,a,p,r,o){var n,s=e.lookupProperty||function(u,i){if(Object.prototype.hasOwnProperty.call(u,i))return u[i]};return'    <li class="group" '+((n=(s(a,"isArray")||l&&s(l,"isArray")||e.hooks.helperMissing).call(l??(e.nullContext||{}),(n=o[1][0])!=null?s(n,"headers"):n,{name:"isArray",hash:{},fn:e.program(3,r,0,o),inverse:e.program(5,r,0,o),data:r,blockParams:o,loc:{start:{line:3,column:22},end:{line:3,column:81}}}))!=null?n:"")+`>
      `+e.escapeExpression(e.lambda((n=o[1][0])!=null?s(n,"group"):n,l))+`
    </li>
`},3:function(e,l,a,p,r){return""},5:function(e,l,a,p,r){return'translate="no"'},7:function(e,l,a,p,r,o){var n,s=e.lookupProperty||function(u,i){if(Object.prototype.hasOwnProperty.call(u,i))return u[i]};return'    <li class="nesting-context" aria-hidden="true" translate="no">'+e.escapeExpression(e.lambda((n=o[1][0])!=null?s(n,"nested_context"):n,l))+`</li>
`},9:function(e,l,a,p,r){return"current-page open"},11:function(e,l,a,p,r){return"#content"},13:function(e,l,a,p,r){return"page"},15:function(e,l,a,p,r){return"false"},17:function(e,l,a,p,r,o){var n,s=e.lookupProperty||function(u,i){if(Object.prototype.hasOwnProperty.call(u,i))return u[i]};return"        "+((n=e.lambda((n=o[1][0])!=null?s(n,"nested_title"):n,l))!=null?n:"")+`
`},19:function(e,l,a,p,r,o){var n,s=e.lookupProperty||function(u,i){if(Object.prototype.hasOwnProperty.call(u,i))return u[i]};return"        "+((n=e.lambda((n=o[1][0])!=null?s(n,"title"):n,l))!=null?n:"")+`
`},21:function(e,l,a,p,r,o){var n,s=e.lookupProperty||function(u,i){if(Object.prototype.hasOwnProperty.call(u,i))return u[i]};return'      <button class="icon-expand" aria-label="expand" aria-expanded="'+((n=(s(a,"isLocal")||l&&s(l,"isLocal")||e.hooks.helperMissing).call(l??(e.nullContext||{}),(n=o[1][0])!=null?s(n,"id"):n,{name:"isLocal",hash:{},fn:e.program(22,r,0,o),inverse:e.program(15,r,0,o),data:r,blockParams:o,loc:{start:{line:23,column:69},end:{line:23,column:118}}}))!=null?n:"")+'" aria-controls="node-'+e.escapeExpression(e.lambda((n=o[1][0])!=null?s(n,"id"):n,l))+`-headers"></button>
`},22:function(e,l,a,p,r){return"true"},24:function(e,l,a,p,r,o){var n,s=e.lookupProperty||function(u,i){if(Object.prototype.hasOwnProperty.call(u,i))return u[i]};return(n=(s(a,"isNonEmptyArray")||l&&s(l,"isNonEmptyArray")||e.hooks.helperMissing).call(l??(e.nullContext||{}),(n=o[1][0])!=null?s(n,"headers"):n,{name:"isNonEmptyArray",hash:{},fn:e.program(25,r,0,o),inverse:e.noop,data:r,blockParams:o,loc:{start:{line:27,column:6},end:{line:35,column:26}}}))!=null?n:""},25:function(e,l,a,p,r,o){var n,s=e.lookupProperty||function(u,i){if(Object.prototype.hasOwnProperty.call(u,i))return u[i]};return'        <ul id="node-'+e.escapeExpression(e.lambda((n=o[2][0])!=null?s(n,"id"):n,l))+`-headers">
`+((n=s(a,"each").call(l??(e.nullContext||{}),(n=o[2][0])!=null?s(n,"headers"):n,{name:"each",hash:{},fn:e.program(26,r,0,o),inverse:e.noop,data:r,blockParams:o,loc:{start:{line:29,column:10},end:{line:33,column:19}}}))!=null?n:"")+`        </ul>
`},26:function(e,l,a,p,r,o){var n,s,u=l??(e.nullContext||{}),i=e.hooks.helperMissing,c="function",t=e.lookupProperty||function(f,m){if(Object.prototype.hasOwnProperty.call(f,m))return f[m]};return`            <li>
              <a href="`+e.escapeExpression(e.lambda((n=o[3][0])!=null?t(n,"id"):n,l))+".html#"+((n=(s=(s=t(a,"anchor")||(l!=null?t(l,"anchor"):l))!=null?s:i,typeof s===c?s.call(u,{name:"anchor",hash:{},data:r,blockParams:o,loc:{start:{line:31,column:40},end:{line:31,column:52}}}):s))!=null?n:"")+'">'+((n=(s=(s=t(a,"id")||(l!=null?t(l,"id"):l))!=null?s:i,typeof s===c?s.call(u,{name:"id",hash:{},data:r,blockParams:o,loc:{start:{line:31,column:54},end:{line:31,column:62}}}):s))!=null?n:"")+`</a>
            </li>
`},28:function(e,l,a,p,r,o){var n,s=l??(e.nullContext||{}),u=e.hooks.helperMissing,i=e.lookupProperty||function(c,t){if(Object.prototype.hasOwnProperty.call(c,t))return c[t]};return'      <ul id="node-'+e.escapeExpression(e.lambda((n=o[1][0])!=null?i(n,"id"):n,l))+`-headers">
`+((n=(i(a,"showSections")||l&&i(l,"showSections")||u).call(s,o[1][0],{name:"showSections",hash:{},fn:e.program(29,r,0,o),inverse:e.noop,data:r,blockParams:o,loc:{start:{line:38,column:8},end:{line:52,column:25}}}))!=null?n:"")+((n=(i(a,"showSummary")||l&&i(l,"showSummary")||u).call(s,o[1][0],{name:"showSummary",hash:{},fn:e.program(34,r,0,o),inverse:e.noop,data:r,blockParams:o,loc:{start:{line:53,column:8},end:{line:57,column:24}}}))!=null?n:"")+((n=i(a,"each").call(s,(n=o[1][0])!=null?i(n,"nodeGroups"):n,{name:"each",hash:{},fn:e.program(36,r,1,o),inverse:e.noop,data:r,blockParams:o,loc:{start:{line:58,column:8},end:{line:72,column:17}}}))!=null?n:"")+`      </ul>
`},29:function(e,l,a,p,r,o){var n,s=l??(e.nullContext||{}),u=e.hooks.helperMissing,i=e.lambda,c=e.escapeExpression,t=e.lookupProperty||function(f,m){if(Object.prototype.hasOwnProperty.call(f,m))return f[m]};return'          <li class="docs '+((n=(t(a,"isLocal")||l&&t(l,"isLocal")||u).call(s,(n=o[2][0])!=null?t(n,"id"):n,{name:"isLocal",hash:{},fn:e.program(30,r,0,o),inverse:e.noop,data:r,blockParams:o,loc:{start:{line:39,column:26},end:{line:39,column:62}}}))!=null?n:"")+`">
            <a href="`+c(i((n=o[2][0])!=null?t(n,"id"):n,l))+`.html#content" class="expand">
              Sections
            </a>
            <button class="icon-expand" aria-label="expand" aria-expanded="`+((n=(t(a,"isLocal")||l&&t(l,"isLocal")||u).call(s,(n=o[2][0])!=null?t(n,"id"):n,{name:"isLocal",hash:{},fn:e.program(22,r,0,o),inverse:e.program(15,r,0,o),data:r,blockParams:o,loc:{start:{line:43,column:75},end:{line:43,column:124}}}))!=null?n:"")+'" aria-controls="'+c(i((n=o[2][0])!=null?t(n,"id"):n,l))+`-sections-list"></button>
            <ul id="`+c(i((n=o[2][0])!=null?t(n,"id"):n,l))+`-sections-list" class="sections-list deflist">
`+((n=t(a,"each").call(s,l!=null?t(l,"sections"):l,{name:"each",hash:{},fn:e.program(32,r,0,o),inverse:e.noop,data:r,blockParams:o,loc:{start:{line:45,column:14},end:{line:49,column:23}}}))!=null?n:"")+`            </ul>
          </li>
`},30:function(e,l,a,p,r){return"open"},32:function(e,l,a,p,r,o){var n,s,u=e.escapeExpression,i=l??(e.nullContext||{}),c=e.hooks.helperMissing,t="function",f=e.lookupProperty||function(m,v){if(Object.prototype.hasOwnProperty.call(m,v))return m[v]};return`                <li>
                  <a href="`+u(e.lambda((n=o[3][0])!=null?f(n,"id"):n,l))+".html#"+u((s=(s=f(a,"anchor")||(l!=null?f(l,"anchor"):l))!=null?s:c,typeof s===t?s.call(i,{name:"anchor",hash:{},data:r,blockParams:o,loc:{start:{line:47,column:44},end:{line:47,column:54}}}):s))+'">'+((n=(s=(s=f(a,"id")||(l!=null?f(l,"id"):l))!=null?s:c,typeof s===t?s.call(i,{name:"id",hash:{},data:r,blockParams:o,loc:{start:{line:47,column:56},end:{line:47,column:64}}}):s))!=null?n:"")+`</a>
                </li>
`},34:function(e,l,a,p,r,o){var n,s=e.lookupProperty||function(u,i){if(Object.prototype.hasOwnProperty.call(u,i))return u[i]};return`          <li>
            <a href="`+e.escapeExpression(e.lambda((n=o[2][0])!=null?s(n,"id"):n,l))+`.html#summary" class="summary">Summary</a>
          </li>
`},36:function(e,l,a,p,r,o){var n,s=e.lambda,u=e.escapeExpression,i=e.lookupProperty||function(c,t){if(Object.prototype.hasOwnProperty.call(c,t))return c[t]};return`          <li class="docs">
            <a href="`+u(s((n=o[2][0])!=null?i(n,"id"):n,l))+".html#"+u(s((n=o[0][0])!=null?i(n,"key"):n,l))+`" class="expand">
              `+u(s((n=o[0][0])!=null?i(n,"name"):n,l))+`
            </a>
            <button class="icon-expand" aria-label="expand" aria-expanded="false" aria-controls="node-`+u(s((n=o[2][0])!=null?i(n,"id"):n,l))+"-group-"+u(s((n=o[0][0])!=null?i(n,"key"):n,l))+`-list"></button>
            <ul id="node-`+u(s((n=o[2][0])!=null?i(n,"id"):n,l))+"-group-"+u(s((n=o[0][0])!=null?i(n,"key"):n,l))+'-list" class="'+u(s((n=o[0][0])!=null?i(n,"key"):n,l))+`-list deflist">
`+((n=i(a,"each").call(l??(e.nullContext||{}),(n=o[0][0])!=null?i(n,"nodes"):n,{name:"each",hash:{},fn:e.program(37,r,0,o),inverse:e.noop,data:r,blockParams:o,loc:{start:{line:65,column:14},end:{line:69,column:23}}}))!=null?n:"")+`            </ul>
          </li>
`},37:function(e,l,a,p,r,o){var n,s,u=e.escapeExpression,i=l??(e.nullContext||{}),c=e.hooks.helperMissing,t="function",f=e.lookupProperty||function(m,v){if(Object.prototype.hasOwnProperty.call(m,v))return m[v]};return`                <li>
                  <a href="`+u(e.lambda((n=o[3][0])!=null?f(n,"id"):n,l))+".html#"+u((s=(s=f(a,"anchor")||(l!=null?f(l,"anchor"):l))!=null?s:c,typeof s===t?s.call(i,{name:"anchor",hash:{},data:r,blockParams:o,loc:{start:{line:67,column:44},end:{line:67,column:54}}}):s))+'" title="'+u((s=(s=f(a,"title")||(l!=null?f(l,"title"):l))!=null?s:c,typeof s===t?s.call(i,{name:"title",hash:{},data:r,blockParams:o,loc:{start:{line:67,column:63},end:{line:67,column:72}}}):s))+'" translate="no">'+u((s=(s=f(a,"id")||(l!=null?f(l,"id"):l))!=null?s:c,typeof s===t?s.call(i,{name:"id",hash:{},data:r,blockParams:o,loc:{start:{line:67,column:89},end:{line:67,column:95}}}):s))+`</a>
                </li>
`},compiler:[8,">= 4.3.0"],main:function(e,l,a,p,r,o,n){var s,u=e.lookupProperty||function(i,c){if(Object.prototype.hasOwnProperty.call(i,c))return i[c]};return(s=u(a,"each").call(l??(e.nullContext||{}),l!=null?u(l,"nodes"):l,{name:"each",hash:{},fn:e.program(1,r,2,o,n),inverse:e.noop,data:r,blockParams:o,loc:{start:{line:1,column:0},end:{line:76,column:9}}}))!=null?s:""},useData:!0,useDepths:!0,useBlockParams:!0}),y.tabset=d({1:function(e,l,a,p,r){var o,n,s=l??(e.nullContext||{}),u=e.hooks.helperMissing,i="function",c=e.escapeExpression,t=e.lookupProperty||function(f,m){if(Object.prototype.hasOwnProperty.call(f,m))return f[m]};return'    <button role="tab" id="tab-'+c((n=(n=t(a,"setIndex")||(l!=null?t(l,"setIndex"):l))!=null?n:u,typeof n===i?n.call(s,{name:"setIndex",hash:{},data:r,loc:{start:{line:3,column:31},end:{line:3,column:43}}}):n))+"-"+c((n=(n=t(a,"index")||r&&t(r,"index"))!=null?n:u,typeof n===i?n.call(s,{name:"index",hash:{},data:r,loc:{start:{line:3,column:44},end:{line:3,column:54}}}):n))+`" class="tabset-tab"
    tabindex="`+((o=t(a,"if").call(s,r&&t(r,"index"),{name:"if",hash:{},fn:e.program(2,r,0),inverse:e.program(4,r,0),data:r,loc:{start:{line:4,column:14},end:{line:4,column:46}}}))!=null?o:"")+`"
    aria-selected="`+((o=t(a,"if").call(s,r&&t(r,"index"),{name:"if",hash:{},fn:e.program(6,r,0),inverse:e.program(8,r,0),data:r,loc:{start:{line:5,column:19},end:{line:5,column:57}}}))!=null?o:"")+`"
    aria-controls="tabpanel-`+c((n=(n=t(a,"setIndex")||(l!=null?t(l,"setIndex"):l))!=null?n:u,typeof n===i?n.call(s,{name:"setIndex",hash:{},data:r,loc:{start:{line:6,column:28},end:{line:6,column:40}}}):n))+"-"+c((n=(n=t(a,"index")||r&&t(r,"index"))!=null?n:u,typeof n===i?n.call(s,{name:"index",hash:{},data:r,loc:{start:{line:6,column:41},end:{line:6,column:51}}}):n))+`">
      `+c(e.lambda(l!=null?t(l,"label"):l,l))+`
    </button>
`},2:function(e,l,a,p,r){return"-1"},4:function(e,l,a,p,r){return"0"},6:function(e,l,a,p,r){return"false"},8:function(e,l,a,p,r){return"true"},10:function(e,l,a,p,r){var o,n,s=l??(e.nullContext||{}),u=e.hooks.helperMissing,i="function",c=e.escapeExpression,t=e.lookupProperty||function(f,m){if(Object.prototype.hasOwnProperty.call(f,m))return f[m]};return'  <div role="tabpanel" id="tabpanel-'+c((n=(n=t(a,"setIndex")||(l!=null?t(l,"setIndex"):l))!=null?n:u,typeof n===i?n.call(s,{name:"setIndex",hash:{},data:r,loc:{start:{line:13,column:36},end:{line:13,column:48}}}):n))+"-"+c((n=(n=t(a,"index")||r&&t(r,"index"))!=null?n:u,typeof n===i?n.call(s,{name:"index",hash:{},data:r,loc:{start:{line:13,column:49},end:{line:13,column:59}}}):n))+`" class="tabset-panel"
    `+((o=t(a,"if").call(s,r&&t(r,"index"),{name:"if",hash:{},fn:e.program(11,r,0),inverse:e.noop,data:r,loc:{start:{line:14,column:4},end:{line:14,column:31}}}))!=null?o:"")+' tabindex="'+((o=t(a,"if").call(s,r&&t(r,"index"),{name:"if",hash:{},fn:e.program(2,r,0),inverse:e.program(4,r,0),data:r,loc:{start:{line:14,column:42},end:{line:14,column:74}}}))!=null?o:"")+`"
    aria-labelledby="tab-`+c((n=(n=t(a,"setIndex")||(l!=null?t(l,"setIndex"):l))!=null?n:u,typeof n===i?n.call(s,{name:"setIndex",hash:{},data:r,loc:{start:{line:15,column:25},end:{line:15,column:37}}}):n))+"-"+c((n=(n=t(a,"index")||r&&t(r,"index"))!=null?n:u,typeof n===i?n.call(s,{name:"index",hash:{},data:r,loc:{start:{line:15,column:38},end:{line:15,column:48}}}):n))+`">
`+((o=t(a,"each").call(s,l!=null?t(l,"content"):l,{name:"each",hash:{},fn:e.program(13,r,0),inverse:e.noop,data:r,loc:{start:{line:16,column:4},end:{line:18,column:13}}}))!=null?o:"")+`  </div>
`},11:function(e,l,a,p,r){return"hidden"},13:function(e,l,a,p,r){var o;return"      "+((o=e.lambda(l,l))!=null?o:"")+`
`},compiler:[8,">= 4.3.0"],main:function(e,l,a,p,r){var o,n=l??(e.nullContext||{}),s=e.lookupProperty||function(u,i){if(Object.prototype.hasOwnProperty.call(u,i))return u[i]};return`<div role="tablist" class="tabset-tablist">
`+((o=s(a,"each").call(n,l!=null?s(l,"tabs"):l,{name:"each",hash:{},fn:e.program(1,r,0),inverse:e.noop,data:r,loc:{start:{line:2,column:2},end:{line:9,column:11}}}))!=null?o:"")+`</div>

`+((o=s(a,"each").call(n,l!=null?s(l,"tabs"):l,{name:"each",hash:{},fn:e.program(10,r,0),inverse:e.noop,data:r,loc:{start:{line:12,column:0},end:{line:20,column:9}}}))!=null?o:"")},useData:!0}),y["tooltip-body"]=d({1:function(e,l,a,p,r){var o,n=e.lookupProperty||function(s,u){if(Object.prototype.hasOwnProperty.call(s,u))return s[u]};return`  <section class="docstring docstring-plain">
    `+e.escapeExpression(e.lambda((o=l!=null?n(l,"hint"):l)!=null?n(o,"description"):o,l))+`
  </section>
`},3:function(e,l,a,p,r){var o,n=e.lambda,s=e.escapeExpression,u=e.lookupProperty||function(i,c){if(Object.prototype.hasOwnProperty.call(i,c))return i[c]};return`  <div class="detail-header">
    <h1 class="signature">
      <span translate="no">`+s(n((o=l!=null?u(l,"hint"):l)!=null?u(o,"title"):o,l))+`</span>
      <div class="version-info" translate="no">`+s(n((o=l!=null?u(l,"hint"):l)!=null?u(o,"version"):o,l))+`</div>
    </h1>
  </div>
`+((o=u(a,"if").call(l??(e.nullContext||{}),(o=l!=null?u(l,"hint"):l)!=null?u(o,"description"):o,{name:"if",hash:{},fn:e.program(4,r,0),inverse:e.noop,data:r,loc:{start:{line:12,column:2},end:{line:16,column:9}}}))!=null?o:"")},4:function(e,l,a,p,r){var o,n=e.lookupProperty||function(s,u){if(Object.prototype.hasOwnProperty.call(s,u))return s[u]};return`    <section class="docstring">
      `+((o=e.lambda((o=l!=null?n(l,"hint"):l)!=null?n(o,"description"):o,l))!=null?o:"")+`
    </section>
`},compiler:[8,">= 4.3.0"],main:function(e,l,a,p,r){var o,n=e.lookupProperty||function(s,u){if(Object.prototype.hasOwnProperty.call(s,u))return s[u]};return(o=n(a,"if").call(l??(e.nullContext||{}),l!=null?n(l,"isPlain"):l,{name:"if",hash:{},fn:e.program(1,r,0),inverse:e.program(3,r,0),data:r,loc:{start:{line:1,column:0},end:{line:17,column:7}}}))!=null?o:""},useData:!0}),y["tooltip-layout"]=d({compiler:[8,">= 4.3.0"],main:function(e,l,a,p,r){return`<div id="tooltip">
  <div class="tooltip-body"></div>
</div>
`},useData:!0}),y["versions-dropdown"]=d({1:function(e,l,a,p,r){var o,n,s=l??(e.nullContext||{}),u=e.hooks.helperMissing,i="function",c=e.escapeExpression,t=e.lookupProperty||function(f,m){if(Object.prototype.hasOwnProperty.call(f,m))return f[m]};return'        <option translate="no" value="'+c((n=(n=t(a,"url")||(l!=null?t(l,"url"):l))!=null?n:u,typeof n===i?n.call(s,{name:"url",hash:{},data:r,loc:{start:{line:7,column:38},end:{line:7,column:45}}}):n))+'"'+((o=t(a,"if").call(s,l!=null?t(l,"isCurrentVersion"):l,{name:"if",hash:{},fn:e.program(2,r,0),inverse:e.noop,data:r,loc:{start:{line:7,column:46},end:{line:7,column:95}}}))!=null?o:"")+`>
          `+c((n=(n=t(a,"version")||(l!=null?t(l,"version"):l))!=null?n:u,typeof n===i?n.call(s,{name:"version",hash:{},data:r,loc:{start:{line:8,column:10},end:{line:8,column:21}}}):n))+`
        </option>
`},2:function(e,l,a,p,r){return" selected disabled"},compiler:[8,">= 4.3.0"],main:function(e,l,a,p,r){var o,n=e.lookupProperty||function(s,u){if(Object.prototype.hasOwnProperty.call(s,u))return s[u]};return`<form autocomplete="off">
  <label>
    <span class="sidebar-projectVersionsDropdownCaret" aria-hidden="true">&#x25bc;</span>
    <span class="sr-only">Project version</span>
    <select class="sidebar-projectVersionsDropdown">
`+((o=n(a,"each").call(l??(e.nullContext||{}),l!=null?n(l,"nodes"):l,{name:"each",hash:{},fn:e.program(1,r,0),inverse:e.noop,data:r,loc:{start:{line:6,column:6},end:{line:10,column:15}}}))!=null?o:"")+`    </select>
  </label>
</form>
`},useData:!0})})();})();
