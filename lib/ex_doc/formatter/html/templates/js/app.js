/*jslint browser: true, es5: true */
/*globals $, hljs, sidebarNodes: true */
(function () {
    "use strict";

    // full_list.js
    var inSearch = null,
        defaultSearchItemTimeOut = 0, //set to "0" if not testing
        searchIndex = 0,
        searchCache = [],
        searchString = '',
        regexSearchString = '',
        caseSensitiveMatch = false,
        ignoreKeyCodeMin = 8,
        ignoreKeyCodeMax = 46,
        commandKey = 91,
        lastRowClass = '',
        sidebarNav = $('.nav'),
        clicked = null;

    function escapeText(text) {
        return text.replace(/[\-\[\]{}()*+?.,\\\^$|#\s]/g, "\\$&");
    }

    function highlight(no_padding) {
        var n = 1;
        $('#full_list a.object_link:visible').each(function () {
            var next = n === 1 ? 2 : 1,
                li = $(this).parent();
            li.removeClass("r" + next).addClass("r" + n);
            if (no_padding) {
                li.addClass("no_padding");
            } else {
                li.removeClass("no_padding");
            }
            n = next;
        });
    }

    function showAllResults() {
        clearTimeout(inSearch);
        inSearch = defaultSearchItemTimeOut;
        $('.search_uncollapsed').removeClass('search_uncollapsed');
        $('#sidebar').removeClass('in_search');
        $('#full_list li').removeClass('found').each(function () {
            var link = $(this).find('a.object_link:first');
            link.text(link.text());
        });
        if (clicked) {
            clicked.parents('li').each(function () {
                $(this).removeClass('collapsed').prev().removeClass('collapsed');
            });
        }
        $('#no_results').text('');
        $('#search').removeClass('loading');
        highlight();
    }

    function searchDone() {
        highlight(true);
        if ($('#full_list li.found').size() === 0) {
            $('#no_results').text('No results were found.').hide().fadeIn();
        } else {
            $('#no_results').text('');
        }

        $('#search').removeClass('loading');
        clearTimeout(inSearch);
        inSearch = null;
    }

    function searchItem() {
        var i,
            item,
            searchName,
            matchString,
            matchRegexp;
        for (i = 0; i < searchCache.length / 50; i += 1) {
            item = searchCache[searchIndex];
            searchName = (searchString.indexOf('.') !== -1 ? item.fullName : item.name);
            matchString = regexSearchString;
            matchRegexp = new RegExp(matchString, caseSensitiveMatch ? "" : "i");

            if (searchName.match(matchRegexp) === null) {
                item.node.removeClass('found');
            } else {
                item.node.addClass('found');
                item.node.parents('li').addClass('search_uncollapsed');
                item.node.removeClass(lastRowClass).addClass(lastRowClass === 'r1' ? 'r2' : 'r1');
                lastRowClass = item.node.hasClass('r1') ? 'r1' : 'r2';
                item.link.html(item.name.replace(matchRegexp, "<strong>$&</strong>"));
            }

            if (searchCache.length === searchIndex + 1) {
                searchDone();
                return;
            }
            searchIndex += 1;
        }
        inSearch = setTimeout(function () {
            searchItem();
        }, defaultSearchItemTimeOut);
    }

    function fullListSearch() {
        // generate cache
        searchCache = [];
        $('#full_list li').each(function () {
            var link = $(this).find('a.object_link:first'),
                fullName;
            if (link.attr('title')) {
                fullName = link.attr('title').split(' ')[0];
                searchCache.push({
                    name: link.text(),
                    fullName: fullName,
                    node: $(this),
                    link: link
                });
            }
        });

        $('#search input').keypress(function (e) {
            if (e.which === 13) {
                $('#full_list li.found:first').find('a.object_link:first').click();
            }
        });

        $('#search input').bind("keyup search reset change propertychange input paste", function (evnt) {
            if ((evnt.keyCode > ignoreKeyCodeMin && evnt.keyCode < ignoreKeyCodeMax) || evnt.keyCode === commandKey) {
                return;
            }

            $('#search').addClass('loading');
            searchString = this.value;
            caseSensitiveMatch = searchString.match(/[A-Z]/) !== null;
            regexSearchString = escapeText(searchString);
            if (searchString === "") {
                showAllResults();
            } else {
                if (inSearch) {
                    clearTimeout(inSearch);
                }
                searchIndex = 0;
                lastRowClass = '';
                $('#sidebar').addClass('in_search');
                $('#no_results').text('');
                searchItem();
            }
        });

        $('#search input').focus();
    }

    function linkList() {
        $('#full_list li, #full_list li a:last').click(function (evt) {
            var toggle,
                win;

            if ($(this).hasClass('toggle')) {
                return true;
            }

            if (this.tagName.toLowerCase() === "li") {
                toggle = $(this).children('a.toggle');
                if (toggle.size() > 0 && evt.pageX < toggle.offset().left) {
                    toggle.click();
                    return false;
                }
            }

            if (clicked) {
                clicked.removeClass('clicked');
            }

            win = window.top.frames.main || window.parent;
            if (this.tagName.toLowerCase() === "a") {
                clicked = $(this).parent('li').addClass('clicked');
                win.location = this.href;
            } else {
                clicked = $(this).addClass('clicked');
                win.location = $(this).find('a:last').attr('href');
            }

            return false;
        });
    }

    function collapse() {
        $('#full_list a.toggle').click(function () {
            $(this).parent().toggleClass('collapsed').next().toggleClass('collapsed');
            highlight();
            return false;
        });

        $('#full_list > li.node').each(function () {
            $(this).addClass('collapsed').next('li.docs').addClass('collapsed');
        });

        highlight();
    }

    function escapeShortcut() {
        $(document).keydown(function (evt) {
            if (evt.which === 27) {
                $('#search_frame', window.top.document).slideUp(100);
                $('#search a', window.top.document).removeClass('active inactive');
                $(window.top).focus();
            }
        });
    }

    $(escapeShortcut);
    $(fullListSearch);
    $(linkList);
    $(collapse);

    /**
     * Fill the sidebar with links to different nodes
     *
     * This function replace an empty unordered list with an
     * an unordered list full of links to the different procotols, exceptions
     * and modules mentioned in the documentation.
     *
     * @param {Object} nodes - Container of protocols, exceptions and modules.
     * @param {String} filter - Filter of nodes, by default 'modules'.
     */
    function fillSidebarWithNodes(nodes, filter) {
        var full_list = $("#full_list");

        function scope(items) {
            var filtered = nodes[items],
                fullList = $('<ul>', {
                    'id': 'full_list'
                });

            if (!filtered) {
                full_list.replaceWith(fullList);
                return;
            }

            filtered.forEach(function (element) {
                var docs_container,
                    id = element.id,
                    li,
                    ul;

                /* li.node */
                li = $('<li>', {
                    'class': 'node'
                });

                if (element.hasOwnProperty('docs')) {
                    li.append($('<a/>').attr('class', 'toggle'));
                }

                li.append($('<a/>', {
                    'href': id + '.html',
                    'title': id,
                    'html': id,
                    'class': 'object_link'
                }));

                li.append($('<span/>', {
                    'class': 'node_name',
                    'html': id
                }));

                fullList.append(li);

                if (element.hasOwnProperty('docs')) {
                    /* li.docs */
                    docs_container = $('<li>', {
                        'class': 'docs'
                    });
                    ul = $('<ul>');

                    element.docs.forEach(function (element) {
                        var detail = $('<li>');

                        detail.append($('<a/>', {
                            'href': id + '.html' + '#' + element,
                            'title': id + '.' + element,
                            'class': 'object_link',
                            'html': element
                        }));
                        detail.append($('<span/>', {
                            'class': 'node_name',
                            'html': id
                        }));
                        ul.append(detail);
                    });

                    docs_container.append(ul);
                    fullList.append(docs_container);
                }
            });
            full_list.replaceWith(fullList);
        }

        filter = filter || 'modules';
        scope(filter);
    }

    window.fillSidebarWithNodes = fillSidebarWithNodes;

    /* Sidebar events */
    function resetSidebar() {
        escapeShortcut();
        fullListSearch();
        linkList();
        collapse();
    }

    sidebarNav.on('click', '#modules_list', function (e) {
        fillSidebarWithNodes(sidebarNodes, "modules");
        resetSidebar();
        $('#modules_list').parent().addClass('selected');
        $('#exceptions_list').parent().removeClass('selected');
        $('#protocols_list').parent().removeClass('selected');
        e.preventDefault();
    });

    sidebarNav.on('click', '#exceptions_list', function (e) {
        fillSidebarWithNodes(sidebarNodes, "exceptions");
        resetSidebar();
        $('#modules_list').parent().removeClass('selected');
        $('#exceptions_list').parent().addClass('selected');
        $('#protocols_list').parent().removeClass('selected');
        e.preventDefault();
    });

    sidebarNav.on('click', '#protocols_list', function (e) {
        fillSidebarWithNodes(sidebarNodes, "protocols");
        resetSidebar();
        $('#modules_list').parent().removeClass('selected');
        $('#exceptions_list').parent().removeClass('selected');
        $('#protocols_list').parent().addClass('selected');
        e.preventDefault();
    });

    // Setup Highlight.js
    hljs.configure({
        tabReplace: '    ', // 4 spaces
        languages: [] // disable auto-detect
    });

    hljs.initHighlighting();
}());
