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
        lastRowClass = '',
        sidebarNav = $('.nav');

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

    /**
     * When the search field is empty show the children nodes of the #full_list
     *
     * Also removes the class .search_uncollapsed, .in_search, .found and .loading
     * among other things to reset the sidebar default status
     */
    function showAllResults() {
        clearTimeout(inSearch);
        inSearch = defaultSearchItemTimeOut;
        $('.search_uncollapsed').removeClass('search_uncollapsed');
        $('#sidebar').removeClass('in_search');
        $('#full_list li').removeClass('found').each(function () {
            var link = $(this).find('a.object_link:first');
            link.text(link.text());
        });
        $('#no_results').text('');
        $('#spinning span').removeClass('glyphicon glyphicon-refresh spinning');
        $('#search button span.glyphicon-remove').addClass('glyphicon-search').removeClass('glyphicon-remove');
        highlight();
    }

    /**
     * If no results were found, shows a message to the user.
     *
     * Also remove the 'loading' icon and clear the timeout returned
     * initially by setTimeout.
     */
    function searchDone() {
        highlight(true);
        if ($('#full_list li.found').size() === 0) {
            $('#no_results').text('No results were found.').hide().fadeIn();
        } else {
            $('#no_results').text('');
        }

        $('#spinning span').removeClass('glyphicon glyphicon-refresh spinning');
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

    function performSearch() {
        searchString = $("#search input").val();
        caseSensitiveMatch = searchString.match(/[A-Z]/) !== null;
        regexSearchString = escapeText(searchString);

        if (searchString === "") {
            showAllResults();
        } else {
            if (inSearch) {
                clearTimeout(inSearch);
            }
            $('#spinning span').addClass('glyphicon glyphicon-refresh spinning');
            $('#search button span.glyphicon-search').addClass('glyphicon-remove').removeClass('glyphicon-search');
            searchIndex = 0;
            lastRowClass = '';
            $('#sidebar').addClass('in_search');
            $('#no_results').text('');
            searchItem();
        }
    }

    /**
     * Fill the searchCache Array with the links found inside of the #full_list
     *
     * Define the initial set of events for the search field.
     */
    function fullListSearch() {
        // generate cache
        searchCache = [];
        $('#spinning span').removeClass('glyphicon glyphicon-refresh spinning');

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

        $('#search input').focus();
    }

    function collapse() {
        $('#full_list a.toggle').click(function () {
            $(this).parent().toggleClass('collapsed').next().toggleClass('collapsed');
            highlight();
            return false;
        });

        $('#full_list > li.node:not(.clicked)').each(function () {
            $(this).addClass('collapsed').next('li.docs').addClass('collapsed');
        });

        highlight();
    }

    /**
     * Identify external links inside of an specific section
     *
     * This function adds an icon to identify an external link.
     *
     * @param {String} section - Section where we want to identify the external links.
     */
    function identifyExternalLinks(section) {
        $([section, 'a'].join(' ')).filter(function () {
            return (this.hostname !== location.hostname);
        }).append($('<span/>').attr({
            'class': 'glyphicon glyphicon-new-window',
            'aria-hidden': 'true'
        })).addClass('external');
    }

    function setupSelected(id) {
        ["#modules_list", "#exceptions_list", "#protocols_list"].forEach(function (element) {
            if (element === id) {
                $(element).parent().addClass('selected');
            } else {
                $(element).parent().removeClass('selected');
            }
        });
    }

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
        var full_list = $("#full_list"),
            module_type;

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
                    ul,
                    current_path,
                    href;

                /* li.node */
                li = $('<li>', {
                    'class': 'node'
                });

                if (element.hasOwnProperty('docs')) {
                    li.append($('<a/>').attr('class', 'toggle'));
                }

                // When visiting a module page, the link to this module page
                // in the menu should not link to a new page, instead should
                // link to the top of the page itself.
                current_path = window.location.pathname.split('/');
                href = id + '.html';
                if (href === current_path[current_path.length - 1]) {
                    li.addClass("clicked");
                    href = href + '#content';
                }

                li.append($('<a/>', {
                    'href': href,
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

        module_type = $('#content h1 small').text();
        if (module_type && (module_type === 'exception' || module_type === 'protocol')) {
            module_type = module_type + 's'; // pluralize 'exception' or 'protocol'
        } else {
            module_type = 'modules';
        }

        filter = filter || module_type;
        scope(filter);
        setupSelected(['#', filter, '_list'].join(''));
    }

    /* Events */
    function resetSidebar() {
        fullListSearch();
        collapse();
    }

    sidebarNav.on('click', '#modules_list', function (e) {
        fillSidebarWithNodes(sidebarNodes, "modules");
        resetSidebar();
        performSearch();
        e.preventDefault();
    });

    sidebarNav.on('click', '#exceptions_list', function (e) {
        fillSidebarWithNodes(sidebarNodes, "exceptions");
        resetSidebar();
        performSearch();
        e.preventDefault();
    });

    sidebarNav.on('click', '#protocols_list', function (e) {
        fillSidebarWithNodes(sidebarNodes, "protocols");
        resetSidebar();
        performSearch();
        e.preventDefault();
    });

    $('[data-toggle="offcanvas"]').on('click', function () {
        $('.row-offcanvas').toggleClass('active');
        $('#content').toggleClass('offcanvas-active');
    });

    $('#search button').on('click', function () {
        $('#search input').val('').focus();
        $('#search button span.glyphicon-remove').addClass('glyphicon-search').removeClass('glyphicon-remove');
        showAllResults();
    });

    $(document).on('keyup', function (e) {
        var searchInput = $('#search input');
        if (e.keyCode === 27 && searchInput.val() !== '') { // escape key maps to 27
            searchInput.val('').focus();
            showAllResults();
        }
    });

    $('#search input').on('keypress', function (e) {
        if (e.which === 13) { // enter key maps to 13
            var firstLinkFound = document.querySelectorAll('#full_list li.found a.object_link')[0];
            firstLinkFound.click();
        }
    });

    $('#search input').on('input', function () {
        performSearch();
    });

    // Setup Highlight.js
    hljs.configure({
        tabReplace: '    ', // 4 spaces
        languages: [] // disable auto-detect
    });

    window.fillSidebarWithNodes = fillSidebarWithNodes;
    $(fullListSearch);
    $(collapse);
    hljs.initHighlighting();
    identifyExternalLinks('#content');
}());
