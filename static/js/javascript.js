window.mainInit = null;

(function() {
    window.mainInit = function() {
        let reportError = function(error) {
            $.ajax({url: '/api/log-error.json',
                    data: {msg: error.toString()}});
            console.error(error);
        };

        function sleep(ms) {
            return new Promise(resolve => setTimeout(resolve, ms));
        }

        let waitForElement = function(selector, cont) {
            console.log('> waitForElement', selector);
            let go = function(i) {
                if (i <= 0) {
                    reportError('Exhausted waiting for ' + selector);
                } else {
                    if ($(selector).length) {
                        console.log('> found ' + selector);
                        cont();
                    } else {
                        console.log('> waitForElement sleeping for ' + selector, "attempt", i);
                        sleep(500).then(() => {
                            go(i-1);
                        });
                    }
                }
            };
            go(5);
        }

        var app = Elm.Index.init({node: document.getElementById("app")});
        app.ports.needLoginRedirect.subscribe(function(data) {
            try {
                let loc = window.location.href;
                window.localStorage.setItem('afterSigninRedirectUrl', loc);
                window.location.href = '/login';
            } catch (error) {
                reportError(error);
            }
        });
        app.ports.redirectBackAfterLogin.subscribe(function(data) {
            try {
                let loc = window.localStorage.getItem('afterSigninRedirectUrl');
                if (loc) {
                    window.localStorage.removeItem('afterSigninRedirectUrl');
                    window.location.href = loc;
                } else {
                    window.location.href = '/dashboard';
                }
            } catch (error) {
                reportError(error);
            }
        });
        app.ports.initTooltips.subscribe(function(data) {
            try {
                console.log('> initTooltips', data);
                $('[data-toggle="tooltip"]').tooltip();
            } catch (error) {
                reportError(error);
            }
        });
        /* app.ports.tooltipToggle.subscribe(function(data) {
         *     // data.id - tooltip element id
         *     try {
         *         console.log('> tooltipToggle', data);
         *         $('#' + data.id).tooltip('toggle');
         *     } catch (error) {
         *         reportError(error);
         *     }
         * });
         * app.ports.tooltipShow.subscribe(function(data) {
         *     // data.id - tooltip element id
         *     try {
         *         console.log('> tooltipShow', data);
         *         $('#' + data.id).tooltip('show');
         *     } catch (error) {
         *         reportError(error);
         *     }
         * });
         * app.ports.tooltipHide.subscribe(function(data) {
         *     // data.id - tooltip element id
         *     try {
         *         console.log('> tooltipHide', data);
         *         $('#' + data.id).tooltip('hide');
         *     } catch (error) {
         *         reportError(error);
         *     }
         * }); */
    };
})();
