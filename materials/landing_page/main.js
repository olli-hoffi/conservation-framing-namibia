/* ─────────────────────────────────────────────────────────
   Namibia Water Crisis — Main JavaScript
   ───────────────────────────────────────────────────────── */

(function () {
    'use strict';

    // ── Element refs ───────────────────────────────────────
    const section       = document.getElementById('scrolly-section');
    const video         = document.getElementById('scrolly-video');
    const loader        = document.getElementById('scrolly-loader');
    const loaderBar     = document.getElementById('scrolly-loader-bar');
    const card0         = document.getElementById('scrolly-card-0');
    const card1         = document.getElementById('scrolly-card-1');
    const card2         = document.getElementById('scrolly-card-2');
    const card3         = document.getElementById('scrolly-card-3');
    const card4         = document.getElementById('scrolly-card-4');
    const progressBar   = document.getElementById('scroll-progress-bar');
    const cookieBanner    = document.getElementById('cookie-banner');
    const cookieAccept    = document.getElementById('cookie-accept');
    const cookieReject    = document.getElementById('cookie-reject');
    const cookieCustomize = document.getElementById('cookie-customize');
    const cookieSave      = document.getElementById('cookie-save');
    const cookieOptions   = document.getElementById('cookie-options');
    const cookieReopen    = document.getElementById('cookie-reopen');
    const consentAnalytics= document.getElementById('consent-analytics');
    const navSurveyBtn  = document.getElementById('nav-survey-btn');
    const surveyCTABtn  = document.getElementById('survey-cta-btn');
    const shareToast    = document.getElementById('share-toast');

    // ── Survey URL (SoSciSurvey) ───────────────────────────
    // The condition rides along as ?r=<condition>_<variant> (e.g. empathy_1).
    // SoSci stores the r value automatically in the REF variable, so no SoSci
    // config is needed. Direct (non-ad) visits send r=direct.
    const SURVEY_URL = 'https://www.soscisurvey.de/water-crisis/';

    // ── UTM condition from URL (for H4 segmentation) ───────
    const utmParams  = new URLSearchParams(location.search);
    const condition  = utmParams.get('utm_content') || 'direct';
    const medium     = utmParams.get('utm_medium')  || 'organic';

    // ── GA4 helper (fires only after consent) ──────────────
    window._ga = function (eventName, params) {
        if (typeof gtag === 'function') {
            gtag('event', eventName, params || {});
        }
    };

    // ── Card fade helpers ──────────────────────────────────
    function ramp(v, lo, hi) {
        return Math.max(0, Math.min(1, (v - lo) / (hi - lo)));
    }

    function cardOpacity(p, inLo, inHi, outLo, outHi) {
        if (outLo === undefined) return ramp(p, inLo, inHi);
        if (p < inLo)  return 0;
        if (p < inHi)  return ramp(p, inLo, inHi);
        if (p < outLo) return 1;
        if (p < outHi) return 1 - ramp(p, outLo, outHi);
        return 0;
    }

    function applyCard(el, opacity) {
        el.style.opacity       = opacity;
        el.style.pointerEvents = opacity > 0.05 ? 'auto' : 'none';
    }

    function updateCards(p) {
        applyCard(card0, cardOpacity(p, -0.05, 0.0,  0.06, 0.16));
        applyCard(card1, cardOpacity(p,  0.18, 0.24, 0.33, 0.40));
        applyCard(card2, cardOpacity(p,  0.45, 0.52, 0.62, 0.68));
        applyCard(card3, cardOpacity(p,  0.70, 0.76, 0.83, 0.89));
        applyCard(card4, cardOpacity(p,  0.86, 0.95));
    }

    // ── Scroll progress ────────────────────────────────────
    function getProgress() {
        const scrollable = section.offsetHeight - window.innerHeight;
        if (scrollable <= 0) return 0;
        return Math.max(0, Math.min(1, window.scrollY / scrollable));
    }

    // ── rAF-throttled scroll handler ───────────────────────
    // Seeks video + updates cards + updates scroll progress bar.
    // Throttling prevents seek-request pileup on fast scroll.
    let rafPending = false;

    function onScroll() {
        if (rafPending) return;
        rafPending = true;
        requestAnimationFrame(function () {
            rafPending = false;
            const p = getProgress();
            if (video.readyState >= 1 && video.duration) {
                video.currentTime = p * video.duration;
            }
            updateCards(p);
            if (progressBar) progressBar.style.width = (p * 100) + '%';
        });
    }

    window.addEventListener('scroll', onScroll, { passive: true });

    // ── Intro: animate currentTime via rAF for ~2 s ────────
    function runIntro(onDone) {
        var introDur = Math.min(video.duration || 3, 2.5);
        var totalMs  = 2000;
        var startMs  = performance.now();

        function tick(now) {
            var t = Math.min(1, (now - startMs) / totalMs);
            video.currentTime = t * introDur;
            if (loaderBar) loaderBar.style.width = (t * 100) + '%';
            updateCards(0);
            if (t < 1) {
                requestAnimationFrame(tick);
            } else {
                video.currentTime = 0;
                onDone();
            }
        }
        requestAnimationFrame(tick);
    }

    // ── Hide loader ────────────────────────────────────────
    function hideLoader() {
        if (!loader) return;
        loader.style.opacity = '0';
        setTimeout(function () { loader.style.display = 'none'; }, 700);
    }

    // ── Start: prime decoder, then run intro ───────────────
    // Safari (desktop + iOS) will not render frames on a paused,
    // never-played video when currentTime is set programmatically.
    // A single play() → pause() cycle primes the decoder.
    function start() {
        function doIntro() {
            runIntro(function () {
                onScroll();
                hideLoader();
                showCookieBanner();
            });
        }

        var promise = video.play();
        if (promise && typeof promise.then === 'function') {
            promise
                .then(function () {
                    video.pause();
                    doIntro();
                })
                .catch(function () {
                    doIntro();
                });
        } else {
            video.pause();
            doIntro();
        }
    }

    // ── Boot ───────────────────────────────────────────────
    if (video.readyState >= 2) {
        start();
    } else {
        video.addEventListener('canplay', function () {
            start();
        }, { once: true });

        if (video.readyState === 0) {
            video.load();
        }

        // Safety: force-hide loader after 6 s
        setTimeout(function () {
            if (loader && loader.style.display !== 'none') {
                hideLoader();
                showCookieBanner();
            }
        }, 6000);
    }

    // ── Consent management (GDPR / TDDDG) ──────────────────
    // GA4 loads ONLY after the user grants analytics consent. Until the real
    // Measurement ID is set in GA4_ID, nothing analytics-related loads at all.
    var CONSENT_KEY = 'cookieConsent';   // localStorage: JSON {analytics:bool, v:1}
    var GA4_ID      = 'G-SHQ44MCBQC';    // GA4 Measurement ID (Namibia Water Crisis web stream)
    var ga4Loaded   = false;

    function readConsent() {
        try {
            var raw = localStorage.getItem(CONSENT_KEY);
            return raw ? JSON.parse(raw) : null;
        } catch (e) { return null; }
    }

    function saveConsent(analyticsGranted) {
        try {
            localStorage.setItem(CONSENT_KEY, JSON.stringify({ analytics: !!analyticsGranted, v: 1 }));
        } catch (e) {}
    }

    function loadGA4() {
        if (ga4Loaded) return;
        if (!GA4_ID || GA4_ID === 'G-XXXXXXXX') return;   // not configured yet
        ga4Loaded = true;
        var s = document.createElement('script');
        s.async = true;
        s.src = 'https://www.googletagmanager.com/gtag/js?id=' + GA4_ID;
        document.head.appendChild(s);
        window.dataLayer = window.dataLayer || [];
        window.gtag = function () { dataLayer.push(arguments); };
        gtag('js', new Date());
        gtag('config', GA4_ID, {
            anonymize_ip: true,
            allow_google_signals: false,
            allow_ad_personalization_signals: false
        });
    }

    function deleteAnalyticsCookies() {
        document.cookie.split(';').forEach(function (c) {
            var name = c.split('=')[0].trim();
            if (name.indexOf('_ga') === 0 || name.indexOf('_gid') === 0 || name.indexOf('_gat') === 0) {
                document.cookie = name + '=; expires=Thu, 01 Jan 1970 00:00:00 GMT; path=/';
            }
        });
    }

    // Shown automatically (after the intro) only if no choice has been made yet.
    function showCookieBanner() {
        if (!cookieBanner) return;
        if (readConsent() !== null) return;
        cookieBanner.classList.add('visible');
    }

    function hideCookieBanner() {
        if (cookieBanner) cookieBanner.classList.remove('visible');
    }

    // Reopened from the footer "Cookie settings" link: always shows + reflects state.
    function openCookieSettings() {
        if (!cookieBanner) return;
        var consent = readConsent();
        if (consentAnalytics) consentAnalytics.checked = !!(consent && consent.analytics);
        if (cookieOptions)   cookieOptions.hidden   = false;
        if (cookieSave)      cookieSave.hidden       = false;
        if (cookieCustomize) cookieCustomize.hidden  = true;
        cookieBanner.classList.add('visible');
    }

    if (cookieAccept) cookieAccept.addEventListener('click', function () {
        saveConsent(true); loadGA4(); hideCookieBanner();
    });
    if (cookieReject) cookieReject.addEventListener('click', function () {
        saveConsent(false); deleteAnalyticsCookies(); hideCookieBanner();
    });
    if (cookieCustomize) cookieCustomize.addEventListener('click', function () {
        if (cookieOptions) cookieOptions.hidden = false;
        if (cookieSave)    cookieSave.hidden    = false;
        cookieCustomize.hidden = true;
    });
    if (cookieSave) cookieSave.addEventListener('click', function () {
        var granted = !!(consentAnalytics && consentAnalytics.checked);
        saveConsent(granted);
        if (granted) { loadGA4(); } else { deleteAnalyticsCookies(); }
        hideCookieBanner();
    });
    if (cookieReopen) cookieReopen.addEventListener('click', openCookieSettings);

    // On boot: if analytics was previously granted, load GA4 right away.
    (function () {
        var consent = readConsent();
        if (consent && consent.analytics) loadGA4();
    })();

    // ── Toast helper ───────────────────────────────────────
    function showToast(msg) {
        if (!shareToast) return;
        shareToast.textContent = msg;
        shareToast.classList.add('visible');
        setTimeout(function () {
            shareToast.classList.remove('visible');
        }, 2800);
    }

    // ── Survey Buttons ─────────────────────────────────────
    function openSurvey() {
        if (!SURVEY_URL || SURVEY_URL.charAt(0) === '#') {
            showToast('Survey launching soon, check back!');
            return;
        }
        var sep = SURVEY_URL.indexOf('?') === -1 ? '?' : '&';
        var url = SURVEY_URL + sep + 'r=' + encodeURIComponent(condition);
        window.open(url, '_blank', 'noopener');
        window._ga('survey_click', { condition: condition, medium: medium });
    }

    if (navSurveyBtn) navSurveyBtn.addEventListener('click', openSurvey);
    if (surveyCTABtn) surveyCTABtn.addEventListener('click', openSurvey);

    // ── Time-on-Page Tracker (H4) ──────────────────────────
    // Records total active time on page, fires as GA4 custom event
    // on page unload. Segments by utm_content (condition).
    var pageStartTime = performance.now();
    var hiddenAt      = null;
    var hiddenTotal   = 0;

    document.addEventListener('visibilitychange', function () {
        if (document.hidden) {
            hiddenAt = performance.now();
        } else {
            if (hiddenAt !== null) {
                hiddenTotal += performance.now() - hiddenAt;
                hiddenAt = null;
            }
        }
    });

    function sendTimeOnPage() {
        var totalMs  = performance.now() - pageStartTime;
        var activeMs = Math.round(totalMs - hiddenTotal);

        // GA4 custom event
        window._ga('time_on_page', {
            active_ms:    activeMs,
            condition:    condition,
            medium:       medium,
            scroll_depth: Math.round(getProgress() * 100)
        });

        // sendBeacon fallback for unload (most reliable)
        if (navigator.sendBeacon && typeof gtag === 'function') {
            // GA4 handles this via event — beacon is a safety net
        }
    }

    window.addEventListener('pagehide', sendTimeOnPage);

    // Also fire at 30s, 60s, 120s as heartbeats (for mid-session data)
    [30000, 60000, 120000].forEach(function (ms) {
        setTimeout(function () {
            if (!document.hidden) {
                window._ga('time_heartbeat', {
                    elapsed_ms: ms,
                    condition:  condition
                });
            }
        }, ms);
    });

})();
