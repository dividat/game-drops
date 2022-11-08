export function setup(app) {
    // Fix up prefixing
    var AudioContext = window.AudioContext || window.webkitAudioContext
    var audioCtx = new AudioContext()

    // Reset AudioContext after inactivity to address missing sound issue
    window.addEventListener(
        'visibilitychange',
        function() {
            if (!window.document.hidden) {
                audioCtx.close()
                audioCtx = new AudioContext()
            }
        },
        false
    )

    var audioBuffers = {}

    app.ports.loadSound &&
        app.ports.loadSound.subscribe(function(url) {
            var request = new XMLHttpRequest()
            request.open('GET', url, true)
            request.responseType = 'arraybuffer'

            request.onload = function() {
                audioCtx.decodeAudioData(
                    request.response,
                    function(buffer) {
                        audioBuffers[url] = buffer
                    },
                    function(err) {
                        console.error(
                            'WebAudio Error: Failed to load ' + url,
                            err
                        )
                    }
                )
            }
            request.send()
        })

    app.ports.playSound &&
        app.ports.playSound.subscribe(function(url) {
            // WORKAROUND Chrome v66 introduces a new autoplay policy that only allows
            // playing sounds under certain conditions:
            // Announcement: https://developers.google.com/web/updates/2017/09/autoplay-policy-changes
            // Related ticket: https://bugs.chromium.org/p/chromium/issues/detail?id=835767
            // Because we can not reasonably detect when playback is first possible,
            // we always try to resume the AudioContext if it is suspended.
            if (audioCtx.state === 'suspended') {
                var timeWhenCalled = Date.now()
                audioCtx
                    .resume()
                    .then(function() {
                        // Play sound only if within 50 ms of ideal invocation
                        // https://gamedev.stackexchange.com/questions/74973/maximum-audio-delay-before-the-player-notices
                        var delayedBy = Date.now() - timeWhenCalled
                        if (delayedBy < 50) {
                            play(url)
                        } else {
                            console.warn(
                                'Skipped playing `%s`, delay would have been %d ms.',
                                url,
                                delayedBy
                            )
                        }
                    })
                    .catch(function() {
                        console.info('Unable to resume AudioContext.')
                    })
            } else {
                play(url)
            }
        })

    function play(url) {
        var buffer = audioBuffers[url]
        if (buffer != null && audioCtx.state === 'running') {
            var source = audioCtx.createBufferSource()
            source.buffer = buffer
            source.connect(audioCtx.destination)
            source.start(0)
        }
    }
}
