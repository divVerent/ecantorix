$ESPEAK = 'espeak -v "$VOICE" ${VOICE_PATH:+--path="$VOICE_PATH"} -z -s "$SPEED" -q --pho -m "$SYLLABLE" | cut -f 1-2 | mbrola -e /usr/share/mbrola/"${VOICE#mb-}"/"${VOICE#mb-}" - temp.wav && rubberband -F -f`echo $PITCH / 100 | bc -l` temp.wav "$OUT"';
$ANALYZE_MINFREQ = 100;
$ANALYZE_MAXFREQ = 300;
$ESPEAK_SPEED_MAX = 200;
$ESPEAK_PITCH_MIN = 25;
$ESPEAK_PITCH_START = 100;
$ESPEAK_PITCH_MAX = 400;
$ESPEAK_PITCH_FACTOR = sub { return $_[0] / 100; };
