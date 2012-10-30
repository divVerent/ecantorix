do 'extravoices/rubberband.inc';
$ESPEAK_PITCH_MIN = $ESPEAK_PITCH_MAX = $ESPEAK_PITCH_START; # we have no pitch control
$ESPEAK = 'espeak -v "$VOICE" ${VOICE_PATH:+--path="$VOICE_PATH"} -z -s "$SPEED" -q --pho -m "$SYLLABLE" | cut -f 1-2 | mbrola -e /usr/share/mbrola/"${VOICE#mb-}"/"${VOICE#mb-}" - "$OUT"';
$ANALYZE_MINFREQ = 100;
$ANALYZE_MAXFREQ = 300;
$ESPEAK_SPEED_MAX = 200;
