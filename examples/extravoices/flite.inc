$SOX_PROCESS_IN_TO_S16LE = 'sox "$IN" -t raw -r "$RATE" -e signed -b 16 -c $PREEFFECTS_CHANNELS - remix - $PREEFFECTS silence 1 1s -30d reverse silence 1 1s -30d reverse';

$ESPEAK_PITCH_MIN = 32;
$ESPEAK_PITCH_START = 250;
$ESPEAK_PITCH_MAX = 500;
$ESPEAK_PITCH_FACTOR = sub { return $_[0]; };

$ANALYZE_MINFREQ = 200;
$ANALYZE_MAXFREQ = 300;

$ESPEAK_SPEED_MIN = 20;
$ESPEAK_SPEED_START = 100;
$ESPEAK_SPEED_MAX = 500;

$ESPEAK = 'flite -voice "$VOICE" -s duration_stretch=`echo 100/"$SPEED" | bc -l` -s int_f0_target_mean="$PITCH" -s int_f0_target_stddev=0 -o "$OUT" -t "$SYLLABLE"';
