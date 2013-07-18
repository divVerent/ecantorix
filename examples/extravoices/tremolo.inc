# better quality
$SOX_AFTEREFFECTS .= "channels 2 ";
$SOX_AFTEREFFECTS_CHANNELS = 2;
$SOX_RATE = 44100;

# tremolo and reverb
$ESPEAK_CACHE_PREFIX .= "tremolo";
$SOX_AFTEREFFECTS .= "pad 0 1 tremolo 9 50 reverb ";
