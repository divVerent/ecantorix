=========
eCantorix
=========

::

       (*)       (*)
        |         |
        |--=====--|
        |  |||||  |
        |  |||||  |
        ;  |||||  :
       (   |||||   )
        \  |||||  /
         `-=====-'

eCantorix is a singing synthesis frontend for espeak. It works by using espeak
to generate raw speech samples, then adjusting their pitch and length and
finally creating a LMMS project file referencing the samples in sync to the
input file.

Dependencies
============

* espeak
* sox
* LMMS
* Perl
  * Config::Tiny
  * MIDI::Opus
  * Math::FFT

How to use
==========

The input file for eCantorix is a slightly more strict MIDI karaoke (.kar) file.

Lyrics have to be stored as text events in the same track as the notes
associated with them.

A lyrics event then covers all notes starting between itself and the next
text event.

You can use an empty text event to end the range of notes covered by lyrics.

In case the range of a lyrics event covers multiple notes, the notes must be
all connected (legato), as they will be performed as a single audio sample
with pitch shifting. This feature however yields poor quality lyrics and is
not recommended - it tends to sound better to rather use separate lyrics events
and thus split a syllable into two, like "soft" -> "saw-oft".

For polyphony, you can either use multiple tracks (with each having their own
instance of the lyrics), or a single track and have the notes placed on multiple
MIDI channels (they then will share the lyrics).

Once you have such an input file, run::

	perl ecantorix.pl -O mmp -o foo.mmp foo.mid

The result is a LMMS project file that can be processed to an audio file using::

	lmms -o foo.wav --render foo.mmp

You can also render to a wave file directly:

	perl ecantorix.pl -O wav -o foo.wav foo.mid

Also, there is now preliminary support for Vocaloid (VSQ) files. The phonemes
are not used yet, though, as doing this would require an external mapping table
that depends on the Vocaloid voice in use.

License
=======

This software, as well as espeak, is licensed under the GPL version 3 (see the
included file COPYING) or any later version, at your choice.

The author believes that the license of the program does not affect licensing
or copyright of the output files; therefore you can use the output files for
any purpose of your liking.

The example files are licensed as follows:

examples/entchen.abc
        This is a wellknown German children's tune.
        Public domain (unknown author, but sure dead for more than 70 years).

examples/nacht-v.abc
        This is from the Zauberflöte (Magic Flute).
        Public domain (Wolfgang Amadeus Mozart).

examples/sadi-moma.abc
        This is the Free Software Song.
        No copyright is claimed on this song or its ABC typesetting by me,
        Richard Stallman, or the FSF.

examples/sadi-moma-pitchbend.abc 
        This is the Free Software Song.
        No copyright is claimed on this song or its ABC typesetting by me,
        Richard Stallman, or the FSF.

examples/sadi-moma-qon.abc
        This is the Free Software Song.
        No copyright is claimed on this song or its ABC typesetting by me,
        Richard Stallman, or the FSF.

examples/sarastro.abc
        This is from the Zauberflöte (Magic Flute).
        Public domain (Wolfgang Amadeus Mozart).
