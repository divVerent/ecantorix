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

	perl ecantorix.pl foo.mid > foo.mmp

The result is a LMMS project file that can be processed to an audio file using::

	lmms -o foo.wav --render foo.mmp

