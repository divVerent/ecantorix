ALL = sadi-moma.wav sadi-moma-pitchbend.wav sadi-moma-f1.wav nacht-v.wav

all: $(ALL)

espeak-data:
	cp -R /usr/share/espeak-data/ espeak-data

%.mid: %.abc
	abc2midi $< 0 -o $@

%.mmp: %.mid %.conf espeak-data
	cp extravoices/* espeak-data/voices/\!v
	perl ../ecantorix.pl $< $*.conf > $@

%.wav: %.mmp
	lmms -o $@ --render $<

clean:
	$(RM) -r espeak-data
	$(RM) *.wav *.mid *.mmp
