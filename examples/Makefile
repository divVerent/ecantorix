ALL = \
      entchen.wav \
      nacht-v.wav \
      sadi-moma-pitchbend.wav \
      sadi-moma-qon.wav \
      sadi-moma.wav \
      sarastro.wav \

all: $(ALL)

espeak-data:
	$(RM) -r espeak-data
	mkdir espeak-data
	cp -R /usr/share/espeak-data/* espeak-data/
	cp -R ~/espeak-data/* espeak-data/ || true

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
