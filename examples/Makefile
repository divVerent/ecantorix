ECANTORIX = ../ecantorix.pl

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
	cp extravoices/* espeak-data/voices/\!v

%.mid: %.abc
	abc2midi $< 0 -o $@

%.mmp: %.mid %.conf espeak-data
	mkdir -p cache
	$(ECANTORIX) -C $*.conf -c cache -O mmp -o $@ $<

%.wav: %.mid %.conf espeak-data
	mkdir -p cache
	$(ECANTORIX) -C $*.conf -c cache -O wav -o $@ $<

%-xon.mid: %.mid %.conf espeak-data
	mkdir -p cache
	$(ECANTORIX) -C $*.conf -c cache -O mid --output-midi-prefix=vocals: -o $@ $<

clean:
	$(RM) -r espeak-data cache
	$(RM) *.wav *.mid *.mmp *.pitch
