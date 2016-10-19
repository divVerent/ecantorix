ECANTORIX = ../ecantorix.pl
ESPEAK_DATA = /usr/lib/x86_64-linux-gnu/espeak-data

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
	cp -R $(ESPEAK_DATA)/* espeak-data/
	cp -R ~/espeak-data/* espeak-data/ || true
	cp extravoices/* espeak-data/voices/\!v

%.mid: %.abc
	abc2midi $< 0 -o $@

%.mmp %.ass: %.mid %.conf espeak-data
	mkdir -p cache
	$(ECANTORIX) -C $*.conf -c cache -O mmp -o $*.mmp $< > $*.ass

%.wav %.ass: %.mid %.conf espeak-data
	mkdir -p cache
	$(ECANTORIX) -C $*.conf -c cache -O wav -o $*.wav $< > $*.ass

%-xon.mid %.ass: %.mid %.conf espeak-data
	mkdir -p cache
	$(ECANTORIX) -C $*.conf -c cache -O mid --output-mid-prefix=vocals: -o $*-xon.mid $< > $*.ass

clean:
	$(RM) -r espeak-data cache
	$(RM) *.wav *.mid *.mmp *.ass
