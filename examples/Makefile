ALL = sadi-moma.wav sadi-moma-pitchbend.wav sadi-moma-f1.wav

all: $(ALL)

%.mid: %.abc
	abc2midi $< 0 -o $@

%.mmp: %.mid %.conf
	perl ../ecantorix.pl $< $*.conf > $@

%.wav: %.mmp
	lmms -o $@ --render $<

clean:
	$(RM) *.wav *.mid *.mmp
