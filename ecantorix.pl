#!/usr/bin/perl
#
#   eCantorix - singing speech synthesis using eSpeak
#   Copyright (C) 2012  Rudolf Polzer
#
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 3 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program.  If not, see <http://www.gnu.org/licenses/>.

use strict;
use warnings;
use MIDI;
use MIDI::Opus;
use Math::FFT;
use Cwd;

# from wavegen.cpp
# set from y = pow(2,x) * 128,  x=-1 to 1
our @pitch_adjust_tab = (
    64, 65, 66, 67, 68, 69, 70, 71,
    72, 73, 74, 75, 76, 77, 78, 79,
    80, 81, 82, 83, 84, 86, 87, 88,
    89, 91, 92, 93, 94, 96, 97, 98,
   100,101,103,104,105,107,108,110,
   111,113,115,116,118,119,121,123,
   124,126,128,130,132,133,135,137,
   139,141,143,145,147,149,151,153,
   155,158,160,162,164,167,169,171,
   174,176,179,181,184,186,189,191,
   194,197,199,202,205,208,211,214,
   217,220,223,226,229,232,236,239,
   242,246,249,252, 254,255
);

# these variables can be overridden using the same syntax from a control file
# e.g. to be able to use a different speaker voice
our $ESPEAK_ATTEMPTS = 8;
our $ESPEAK_TRANSPOSE = -24;
our $ESPEAK_PITCH_MIN = 0;
our $ESPEAK_PITCH_START = 50;
our $ESPEAK_PITCH_MAX = 99;
our $ESPEAK_SPEED_MIN = 80;
our $ESPEAK_SPEED_START = 175;
our $ESPEAK_SPEED_MAX = 450;
our $ESPEAK_PITCH_CACHE = 1;
#our $ESPEAK_PITCH_FACTOR = sub { return $pitch_adjust_tab[$_[0]]; }; # works only for voices with zero range
our $ESPEAK_PITCH_FACTOR;
our $ESPEAK = 'espeak -z -p "$PITCH" -s "$SPEED" -w "$OUT" -m "<prosody range=\"0\">$SYLLABLE</prosody>"';
our $ESPEAK_CACHE = getcwd();
our $ESPEAK_CACHE_PREFIX = "note";
our $SOX_RATE = 22050;
our $SOX_PROCESS_IN_TO_S16LE = 'sox "$IN" -t raw -r "$RATE" -e signed -b 16 -c 1 - remix - silence 1 1s 0 reverse silence 1 1s 0 reverse';
our $SOX_PROCESS_TEMPO_PITCHBEND_S16LE_TO_OUT = 'sox -t raw -r "$RATE" -e signed -b 16 -c 1 - "$OUT" tempo -s "$TEMPO" $PITCHBEND';
our $ANALYZE_MINFREQ = 20;
our $ANALYZE_MAXFREQ = 500;
our $ANALYZE_BIAS = 1.4;
our $ANALYZE_RANGE1 = 2;
our $ANALYZE_RANGE2 = 16;
our $ANALYZE_ENV = 3;
our $OUTPUT_FORMAT = 'lmms';
our $OUTPUT_MIDI_PREFIX = 'vocals:';
# end of customizable variables

my ($filename, $controlfile) = @ARGV;

if(length $controlfile)
{
	do "$controlfile";
}

my $opus = MIDI::Opus->new({from_file => $filename});
my $tracks = $opus->tracks_r();

sub abstime(@)
{
	my $t = 0;
	return map {
		[$_->[0], $t += $_->[1], @{$_}[2..(@$_-1)]];
	} @_;
}

sub reltime(@)
{
	my $t = 0;
	return map {
		my $tsave = $t;
		$t = $_->[1];
		[$_->[0], $t - $tsave, @{$_}[2..(@$_-1)]];
	} @_;
}

sub tick2lmms($)
{
	return int(48 / $opus->ticks() * $_[0] + 0.5);
}
my %out = (
	lmms => {
		header => sub
		{
			my ($self, $totallen, $tempi) = @_;
			my $lmms_totallen = tick2lmms $totallen;

			print <<EOF;
<?xml version="1.0"?>
<!DOCTYPE multimedia-project>
<multimedia-project version="1.0" creator="Linux MultiMedia Studio (LMMS)" creatorversion="0.4.13" type="song">
	<head timesig_numerator="4" mastervol="100" timesig_denominator="4" masterpitch="0">
		<bpm value="120" id="3481048"/>
	</head>
	<song>
		<trackcontainer width="600" x="5" y="5" maximized="0" height="300" visible="1" type="song" minimized="0">
			<track muted="0" type="5" name="Automation track">
				<automationtrack/>
				<automationpattern name="Tempo" pos="0" len="$lmms_totallen">
EOF
			for(@$tempi)
			{
				my $lmms_tick = tick2lmms($_->[0]);
				# x [sec/tick]
				# x/60 [min/tick]
				# 60/x [tick/min]
				# (60/$opus->ticks())/x [quarter/min]
				my $lmms_tempo = int((60 / $opus->ticks()) / $_->[1] + 0.5);
				print <<EOF;
					<time value="$lmms_tempo" pos="$lmms_tick"/>
EOF
}
			print <<EOF;
					<object id="3481048"/>
				</automationpattern>
			</track>
			<track muted="0" type="2" name="Sample track">
				<sampletrack vol="100">
					<fxchain numofeffects="0" enabled="0"/>
				</sampletrack>
EOF
		},
		sample => sub {
			my ($self, $tick, $dtick, $outname) = @_;
			my $lmms_tick = tick2lmms($tick);
			my $lmms_dtick = tick2lmms($tick + $dtick) - $lmms_tick;
			print <<EOF;
				<sampletco muted="0" pos="$lmms_tick" len="$lmms_dtick" src="$outname" />
EOF
		},
		footer => sub {
			my ($self) = @_;
			print <<EOF;
			</track>
		</trackcontainer>
	</song>
</multimedia-project>
EOF
		}
	},
	midi => {
		header => sub {
			my ($self, $totallen, $tempi) = @_;
			$self->{opus} = MIDI::Opus->new();
			$self->{opus}->format(1);
			$self->{opus}->ticks($opus->ticks());
			my $metatrack = MIDI::Track->new();
			$metatrack->events(reltime map {
				['set_tempo', $_->[0], $_->[1] * $opus->ticks() / 0.000001]
			} @$tempi);
			my $notetrack = MIDI::Track->new();
			$self->{opus}->tracks($metatrack, $notetrack);
		},
		sample => sub {
			my ($self, $tick, $dtick, $outname) = @_;
			my $track = $self->{opus}->tracks_r()->[-1];
			$outname =~ s/^.*\///;
			$track->new_event(
				'text_event', $tick, "$OUTPUT_MIDI_PREFIX$outname");
		},
		footer => sub {
			my ($self) = @_;
			my $track = $self->{opus}->tracks_r()->[-1];
			$track->events(reltime $track->events());
			$self->{opus}->write_to_handle(\*STDOUT);
			delete $self->{opus};
		}
	}
);
my $out = $out{$OUTPUT_FORMAT};
my $out_self = {};

sub getfirstmaximum($$$)
{
	my ($correl, $mi, $ma) = @_;

	# we're looking for the first "big" maximum in the range

	my $best = $mi;
	my $bestscore = 0;
	for($mi .. $ma)
	{
		my $s = $correl->[$_];
		my $dist = $_ - $best;
		my $bias = $ANALYZE_BIAS;
		if($dist <= $ANALYZE_RANGE1)
		{
			$bias = 1;
		}
		elsif($dist < $ANALYZE_RANGE2)
		{
			$bias = ($bias - 1) * (($dist - $ANALYZE_RANGE1) / ($ANALYZE_RANGE2 - $ANALYZE_RANGE1)) + 1;
		}
		if($s > $bestscore * $bias)
		{
			$best = $_;
			$bestscore = $s;
		}
	}

	# now $best points APPROXIMATELY to the frequency... but is not correct
	# do a second search for a maximum around $best

#	print STDERR "Guess: $best\n";

	$mi = int($best * 0.67)
		if $mi < int($best * 0.67);
	$ma = int($best * 1.50)
		if $ma > int($best * 1.50);

	for($mi .. $ma)
	{
		my $s = $correl->[$_];
		if($s > $bestscore)
		{
			$best = $_;
			$bestscore = $s;
		}
	}

#	print STDERR "Refined: $best\n";

	return $best;
}

sub getpitch($$$$)
{
	my ($data, $mi, $ma, $debugfile) = @_;

	# pad for autocorrelation to work
	my $n = @$data;
	my $n2 = $n;
	--$n2;
	$n2 |= $n2 >> 16;
	$n2 |= $n2 >> 8;
	$n2 |= $n2 >> 4;
	$n2 |= $n2 >> 2;
	$n2 |= $n2 >> 1;
	++$n2;
	my @data = (@$data, (0) x (2 * $n2 - $n));
	my $fft = new Math::FFT(\@data);
	my $correl = $fft->correl($fft);

	$ma = int(@$correl / 2 - 1)
		if $ma > int(@$correl / 2 - 1);

	my $best = getfirstmaximum($correl, $mi, $ma);

	my $sf = 0;
	my $s = 0;
	for($best-$ANALYZE_ENV .. $best+$ANALYZE_ENV)
	{
		$s += $correl->[$_];
		$sf += $_ * $correl->[$_];
	}

	my $ret = $sf / $s;

#	print STDERR "Finished: $ret\n";

	if(defined $debugfile)
	{
		open my $fh, ">", $debugfile;
		for(0..@$correl-1)
		{
			my $valid = ($_ >= $mi && $_ <= $ma);
			my $diff = ($_ - $ret);
			print $fh "$_ $correl->[$_] $diff $valid\n";
		}
		close $fh;
		print STDERR "$debugfile written\n";
	}

	return $ret;
}

my @pitch_cache = ();
sub get_pitch_cached($);
sub get_pitch_cached($)
{
	my ($pitch) = @_;
	if($ESPEAK_PITCH_FACTOR)
	{
		if($pitch != $ESPEAK_PITCH_START)
		{
			my $base = get_pitch_cached($ESPEAK_PITCH_START);
			my $f = $ESPEAK_PITCH_FACTOR->($pitch) / $ESPEAK_PITCH_FACTOR->($ESPEAK_PITCH_START);
			return $f * $base;
		}
	}
	return $pitch_cache[$pitch]
		if exists $pitch_cache[$pitch];
	my @hz = ();
	print STDERR "Caching pitch $pitch... ";
	for my $syllable(qw/do re mi fa so la ti/)
	{
		for(1..5)
		{
			local $ENV{OUT} = "out.wav";
			local $ENV{PITCH} = $pitch;
			local $ENV{SPEED} = $ESPEAK_SPEED_MIN + int rand ($ESPEAK_SPEED_MAX - $ESPEAK_SPEED_MIN + 1);;
			local $ENV{SYLLABLE} = $syllable;
			0 == system $ESPEAK
				or die "$ESPEAK: $! $?";
			local $ENV{RATE} = $SOX_RATE;
			local $ENV{IN} = "out.wav";
			open my $fh, '-|', $SOX_PROCESS_IN_TO_S16LE
				or die "$SOX_PROCESS_IN_TO_S16LE: $!";
			my $data = do { undef local $/; <$fh>; };
			close $fh
				or die "$SOX_PROCESS_IN_TO_S16LE: $! $?";
			die "$SOX_PROCESS_IN_TO_S16LE: No data"
				unless length $data;

			my @data = unpack "s*", $data;
			my $thishz = $SOX_RATE / getpitch(\@data, $SOX_RATE / $ANALYZE_MAXFREQ, $SOX_RATE / $ANALYZE_MINFREQ, undef);

			push @hz, $thishz;
		}
	}
	my $median = [sort { $a <=> $b } @hz]->[(@hz - 1) / 2];
	print STDERR "$median\n";
	return $pitch_cache[$pitch] = $median;
}
sub find_pitch_cached($)
{
	my ($freq) = @_;

	# perform a binary search in the pitch cache

	my $mi = $ESPEAK_PITCH_MIN;
	my $ma = $ESPEAK_PITCH_MAX;
	my $mihz = get_pitch_cached($mi);
	return $mi
		if $freq <= $mihz;
	my $mahz = get_pitch_cached($ma);
	die "CACHE INCONSISTENCY: $mahz not > $mihz"
		if $mahz <= $mihz;
	return $ma
		if $freq >= $mahz;
	while($ma - $mi > 1)
	{
		my $cur = int(($mi + $ma) / 2);
		my $curhz = get_pitch_cached($cur);
		die "CACHE INCONSISTENCY: $curhz not > $mihz"
			if $curhz <= $mihz;
		die "CACHE INCONSISTENCY: $curhz not < $mahz"
			if $curhz >= $mahz;
		if($freq > $curhz)
		{
			$mi = $cur;
			$mihz = $curhz;
		}
		elsif($freq < $curhz)
		{
			$ma = $cur;
			$mahz = $curhz;
		}
		else
		{
			return $cur; # never gonna happen anyway
		}
	}
	if($freq > ($mihz + $mahz) / 2)
	{
		return $ma;
	}
	else
	{
		return $mi;
	}
}

sub play_note($$$$$$$)
{
	my ($tick, $dtick, $t, $dt, $note, $pitchbend, $syllable) = @_;

	# iterative find fitting -p (pitch, 0..99) and -s (speed, 80..450)
	# parameters for espeak

	$syllable =~ s/^\///g;
	$syllable =~ s/^\\//g;
	$syllable =~ s/^ //g;

	my $pitchbend_str = "";
	my $pitchbend_t = 0;
	for(@$pitchbend)
	{
		my ($start, $end, $semitones) = @$_;
		my $delay = $start - $pitchbend_t;
		my $duration = $end - $start;
		$pitchbend_t = $end;
		my $cents = $semitones * 100;
		$duration = 0.001
			if $duration < 0.001;
		$pitchbend_str .= sprintf " %.6f,%.1f,%.6f",
			$delay, $cents, $duration;
	}
	$pitchbend_str = "bend$pitchbend_str"
		if length $pitchbend_str;

	my $hz = 440 * 2**(($note - 69 + $ESPEAK_TRANSPOSE) / 12);
	my $pitch = $ESPEAK_PITCH_START;
	my $speed = $ESPEAK_SPEED_START;

	(my $pitchbend_fstr = $pitchbend_str) =~ s/ /_/g;
	my $outname = sprintf "%s/%s_%.2f_%.2f_%s_%s.wav",
		$ESPEAK_CACHE, $ESPEAK_CACHE_PREFIX, $dt, $hz, $pitchbend_fstr, $syllable;

	if(!-f $outname)
	{
		print STDERR "Rendering $outname...\n";
		my $thisdt;
		my $thishz;
		my $data;
		if($ESPEAK_PITCH_CACHE)
		{
			$pitch = find_pitch_cached($hz);
		}
		for(1..$ESPEAK_ATTEMPTS)
		{
			local $ENV{OUT} = "out.wav";
			local $ENV{PITCH} = $pitch;
			local $ENV{SPEED} = $speed;
			local $ENV{SYLLABLE} = $syllable;
			0 == system $ESPEAK
				or die "$ESPEAK: $! $?";
			local $ENV{RATE} = $SOX_RATE;
			local $ENV{IN} = "out.wav";
			open my $fh, '-|', $SOX_PROCESS_IN_TO_S16LE
				or die "$SOX_PROCESS_IN_TO_S16LE: $!";
			$data = do { undef local $/; <$fh>; };
			close $fh
				or die "$SOX_PROCESS_IN_TO_S16LE: $! $?";
			die "$SOX_PROCESS_IN_TO_S16LE: No data"
				unless length $data;

			$thisdt = (length($data) / 2) / $SOX_RATE;

			if($ESPEAK_PITCH_CACHE)
			{
				$thishz = get_pitch_cached($pitch);
			}
			else
			{
				my @data = unpack "s*", $data;
				$thishz = $SOX_RATE / getpitch(\@data, $SOX_RATE / $ANALYZE_MAXFREQ, $SOX_RATE / $ANALYZE_MINFREQ, undef);
				$pitch = [sort { $a <=> $b } (
					$ESPEAK_PITCH_MIN,
					int($pitch * $hz / $thishz + 0.5),
					$ESPEAK_PITCH_MAX)]->[1];
			}

			$speed = [sort { $a <=> $b } (
				$ESPEAK_SPEED_MIN,
				int($speed * $thisdt / $dt + 0.5),
				$ESPEAK_SPEED_MAX)]->[1];
		}

		my $pitchfix = $hz / $thishz;
		my $lengthfix = $dt / $thisdt;
		#print STDERR "Pitch correction: $thishz -> $hz\n";
		#print STDERR "Length correction: $thisdt -> $dt\n";
		if(!$ESPEAK_PITCH_CACHE)
		{
			if($pitchfix <= 0 || abs(log($pitchfix)) > 0.1)
			{
				warn "Large pitch correction: $thishz -> $hz";
				getpitch([unpack("s*", $data)], $SOX_RATE / $ANALYZE_MAXFREQ, $SOX_RATE / $ANALYZE_MINFREQ, "$outname.plot");
			}
		}
		my $rate0 = $SOX_RATE;

		my $tempofix = 1 / $lengthfix;
		my $rate = int(0.5 + $rate0 * $pitchfix);
		$tempofix *= $rate0 / $rate;;

		local $ENV{RATE} = $rate;
		local $ENV{TEMPO} = $tempofix;
		local $ENV{OUT} = $outname;
		local $ENV{PITCHBEND} = $pitchbend_str;
		open my $fh, '|-', $SOX_PROCESS_TEMPO_PITCHBEND_S16LE_TO_OUT
			or die "$SOX_PROCESS_TEMPO_PITCHBEND_S16LE_TO_OUT: $!";
		print $fh $data;
		close $fh
			or die "$SOX_PROCESS_TEMPO_PITCHBEND_S16LE_TO_OUT: $! $?";
	}
	$out->{sample}->($out_self, $tick, $dtick, $outname);
}

# we store tempo as seconds per tick
my @tempi = map {
	$_->[0] eq 'set_tempo'
		? ([$_->[1], $_->[2] * 0.000001 / $opus->ticks()])
		: ();
} abstime $opus->tracks_r()->[0]->events();
sub tick2sec($)
{
	my ($tick) = @_;
	my $sec = 0;
	my $curtempo = [0, 0.5 / $opus->ticks()];
	for(@tempi)
	{
		if($_->[0] < $tick)
		{
			# this event is in the past
			# we add the full time since the last one then
			$sec += ($_->[0] - $curtempo->[0]) * $curtempo->[1];
		}
		else
		{
			# if this event is in the future, we break
			last;
		}
		$curtempo = $_;
	}
	$sec += ($tick - $curtempo->[0]) * $curtempo->[1];
	return $sec;
};

my $totallen = 0;
for my $trackno(0..@$tracks-1)
{
	print STDERR "Processing track $trackno...\n";
	my $track = $tracks->[$trackno];
	my @events = abstime $track->events();
	$totallen = $events[-1][1]
		if @events and $events[-1][1] > $totallen;;
}

$out->{header}->($out_self, $totallen, \@tempi);

for my $trackno(0..@$tracks-1)
{
	print STDERR "Processing track $trackno...\n";
	my $track = $tracks->[$trackno];

	my @events = abstime $track->events();

	# lyrics = array of [starttick, text, channels]
	# channels = hash of channel => notes
	# notes = array of [starttick, ticks, pitch]
	my @lyrics = map {
		$_->[0] =~ /^lyrics$|^text_event$/ ? [$_->[1], $_->[2], {}] : ()
	} @events;
	my $insert_note = sub
	{
		my ($starttick, $ticks, $channel, $pitch) = @_;
		my $found = undef;
		for(@lyrics)
		{
			if($_->[0] <= $starttick)
			{
				$found = $_;
			}
		}
		if($found)
		{
			push @{$found->[2]{$channel}},
				[$starttick, $ticks, $pitch];
			return 1;
		}
		else
		{
			return undef;
		}
	};

	my @notes = ();
	my %notehash;
	for(@events)
	{
		$_->[0] = 'note_off' if $_->[0] eq 'note_on' and $_->[4] == 0;
		next
			if $_->[0] ne 'note_on' and $_->[0] ne 'note_off';
		my $time = $_->[1];
		my $channel = $_->[2];
		my $pitch = $_->[3];
		if($_->[0] eq 'note_on')
		{
			if(exists $notehash{$channel}{$pitch})
			{
				warn "note_already_on: $time/$channel/$pitch";
				next;
			}
			$notehash{$channel}{$pitch} = $time;
		}
		else
		{
			if(!exists $notehash{$channel}{$pitch})
			{
				warn "Spurious note_off: $time/$channel/$pitch";
				next;
			}
			my $starttime = $notehash{$channel}{$pitch};
			delete $notehash{$channel}{$pitch};

			$insert_note->(
				$starttime, $time - $starttime,
				$channel, $pitch)
				or warn "No lyrics found for note: $starttime/$time/$channel/$pitch";
		}
	}

	for(@lyrics)
	{
		my ($starttick, $text, $channels) = @$_;
		next
			if $text eq "";
		for my $channel(sort keys %$channels)
		{
			my $notes = $channels->{$channel};
			my $realstarttick = $notes->[0]->[0];
			my $realendtick = $notes->[-1]->[0] + $notes->[-1]->[1];
			my $realstarttime = tick2sec $realstarttick;
			my $realendtime = tick2sec $realendtick;
			next
				if $realendtime - $realstarttime > 10;
			my @pitchbend = ();
			my $lastnoteendtime = $realstarttime;
			my $sumpitch = 0;
			my $sumtime = 0;
			for(@$notes)
			{
				my ($notestarttick, $noteticks, $pitch) = @$_;
				my $noteendtick = $notestarttick + $noteticks;
				my $notestarttime = tick2sec($notestarttick);
				my $noteendtime = tick2sec($noteendtick);
				die "Overlapping notes: $notestarttime/$channel/$pitch"
					if $notestarttime < $lastnoteendtime;
				$sumtime += $noteendtime + $notestarttime;
				$sumpitch += ($noteendtime + $notestarttime) * $pitch;
				push @pitchbend, [
					$lastnoteendtime - $realstarttime,
					$notestarttime - $realstarttime,
					$pitch];
				$lastnoteendtime = $noteendtime;
			}
			my $avgpitch = $sumpitch / $sumtime;
			$_->[2] -= $avgpitch
				for @pitchbend;
			shift @pitchbend
				while @pitchbend
					and abs($pitchbend[0][2]) < 0.001;
			play_note
				$realstarttick,
				$realendtick - $realstarttick,
				$realstarttime,
				$realendtime - $realstarttime,
				$avgpitch, \@pitchbend, $text;
		}
	}
}

$out->{footer}->($out_self);
