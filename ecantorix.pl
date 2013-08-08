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

use FindBin;
use lib $FindBin::Bin;

use strict;
use warnings;
use MIDI;
use URI::Escape;
use Math::FFT;
use Cwd;
use Getopt::Long;
use Digest::SHA;
use eCantorix::Util;

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

# voice parameters
our $ESPEAK_VOICE = "default";
our $ESPEAK_VOICE_PATH;
our $ESPEAK_TRANSPOSE = -24;
our $ESPEAK_USE_PITCH_ADJUST_TAB = 0;

# filtering
our $SOX_RATE = 22050;
our $SOX_PREEFFECTS;
our $SOX_PREEFFECTS_CHANNELS = 1;
our $SOX_AFTEREFFECTS;
our $SOX_AFTEREFFECTS_CHANNELS = 1;

# paths
our $ESPEAK_CACHE = ".";
our $ESPEAK_TEMPDIR = undef; # use $ESPEAK_CACHE/tmp by default
our $ESPEAK_CACHE_PREFIX = "";

# input syllable editing (perl expression or sub operating on $_)
our $EDIT_SYLLABLES;

# hint: $EDIT_SYLLABLES = sub { s/^\s*\[\[\s*(.*)\s*\]\]\s*$/$1/s or s/^\s*(.*?)\s*$/[[$1]]/s; };
# changes the parsing to [[words]] and phonetics, instead of the default (words and [[phonetics]])

# output
our $OUTPUT_FORMAT = 'wav';
our $OUTPUT_FILE = '-';

# if $OUTPUT_FORMAT is 'mid':
our $OUTPUT_MID_PREFIX = 'vocals:';

# tools (usually need no changes here)
our $ESPEAK = 'espeak -v "$VOICE" ${VOICE_PATH:+--path="$VOICE_PATH"} -z -p "$PITCH" -s "$SPEED" -a "$VELOCITY" -w "$OUT" -m "<prosody range=\"0\"> $SYLLABLE </prosody>"';
our $SOX_PROCESS_IN_TO_S16LE = 'sox "$IN" -t raw -r "$RATE" -e signed -b 16 -c $PREEFFECTS_CHANNELS - remix - $PREEFFECTS silence 1 1s 0 reverse silence 1 1s 0 reverse';
our $SOX_PROCESS_TEMPO_PITCHBEND_S16LE_TO_OUT = 'sox -t raw -r "$RATE" -e signed -b 16 -c $PREEFFECTS_CHANNELS - "$OUT" tempo -s "$TEMPO" $PITCHBEND $AFTEREFFECTS';
our $SOX_PROCESS_TEMPO_PITCHBEND_S16LE_TO_OUT_USES_PITCH = 0;
our $SOX_TEMPO_MIN = 0.1;
our $SOX_TEMPO_MAX = 100;

# espeak tool options (normally don't touch this)
our $ESPEAK_ATTEMPTS = 8;
our $ESPEAK_PITCH_MIN = 0;
our $ESPEAK_PITCH_START = 50;
our $ESPEAK_PITCH_MAX = 99;
our $ESPEAK_SPEED_MIN = 80;
our $ESPEAK_SPEED_START = 175;
our $ESPEAK_SPEED_MAX = 450;
our $ESPEAK_VELOCITY_MIN = 0;
our $ESPEAK_VELOCITY_NORMAL = 100;
our $ESPEAK_VELOCITY_MAX = 200;

# pitch caching (normally don't touch this)
our $ESPEAK_PITCH_CACHE = 1;
our $ESPEAK_PITCH_CACHE_SPEEDS = 5;
our $ESPEAK_PITCH_FACTOR; # this can be a sub that calculates the pitch change

# pitch calculating (normally don't touch this)
our $ANALYZE_MINFREQ = 20;
our $ANALYZE_MAXFREQ = 500;
our $ANALYZE_BIAS = 1.4;
our $ANALYZE_RANGE1 = 2;
our $ANALYZE_RANGE2 = 16;
our $ANALYZE_ENV = 3;

# ASS output
our $ASS_PRETIME = 1.5;
our $ASS_POSTTIME = 0.5;
our $ASS_LENGTHFACTOR = 1.0;
# end of customizable variables

# some options make sense overriding from the command line
Getopt::Long::Configure("gnu_getopt", "auto_help", "auto_version");
GetOptions(
	'control-file|C=s' => sub { do $_[1]; },
	'voice|v=s' => \$ESPEAK_VOICE,
	'transpose|t=s' => sub { $ESPEAK_TRANSPOSE += $_[1]; },
	'rate|r=s' => \$SOX_RATE,
	'pre-effects|P=s' => \$SOX_PREEFFECTS,
	'after-effects|A=s' => \$SOX_AFTEREFFECTS,
	'cache|c=s' => \$ESPEAK_CACHE,
	'output-format|O=s' => \$OUTPUT_FORMAT,
	'output|o=s' => \$OUTPUT_FILE,
	'output-mid-prefix=s' => \$OUTPUT_MID_PREFIX,
	'edit-syllables=s' => \$EDIT_SYLLABLES,
	'dry-run|n' => sub { $OUTPUT_FORMAT = undef; },
);

my ($filename) = @ARGV;

$EDIT_SYLLABLES = eval "sub { $EDIT_SYLLABLES; }"
	if defined $EDIT_SYLLABLES and not ref $EDIT_SYLLABLES;

if(!defined $ESPEAK_PITCH_FACTOR and $ESPEAK_USE_PITCH_ADJUST_TAB)
{
	# works only for voices with zero range
	$ESPEAK_PITCH_FACTOR = sub { return $pitch_adjust_tab[$_[0]]; };
}

$ESPEAK_CACHE = Cwd::abs_path($ESPEAK_CACHE);
$ESPEAK_TEMPDIR //= "$ESPEAK_CACHE/tmp";
mkdir "$ESPEAK_TEMPDIR";

my $statusline_active = 0;
sub statusline(@)
{
	if($statusline_active)
	{
		print STDERR "\e[A\e[K";
	}
	print STDERR @_, "\n";
	$statusline_active = 1;
}
sub statusout(@)
{
	print STDERR @_;
	$statusline_active = 0;
}

my $opus = MIDI::Opus->new({from_file => $filename});

sub tick2lmms($)
{
	return int(48 / $opus->ticks() * $_[0] + 0.5);
}
my %out = (
	mmp => {
		header => sub
		{
			my ($self, $totalticks, $totaltime, $tempi) = @_;
			open $self->{fh}, ">", $OUTPUT_FILE
				or die "open $OUTPUT_FILE: $!";

			print { $self->{fh} } <<EOF;
<?xml version="1.0"?>
<!DOCTYPE multimedia-project>
<multimedia-project version="1.0" creator="Linux MultiMedia Studio (LMMS)" creatorversion="0.4.13" type="song">
	<head timesig_numerator="4" mastervol="100" timesig_denominator="4" masterpitch="0">
		<bpm value="120" id="3481048"/>
	</head>
	<song>
		<trackcontainer width="600" x="5" y="5" maximized="0" height="300" visible="1" type="song" minimized="0">
EOF
			if(@$tempi)
			{
				my $lmms_tempolen = tick2lmms($tempi->[-1][0]) + 1;
				print { $self->{fh} } <<EOF;
			<track muted="0" type="5" name="Automation track">
				<automationtrack/>
				<automationpattern name="Tempo" pos="0" len="$lmms_tempolen">
EOF
				for(@$tempi)
				{
					my $lmms_tick = tick2lmms($_->[0]);
					# x [sec/tick]
					# x/60 [min/tick]
					# 60/x [tick/min]
					# (60/$opus->ticks())/x [quarter/min]
					my $lmms_tempo = int((60 / $opus->ticks()) / $_->[1] + 0.5);
					print { $self->{fh} } <<EOF;
					<time value="$lmms_tempo" pos="$lmms_tick"/>
EOF
				}
				print { $self->{fh} } <<EOF;
					<object id="3481048"/>
				</automationpattern>
			</track>
EOF
			}
			print { $self->{fh} } <<EOF;
			<track muted="0" type="2" name="Sample track">
				<sampletrack vol="100">
					<fxchain numofeffects="0" enabled="0"/>
				</sampletrack>
EOF
		},
		sample => sub {
			my ($self, $tick, $dtick, $time, $dtime, $outname) = @_;
			my $lmms_tick = tick2lmms($tick);
			my $lmms_dtick = tick2lmms($tick + $dtick) - $lmms_tick;
			print { $self->{fh} } <<EOF;
				<sampletco muted="0" pos="$lmms_tick" len="$lmms_dtick" src="$outname" />
EOF
		},
		footer => sub {
			my ($self) = @_;
			print { $self->{fh} } <<EOF;
			</track>
		</trackcontainer>
	</song>
</multimedia-project>
EOF
		}
	},
	mid => {
		header => sub {
			my ($self, $totalticks, $totaltime, $tempi) = @_;
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
			my ($self, $tick, $dtick, $time, $dtime, $outname) = @_;
			my $track = $self->{opus}->tracks_r()->[-1];
			$outname =~ s/^.*\///;
			$track->new_event(
				'text_event', $tick, "$OUTPUT_MID_PREFIX$outname");
		},
		footer => sub {
			my ($self) = @_;
			my $track = $self->{opus}->tracks_r()->[-1];
			$track->events(reltime $track->events());
			$self->{opus}->write_to_file($OUTPUT_FILE);
			delete $self->{opus};
		}
	},
	wav => {
		header => sub {
			my ($self, $totalticks, $totaltime, $tempi) = @_;
			$self->{buf} = [];
		},
		sample => sub {
			my ($self, $tick, $dtick, $time, $dtime, $outname) = @_;
			my $sample_start = int($time * $SOX_RATE) * $SOX_AFTEREFFECTS_CHANNELS;
			my $cmd = 'sox "$IN" -t raw -r "$RATE" -e signed -b 16 -c $AFTEREFFECTS_CHANNELS -';
			my $fh = do
			{
				local $ENV{IN} = $outname;
				local $ENV{RATE} = $SOX_RATE;
				local $ENV{AFTEREFFECTS_CHANNELS} = $SOX_AFTEREFFECTS_CHANNELS;
				open my $fh, '-|', "$cmd"
					or die "$cmd: $!";
				$fh;
			};
			my $data = do { undef local $/; <$fh>; };
			close $fh
				or die "$cmd: $! $?";
			die "$cmd: No data"
				unless length $data;
			my @samples = unpack "s*", $data;
			my $b = $self->{buf};
			$b->[$_ + $sample_start] += $samples[$_]
				for 0..@samples-1;
		},
		footer => sub {
			my ($self) = @_;
			my $clip = 0;
			for(@{$self->{buf}})
			{
				$_ //= 0;
				if($_ > 32767)
				{
					++$clip;
					$_ = 32767;
				}
				if($_ < -32768)
				{
					++$clip;
					$_ = -32768;
				}
			}
			statusout "$clip clipped samples\n"
				if $clip > 0;
			statusout "writing...\n";
			my $cmd = 'sox -t raw -r "$RATE" -e signed -b 16 -c $AFTEREFFECTS_CHANNELS - -t wav "$OUT"';
			my $fh = do
			{
				local $ENV{OUT} = $OUTPUT_FILE;
				local $ENV{RATE} = $SOX_RATE;
				local $ENV{AFTEREFFECTS_CHANNELS} = $SOX_AFTEREFFECTS_CHANNELS;
				open my $fh, '|-', "$cmd"
					or die "$cmd: $!";
				$fh;
			};
			print $fh pack "s*", @{$self->{buf}};
			close $fh
				or die "$cmd: $! $?";
		}
	}
);
my $out = defined $OUTPUT_FORMAT ? $out{$OUTPUT_FORMAT} : undef;
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

	# EAC algorithm
	# 1. clip off values below zero
	for(0..@$correl-1)
	{
		$correl->[$_] = 0
			if $correl->[$_] < 0;
	}
	# 2. subtract a time-doubled signal
	for(reverse 1..@$correl/2-1)
	{
		if($_ % 2)
		{
			$correl->[$_] -= ($correl->[($_ - 1) / 2] + $correl->[($_ - 1) / 2]) / 2;
		}
		else
		{
			$correl->[$_] -= $correl->[$_ / 2];
		}
		# 3. clip off values below zero again
		$correl->[$_] = 0
			if $correl->[$_] < 0;
	}

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
	}

	return $ret;
}

sub get_voice_sample($$$$)
{
	my ($pitch, $velocity, $speed, $syllable) = @_;
	{
		local $ENV{OUT} = $ESPEAK_TEMPDIR . "/espeak_raw.wav";
		local $ENV{PITCH} = $pitch;
		local $ENV{VELOCITY} = $velocity;
		local $ENV{SPEED} = $speed;
		local $ENV{VOICE} = $ESPEAK_VOICE;
		local $ENV{VOICE_PATH} = $ESPEAK_VOICE_PATH;
		local $ENV{SYLLABLE} = $syllable;
		local $ENV{TEMP} = $ESPEAK_TEMPDIR;
		0 == system $ESPEAK
			or die "$ESPEAK: $! $?";
	}
	my $fh = do
	{
		local $ENV{PREEFFECTS} = $SOX_PREEFFECTS;
		local $ENV{PREEFFECTS_CHANNELS} = $SOX_PREEFFECTS_CHANNELS;
		local $ENV{RATE} = $SOX_RATE;
		local $ENV{IN} = $ESPEAK_TEMPDIR . "/espeak_raw.wav";
		local $ENV{TEMP} = $ESPEAK_TEMPDIR;
		open my $fh, '-|', $SOX_PROCESS_IN_TO_S16LE
			or die "$SOX_PROCESS_IN_TO_S16LE: $!";
		$fh;
	};
	my $data = do { undef local $/; <$fh>; };
	close $fh
		or die "$SOX_PROCESS_IN_TO_S16LE: $! $?";
	return $data;
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
	my $cache = sprintf "%s/%s_%s_%d.pitch",
		$ESPEAK_CACHE, $ESPEAK_CACHE_PREFIX, $ESPEAK_VOICE, $pitch;
	if(open my $fh, "<", $cache)
	{
		my $ret = <$fh> + 0;
		close $fh;
		return $ret;
	}
	my @hz = ();
	statusout "Caching pitch $pitch... ";
	for my $syllable(qw/do re mi fa so la ti/)
	{
		for(0..($ESPEAK_PITCH_CACHE_SPEEDS - 1))
		{
			my $speed = int(0.5 + $ESPEAK_SPEED_MIN + ($ESPEAK_SPEED_MAX - $ESPEAK_SPEED_MIN + 1) * (($_ + 0.5) / $ESPEAK_PITCH_CACHE_SPEEDS));
			my $data = get_voice_sample $pitch, $ESPEAK_VELOCITY_NORMAL, $speed, $syllable;
			die "$SOX_PROCESS_IN_TO_S16LE: No data"
				unless length $data;
			my @data = unpack "s*", $data;
			my $thishz = $SOX_RATE / getpitch(\@data, $SOX_RATE / $ANALYZE_MAXFREQ, $SOX_RATE / $ANALYZE_MINFREQ, undef);

			push @hz, $thishz;
		}
	}
	my $median = [sort { $a <=> $b } @hz]->[(@hz - 1) / 2];
	my $above = scalar grep { $_ >= $median * (2 ** (0.5 / 12)) } @hz;
	my $below = scalar grep { $_ <= $median / (2 ** (0.5 / 12)) } @hz;
	statusout "$median ($above above, $below below)\n";
	if(open my $fh, ">", $cache)
	{
		print $fh "$median\n";
		close $fh;
	}
	return $pitch_cache[$pitch] = $median;
}
sub find_pitch_cached($)
{
	my ($freq) = @_;

	# perform a binary search in the pitch cache

	my $mi = $ESPEAK_PITCH_MIN;
	my $ma = $ESPEAK_PITCH_MAX;
	return $mi
		if $mi == $ma;
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

my @last_syllables = ();
sub play_note($$$$$$$$)
{
	my ($tick, $dtick, $t, $dt, $note, $velocity, $pitchbend, $syllable) = @_;

	# kill whitespace (it does nothing)
	$syllable =~ s/^\s+//;
	$syllable =~ s/\s+$//;

	# iterative find fitting -p (pitch, 0..99) and -s (speed, 80..450)
	# parameters for espeak

	my $pitchbend_str = "";
	my $pitchbend_t = 0;
	for(@$pitchbend)
	{
		my ($start, $end, $semitones) = @$_;
		my $delay = $start - $pitchbend_t;
		my $duration = $end - $start;
		$pitchbend_t = $end;
		my $cents = $semitones * 100;
		my $cents_str = sprintf "%.1f", $cents;
		$cents_str = "0.001"
			if $cents_str eq "0.0"; # prevent sox error
		$duration = 0.001
			if $duration < 0.001;
		$pitchbend_str .= sprintf " %.6f,%s,%.6f",
			$delay, $cents_str, $duration;
	}
	$pitchbend_str = "bend$pitchbend_str"
		if length $pitchbend_str;

	my $hz = 440 * 2**(($note - 69 + $ESPEAK_TRANSPOSE) / 12);
	my $pitch = $ESPEAK_PITCH_START;
	my $speed = $ESPEAK_SPEED_START;
	my $evelocity = int($ESPEAK_VELOCITY_MIN
		+ ($velocity / 127.0)
			* ($ESPEAK_VELOCITY_MAX - $ESPEAK_VELOCITY_MIN) + 0.5);

	my $propstr = Digest::SHA::sha1_hex join "/",
		$pitchbend_str, $syllable;
	my $outname = sprintf "%s/%s_%s_%d_%.2f_%.2f_%.2f_%s.wav",
		$ESPEAK_CACHE, $ESPEAK_CACHE_PREFIX, $ESPEAK_VOICE, $SOX_RATE, $dt, $hz, $velocity, $propstr;

	push @last_syllables, $syllable;
	shift @last_syllables
		if @last_syllables > 10;
	statusline "@last_syllables";

	if(!-f $outname)
	{
		statusout "Rendering $outname...";
		my $thisdt;
		my $thishz;
		my $data;
		if($ESPEAK_PITCH_CACHE)
		{
			$pitch = find_pitch_cached($hz);
		}
		for(1..$ESPEAK_ATTEMPTS)
		{
			$data = get_voice_sample $pitch, $evelocity, $speed, $syllable;
			if(!length $data)
			{
				warn "$SOX_PROCESS_IN_TO_S16LE: No data";
				return;
			}

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

		statusout "pitch=$pitch velocity=$evelocity speed=$speed $pitchbend_str";

		my $pitchfix = $hz / $thishz;
		my $lengthfix = $dt / $thisdt;
		if(!$ESPEAK_PITCH_CACHE)
		{
			if($pitchfix <= 0 || abs(log($pitchfix)) > 0.1)
			{
				warn "Large pitch correction: $thishz -> $hz";
				getpitch([unpack("s*", $data)], $SOX_RATE / $ANALYZE_MAXFREQ, $SOX_RATE / $ANALYZE_MINFREQ, "$outname.plot");
			}
		}
		my $rate = $SOX_RATE;

		my $tempofix = 1 / $lengthfix;

		if(!$SOX_PROCESS_TEMPO_PITCHBEND_S16LE_TO_OUT_USES_PITCH)
		{
			my $rate0 = $rate;
			$rate = int(0.5 + $rate0 * $pitchfix);
			$tempofix *= $rate0 / $rate;;
			$pitchfix = 1;
		}

		$tempofix = $SOX_TEMPO_MIN
			if $tempofix < $SOX_TEMPO_MIN;
		$tempofix = $SOX_TEMPO_MAX
			if $tempofix > $SOX_TEMPO_MAX;

		my $fh = do
		{
			local $ENV{PREEFFECTS_CHANNELS} = $SOX_PREEFFECTS_CHANNELS;
			local $ENV{AFTEREFFECTS} = $SOX_AFTEREFFECTS;
			local $ENV{AFTEREFFECTS_CHANNELS} = $SOX_AFTEREFFECTS_CHANNELS;
			local $ENV{RATE} = $rate;
			local $ENV{TEMPO} = $tempofix;
			local $ENV{PITCH} = $pitchfix;
			local $ENV{OUT} = $outname;
			local $ENV{PITCHBEND} = $pitchbend_str;
			local $ENV{TEMP} = $ESPEAK_TEMPDIR;
			open my $fh, '|-', $SOX_PROCESS_TEMPO_PITCHBEND_S16LE_TO_OUT
				or die "$SOX_PROCESS_TEMPO_PITCHBEND_S16LE_TO_OUT: $!";
			$fh;
		};
		print $fh $data;
		close $fh
			or die "$SOX_PROCESS_TEMPO_PITCHBEND_S16LE_TO_OUT: $! $?";

		statusout "\n";
	}

	$out->{sample}->($out_self, $tick, $dtick, $t, $dt, $outname);
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

sub hmmsshh($)
{
	my ($t) = @_;
	my $ticks = int($t * 100 + 0.5);
	my $hh = $ticks % 100; $ticks = int($ticks / 100);
	my $SS = $ticks % 60; $ticks = int($ticks / 60);
	my $MM = $ticks % 60; $ticks = int($ticks / 60);
	my $H = $ticks;
	return sprintf "%d:%02d:%02d.%02d", $H, $MM, $SS, $hh;
}
my $ass_line = "";
my $ass_starttime = undef;
my $ass_lasttime = undef;
my $ass_endtime = undef;
my $ass_first = 1;
my $ass_wantspace = "";
sub dump_lyric($$$$)
{
	my ($start, $end, $text, $endofline) = @_;

	return
		if $OUTPUT_FILE eq '-';

	my $endtime = undef;
	if (defined $ass_endtime)
	{
		if (defined $start)
		{
			my $d = $start - $ass_endtime;
			$d = $ASS_PRETIME + $ASS_POSTTIME
				if $d > $ASS_PRETIME + $ASS_POSTTIME;
			$d = 0
				if $d < 0;
			$d *= $ASS_POSTTIME / ($ASS_PRETIME + $ASS_POSTTIME);
			$endtime = $ass_endtime + $d;
		}
		else
		{
			$endtime = $ass_endtime + $ASS_POSTTIME;
		}
		my $startstr = hmmsshh $ass_starttime;
		my $endstr = hmmsshh $endtime;
		statusout "";
		print "Dialogue: 0,$startstr,$endstr,Karaoke,eCantorix,0000,0000,0000,,$ass_line\n";
		$ass_line = "";
		undef $ass_starttime;
		undef $ass_lasttime;
		undef $ass_endtime;
		$ass_wantspace = "";
	}

	# just finalize if $text is not defined
	return
		if not defined $text;

	if ($ass_first && $endofline) {
		$ass_first = 0;
		statusout "";
		print <<EOF;
[Script Info]
; Script generated by eCantorix
Title: <untitled>
Original Script: <unknown>
ScriptType: v4.00+
Collisions: Normal
PlayResX: 720
PlayResY: 480
PlayDepth: 0
Timer: 100.0000
WrapStyle: 0

[V4+ Styles]
Format: Name, Fontname, Fontsize, PrimaryColour, SecondaryColour, TertiaryColour, BackColour, Bold, Italic, Underline, StrikeOut, ScaleX, ScaleY, Spacing, Angle, BorderStyle, Outline, Shadow, Alignment, MarginL, MarginR, MarginV, AlphaLevel, Encoding
Style: Karaoke,Arial,28,&H00FF0000,&H00FFFF00,&H00FFFFFF,&H80000000,-1,0,0,0,100,100,0.00,0.00,1,1.00,2.00,2,30,30,30,0

[Events]
Format: Layer, Start, End, Style, Name, MarginL, MarginR, MarginV, Effect, Text
EOF
	}

	$text =~ s/^(\s*)//;
	my $space = $ass_wantspace . $1;
	$text =~ s/(\s*)$//;
	$ass_wantspace = $1;

	if (!defined $ass_starttime)
	{
		if (defined $endtime)
		{
			my $d = $start - $endtime;
			$d = $ASS_PRETIME
				if $d > $ASS_PRETIME;
			$d = 0
				if $d < 0;
			$ass_starttime = $start - $d;
		}
		else
		{
			$ass_starttime = $start - $ASS_PRETIME;
		}
		$ass_lasttime = $ass_starttime;
		$space = "";
	}

	# advance to $start
	my $ass_delta = int(100 * ($start - $ass_lasttime) + 0.5);
	$ass_delta = 0
		if $ass_delta < 0;
	$ass_line .= "{\\k$ass_delta}"
		if $ass_delta > 0 || $space ne "";
	$ass_line .= $space;
	# output lyrics
	my $ass_length = int(100 * ($start + ($end - $start) * $ASS_LENGTHFACTOR - $ass_lasttime) + 0.5 - $ass_delta);
	$ass_length = 0
		if $ass_length < 0;
	$ass_line .= "{\\kf$ass_length}"
		if $ass_length > 0;
	$ass_line .= $text;
	$ass_lasttime += 0.01 * ($ass_delta + $ass_length);

	if ($endofline)
	{
		$ass_endtime = $end;
	}
}

my $tracks = $opus->tracks_r();

my $totallen = 0;
for my $trackno(0..@$tracks-1)
{
	my $track = $tracks->[$trackno];
	my @events = abstime $track->events();
	$totallen = $events[-1][1]
		if @events and $events[-1][1] > $totallen;;
}
my $totaltime = tick2sec $totallen;
$out->{header}->($out_self, $totallen, $totaltime, \@tempi)
	if defined $out;

my %text_event_types = map { $_ => 1 } qw(lyric text_event);
my %note_event_types = map { $_ => 1 } qw(note_on note_off);

my $lyricstrack = undef;
for my $trackno(0..@$tracks-1)
{
	statusout "Analyzing track $trackno...";
	my $track = $tracks->[$trackno];
	my @events = abstime $track->events();
	my $has_lyrics = grep { $text_event_types{$_->[0]} } @events;
	my $has_notes = grep { $note_event_types{$_->[0]} } @events;
	if ($has_lyrics)
	{
		if ($has_notes)
		{
			# mixed note/lyrics track. no need to process.
			$lyricstrack = -1;
		}
		else
		{
			# lyrics-only track. Append this to all tracks if it is
			# the only one with lyrics.
			if (defined $lyricstrack)
			{
				# more than one with lyrics - don't do this
				$lyricstrack = -1;
			}
			else
			{
				$lyricstrack = $trackno;
			}
		}
	}
}
$lyricstrack = undef
	if defined $lyricstrack and $lyricstrack == -1;

for my $trackno(0..@$tracks-1)
{
	statusout "Processing track $trackno...\n";
	my $track = $tracks->[$trackno];

	my @events = abstime $track->events();

	if (defined $lyricstrack)
	{
		next
			if $trackno == $lyricstrack;
		@events = sorttime(
			@events, abstime $tracks->[$lyricstrack]->events());
	}

	# lyrics = array of [starttick, text, channels]
	# channels = hash of channel => notes
	# notes = array of [starttick, ticks, pitch]
	my @lyrics = map {
		$text_event_types{$_->[0]} ? [$_->[1], $_->[2], {}] : ()
	} @events;
	my $insert_note = sub
	{
		my ($starttick, $ticks, $channel, $pitch, $velocity) = @_;
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
				[$starttick, $ticks, $pitch, $velocity];
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
		my $velocity = $_->[4];
		if($_->[0] eq 'note_on')
		{
			if(exists $notehash{$channel}{$pitch})
			{
				warn "note_already_on: $time/$channel/$pitch";
				next;
			}
			$notehash{$channel}{$pitch} = [$time, $velocity];
		}
		else
		{
			if(!exists $notehash{$channel}{$pitch})
			{
				warn "Spurious note_off: $time/$channel/$pitch";
				next;
			}
			my ($starttime, $startvelocity) = @{$notehash{$channel}{$pitch}};
			delete $notehash{$channel}{$pitch};

			$insert_note->(
				$starttime, $time - $starttime,
				$channel, $pitch, $startvelocity)
				or warn "No lyrics found for note: $starttime/$time/$channel/$pitch";
		}
	}

	for(@lyrics)
	{
		my ($starttick, $text, $channels) = @$_;
		next
			if $text =~ /^%/;
		$text =~ s/^\///;
		$text =~ s/^\\//;
		my $has_newline = 0;
		$has_newline = 1
			if $text =~ s/\\n\s*$//;

		if($EDIT_SYLLABLES)
		{
			local $_ = $text;
			$EDIT_SYLLABLES->();
			$text = $_;
		}

		next
			if $text !~ /\S/;

		for my $channel(sort keys %$channels)
		{
			my $notes = $channels->{$channel};
			my $realstarttick = $notes->[0]->[0];
			my $realendtick = $notes->[-1]->[0] + $notes->[-1]->[1];
			my $realstarttime = tick2sec $realstarttick;
			my $realendtime = tick2sec $realendtick;
			next
				if $realendtime - $realstarttime > 30;
			dump_lyric $realstarttime, $realendtime, $text, $has_newline;
			next
				if not defined $out;
			my @pitchbend = ();
			my $lastnoteendtime = $realstarttime;
			my $sumpitch = 0;
			my $sumvelocity = 0;
			my $sumtime = 0;
			for(@$notes)
			{
				my ($notestarttick, $noteticks, $pitch, $velocity) = @$_;
				my $noteendtick = $notestarttick + $noteticks;
				my $notestarttime = tick2sec($notestarttick);
				my $noteendtime = tick2sec($noteendtick);
				die "Overlapping notes: $notestarttime/$channel/$pitch"
					if $notestarttime < $lastnoteendtime;
				$sumtime += $noteendtime + $notestarttime;
				$sumpitch += ($noteendtime + $notestarttime) * $pitch;
				$sumvelocity += ($noteendtime + $notestarttime) * $velocity;
				push @pitchbend, [
					$lastnoteendtime - $realstarttime,
					$notestarttime - $realstarttime,
					$pitch];
				$lastnoteendtime = $noteendtime;
			}
			my $avgpitch = $sumpitch / $sumtime;
			my $avgvelocity = $sumvelocity / $sumtime;
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
				$avgpitch, $avgvelocity, \@pitchbend, $text;
		}
	}

	dump_lyric undef, undef, undef, undef;
}

$out->{footer}->($out_self)
	if defined $out;
