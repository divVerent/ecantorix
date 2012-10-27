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
use Getopt::Long;
use eCantorix::Util;

our %VSQ_PHONEMES = (
	"a" => "A:",
	"b" => "b",
	"b'" => "b",
	"C" => "C",
	"d" => "d",
	"d'" => "d",
	"e" => "e",
	"g" => "g",
	"g'" => "g",
	"h" => "h",
	"i" => "I",
	"j" => "j",
	"J" => "n^",
	"k" => "k",
	"k'" => "k",
	"m" => "m",
	"m'" => "m",
	"M" => "u:", # ?
	"n" => "n",
	"N" => "N",
	"N'" => "N",
	"N\\" => "N",
	"N\\'" => "N",
	"o" => "o:", # ?
	"p" => "p",
	"p\\" => "f",
	"p'" => "p",
	"p\\'" => "f",
	"s" => "s",
	"S" => "S",
	"t" => "t",
	"t'" => "tC",
	"w" => "w",
	"z" => "z",
	"Z" => "Z",
	"1" => "h",
	"2" => "h",
	"3" => "h",
	"4" => "l", # Japanese can't speak r ;)
	"4'" => "l", # Japanese can't speak r ;)
	"5" => "h",
	"6" => "h",
	"*" => "h",
	"dz" => "dz",
	"ts" => "ts",
	"tS" => "tS",
	"dZ" => "dZ"
);
sub vsq2midi($$)
{
	my ($opus, $options) = @_;
	my $is_vsq = 0;
	for my $track($opus->tracks())
	{
		my @vsq_ini = ();
		my @events = ();
		my %channels = ();
		for(abstime $track->events())
		{
			if($_->[0] eq 'text_event' && $_->[2] =~ /^DM:(\d+):(.*)/s)
			{
				$vsq_ini[$1] = $2;
			}
			else
			{
				push @events, $_;
				if($_->[0] eq 'control_change')
				{
					++$channels{$_->[2]};
				}
			}
		}
		if(@vsq_ini)
		{
			my $channel = 0;
			my @channels = keys %channels;
			if(@channels == 1)
			{
				$channel = $channels[0];
			}
			else
			{
				warn "No unique channel on a vocaloid track: @channels\n";
			}
			my $vsq_ini = join "", @vsq_ini;
			require Config::Tiny;
			my $ini = Config::Tiny->read_string($vsq_ini)
				or die "VSQ import: text events do not form an INI file";
			my $lasttext = "";
			for my $k(sort { $a <=> $b } keys %{$ini->{EventList}})
			{
				my $v = $ini->{EventList}->{$k};
				next
					if $v eq 'EOS';
				my $ev = $ini->{$v};
				if($ev->{Type} eq 'Anote')
				{
					my $start = $k;
					my $len = $ev->{'Length'};
					my $note = $ev->{'Note#'};
					my $lyrichandle = $ev->{'LyricHandle'};
					my $velocity = $ev->{'Dynamics'};
					my $lyric = $ini->{$lyrichandle};
					unless($lyric->{L0} =~ /^"([^"]*)","([^"]*)"/)
					{
						warn "Invalid lyric L0 info: $lyric->{L0}";
						next;
					}
					my $text = $1;
					my $phonemes = $2;
					if($options->{use_phonemes})
					{
						my $ephonemes = "";
						for(split /\s+/, $phonemes)
						{
							if(exists $VSQ_PHONEMES{$_})
							{
								$ephonemes .= $VSQ_PHONEMES{$_};
							}
							else
							{
								warn "Unknown phoneme: $_, trying as-is";
								$ephonemes .= $_;
							}
						}
						if($ephonemes !~ /[\@325aAeEiIoO0uUV]/)
						{
							# if all we have is consonants, repeat the last one a lot
							# note: for n^ we need to repeat the n
							$ephonemes =~ s/(.)(\^?)$/$1$1$1$1$1$1$1$1$2/;
						}
						push @events, ['lyric', $start, "[[$ephonemes]]"];
					}
					else
					{
						if($text ne "-")
						{
							push @events, ['lyric', $start, $text];
						}
					}
					push @events, ['note_on', $start, $channel, $note, $velocity];
					push @events, ['note_off', $start + $len, $channel, $note, $velocity];
					$lasttext = $text;
				}
				else
				{
					warn "Unsupported VSQ event: $ev->{Type}";
				}
			}
			$track->events_r([reltime sorttime @events]);
			$is_vsq = 1;
		}
	}
}

my $infile = "-";
my $outfile = "-";
my $options = {
	use_phonemes => 0
};
Getopt::Long::Configure("gnu_getopt", "auto_help", "auto_version");
GetOptions(
	'input|i' => \$infile,
	'output|o' => \$outfile,
	'phonemes|p' => \$options->{use_phonemes}
);
my $opus = MIDI::Opus->new({from_file => $infile})
	or die "MIDI::Opus: could not read $infile";
vsq2midi $opus, $options;
$opus->write_to_file($outfile)
	or die "MIDI::Opus: could not write $outfile";
