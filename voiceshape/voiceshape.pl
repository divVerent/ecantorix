#!/usr/bin/perl

use strict;
use warnings;
use Getopt::Long;
use POSIX;

my %settings = (
	"language" => "variant",
	"name" => "foo",
	"gender" => ["neutrum", 100],
	"pitch" => [82, 118],
	"formant 0" => [100, 100, 100, 0],
	"formant 1" => [100, 100, 100, 0],
	"formant 2" => [100, 100, 100, 0],
	"formant 3" => [100, 100, 100, 0],
	"formant 4" => [100, 100, 100, 0],
	"formant 5" => [100, 100, 100, 0],
	"formant 6" => [100, 100, 100, 0],
	"formant 7" => [100, 100, 100, 0],
	"formant 8" => [100, 100, 100, 0],
	"echo" => [0, 0],
	"tone" => [600, 170, 1200, 135, 2000, 110],
	"flutter" => 2,
	"roughness" => 2,
	"voicing" => 100,
	"consonants" => [100, 100],
	"breath" => [0, 0, 0, 0, 0, 0, 0, 0],
	"breathw" => undef,
	"stressAdd" => [0, 0, 0, 0, 0, 0, 0, 0]
);

while(<STDIN>)
{
	chomp;
	s/\/\/.*//;
	s/^\s+//;
	s/\s+$//;
	next
		if /^$/;
	my $found = 0;
	for my $k(keys %settings)
	{
		if(/^$k (.*)/)
		{
			if(ref $settings{$k} eq 'ARRAY')
			{
				my @data = split /\s+/, $1;
				my $len = @{$settings{$k}};
				@data = (@data, (0) x ($len - @data))
					if $len > @data;
				$settings{$k} = \@data;
			}
			else
			{
				$settings{$k} = $1;
			}
			$found = 1;
		}
	}
	warn "Line not matched: $_"
		if not $found;
}

Getopt::Long::Configure("gnu_getopt", "auto_help", "auto_version");
GetOptions(
	'pitch|p=s' => sub { $settings{pitch}[0] *= $_[1]; $settings{pitch}[1] *= $_[1]; },
	'pitch-set|P=s' => sub { my @p = split /\s+/, $_[1]; die "INVALID" if @p < 1; @p = (@p, @p) while @p < 2; $settings{pitch} = [@p[0..1]]; },

	'formant-freq|f=s' => sub { for(0..8) { $settings{"formant $_"}[0] *= $_[1]; $settings{"formant $_"}[2] *= $_[1]; $settings{"formant $_"}[3] *= $_[1]; } },
	'formant-width|w=s' => sub { for(0..8) { $settings{"formant $_"}[2] *= $_[1]; } },

	'flutter|u=s' => sub { $settings{flutter} += $_[1]; },
	'flutter-set|U=s' => sub { $settings{flutter} = $_[1]; },

	'roughness|r=s' => sub { $settings{roughness} += $_[1]; },
	'roughness-set|R=s' => sub { $settings{roughness} = $_[1]; },

	'voicing|v=s' => sub { $settings{voicing} *= $_[1]; },
	'voicing-set|V=s' => sub { $settings{voicing} *= $_[1]; },

	'consonants|c=s' => sub { $settings{consonants}[0] *= $_[1]; $settings{consonants}[1] *= $_[1]; },
	'consonants-set|C=s' => sub { $settings{consonants} = [$_[1], $_[1]]; },

	'breath|b=s' => sub { for(0..7) { $settings{breath}[$_] *= $_[1]; } },
	'breath-set|B=s' => sub { my @p = split /\s+/, $_[1]; die "INVALID" if @p < 1; @p = (@p, @p) while @p < 8; $settings{breath} = [@p[0..7]]; },

	'stress|s=s' => sub { for(0..7) { $settings{stressAdd}[$_] *= $_[1]; } },
	'stress-set|S=s' => sub { for(0..7) { $settings{stressAdd}[$_] = $_[1]; } },

	'tone|t=s' => sub { for(0..@{$settings{tone}}/2-1) { $settings{tone}[2*$_] *= $_[1]; } },
	'tone-set|T=s' => sub { my @p = split /\s+/, $_[1]; die "INVALID" if !@p or @p % 2; $settings{tone} = [@p]; },
);

sub getnum {
	use POSIX qw(strtod);
	my $str = shift;
	return undef
		if not defined $str;
	$str =~ s/^\s+//;
	$str =~ s/\s+$//;
	$! = 0;
	my($num, $unparsed) = strtod($str);
	if (($str eq '') || ($unparsed != 0) || $!) {
		return undef;
	}
	else {
		return $num;
	}
}

sub is_numeric { defined getnum($_[0]) }

for(sort keys %settings)
{
	my $data = $settings{$_};
	if(ref $data eq 'ARRAY')
	{
		$data = join ' ',
			map { is_numeric($_) ? sprintf("%d", $_) : $_; } @$data;
	}
	else
	{
		$data = is_numeric($data) ? sprintf("%d", $data) : $data;
	}
	print "$_ $data\n"
		if defined $settings{$_};
}
