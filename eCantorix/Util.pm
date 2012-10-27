package eCantorix::Util;
use strict;
use warnings;
use Exporter 'import';
our @EXPORT = qw(abstime reltime sorttime);

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

sub sorttime(@)
{
	my $i = 0;
	return map {
		[@$_[1..@$_-1]]
	} sort {
		$a->[2] <=> $b->[2]
			or
		$a->[0] <=> $b->[0]
	} map {
		[$i++, @$_]
	} @_;
}

1;
