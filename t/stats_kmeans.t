#!/usr/bin/perl 

use strict;
use warnings;
use Test::More;

BEGIN {
    plan tests => 12;
      # 1-2
    use_ok( 'PDL::Stats::Basic' );
    use_ok( 'PDL::Stats::Kmeans' );
}

use PDL::LiteF;
use PDL::NiceSlice;

sub tapprox {
  my($a,$b, $eps) = @_;
  $eps ||= 1e-6;
  my $diff = abs($a-$b);
    # use max to make it perl scalar
  ref $diff eq 'PDL' and $diff = $diff->max;
  return $diff < $eps;
}

is(tapprox( t_iv_cluster(), 0 ), 1);
sub t_iv_cluster {
  my @a = qw( a a b b );
  my $a = iv_cluster( \@a );
  return ( $a - pdl(byte, [1,1,0,0], [0,0,1,1]) )->sum;
}

is(tapprox( t_assign(), 0 ), 1);
sub t_assign {
  my $centroid = ones 2, 3;
  $centroid(0,) .= 0;
  my $a = sequence 4, 3;
  $a %= 2;
  my $c = $a->assign($centroid);
  my $cluster = pdl(byte, [1,0,1,0], [0,1,0,1]);
  return ($c - $cluster)->sum;
}

is(tapprox( t_centroid(), 0 ), 1);
sub t_centroid {
  my $a = sequence 4, 3;
  my $cluster = pdl(byte, [1,0,1,0], [0,1,0,1]);
  my ($m, $ss) = $a->centroid($cluster);
  my $m_a = pdl([1,2], [5,6], [9,10]);
  my $ss_a = ones(2,3) * 2;
  return sum( $m - $m_a + ( $ss - $ss_a ) );
}

is(tapprox( t_assign_bad(), 0 ), 1);
sub t_assign_bad {
  my $centroid = ones 2, 3;
  $centroid(0,) .= 0;
  my $a = sequence 5, 3;
  $a->setbadat(4,0);
  $a->setbadat(4,2);
  $a %= 2;
  my $c = $a->assign($centroid);
  my $cluster = pdl(byte, [1,0,1,0,0], [0,1,0,1,1]);
  return ($c - $cluster)->sum;
}

is(tapprox( t_centroid_bad(), 0 ), 1);
sub t_centroid_bad {
  my $a = sequence 5, 3;
  $a->setbadat(4,0);
  $a->setbadat(4,2);
  my $cluster = pdl(byte, [1,0,1,0,0], [0,1,0,1,1]);
  my ($m, $ss) = $a->centroid($cluster);
  my $m_a = pdl([1,2], [6,7.6666667], [11,12]);
  my $ss_a = ones(2,3);
  $ss_a(1,1) .= 1.5555556;
  return sum( $m - $m_a + ( $ss - $ss_a ) );
}

is(tapprox( t_kmeans(), 0 ), 1);
sub t_kmeans {
  my $data = sequence 7, 3;
  $data(1, ) .= 0;
  my %m = $data->kmeans({NCLUS=>2, V=>0});
  return sum( $m{centroid}->sumover - pdl qw(3.3333333  10.333333  17.333333) );
}

t_kmeans_4d();
sub t_kmeans_4d {
  my $data = sequence 7, 3, 2, 2;
  $data(1, ) .= 0;
  $data(0,1,0, ) .= 0;
  $data->where($data == 42) .= 0;
  my %m = $data->kmeans( {nclus=>[2,1,1], v=>0} );

  my %a = (
    'R2'  => pdl( qw( 0.74223245 0.97386667 0.84172845 0.99499377 ) ),
    'n'   => pdl( [5,2], [1,6], [5,2], [1,6] ),
    'ss_sum'  => pdl (
 [ qw(        10         10        108 )],
 [ qw( 23.333333  23.333333  23.333333 )],
 [ qw(        10         10       1578 )],
 [ qw( 23.333333  23.333333  23.333333 )],
           ),
  );

  is(tapprox( sum( $m{R2} - $a{R2} ), 0 ), 1);
  is(tapprox( sum( $m{n}  - $a{n}  ), 0 ), 1);
  is(tapprox( sum( $m{ss}->sumover - $a{ss_sum} ), 0, 1e-3 ), 1);
}

is(tapprox( t_kmeans_bad(), 0 ), 1);
sub t_kmeans_bad {
  my $data = sequence 7, 3;
  $data = $data->setbadat(4,0);
  my %m = $data->kmeans({NCLUS=>2, V=>0});
  return sum( $m{ms}->sumover - pdl qw( 1.5  1.9166667  1.9166667 ) );
}
