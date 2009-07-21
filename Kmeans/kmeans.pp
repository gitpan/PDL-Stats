#!/usr/bin/perl
pp_add_exported('', 'random_cluster', 'iv_cluster');

pp_addpm({At=>'Top'}, <<'EOD');

use Carp;
use PDL::LiteF;
use PDL::NiceSlice;
use PDL::Stats::Basic;

=head1 NAME

PDL::Stats::Kmeans -- classic k-means cluster analysis 

=head1 DESCRIPTION

Assumes that we have data pdl dim [observation, variable] and the goal is to put observations into clusters based on their values on the variables. The terms "observation" and "variable" are quite arbitrary but serve as a reminder for "that which is being clustered" and "that which is used to cluster".

The terms FUNCTIONS and METHODS are arbitrarily used to refer to methods that are threadable and methods that are non-threadable, respectively.

=head1 SYNOPSIS

    use PDL::LiteF;
    use PDL::NiceSlice;
    use PDL::Stats;

    my ($data, $idv, $ido) = get_data( $file );

    my ($cluster, $centroid, $ss_this, $ss_last, $ss_centroid);

      # start out with 8 random clusters
    $cluster = random_cluster( $data->dim(0), 8 );

    ($centroid, $ss_centroid) = $data->centroid( $cluster );
    $ss_this = $ss_centroid->sum;

      # iterate to minimize total ss
      # stop when change in ss is less than $crit amount 
    do {
      $ss_last = $ss_this;
      $cluster = $data->assign( $centroid );

      ($centroid, $ss_centroid) = $data->centroid( $cluster );
      $ss_this = $ss_centroid->sum;
    }
    while ( $ss_last - $ss_this > $crit );

or just do

    my %result = $data->kmeans( \%opt );
    print "$_\t$result{$_}\n" for (sort keys %result);

plot the clusters if there are only 2 vars

    use PDL::Graphics::PGPLOT::Window;

    my ($win, $c);
    $win = pgwin(Dev=>'/xs');
    $win->env($data( ,0)->minmax, $data( ,1)->minmax);

    $win->points( $data->dice_axis(0,which($m{cluster}->(,$_)))->dog,
                  {COLOR=>++$c} )
      for (0..$m{cluster}->dim(1)-1);

=cut

EOD

pp_addhdr('
#include <math.h>
#include <stdlib.h>
#include <time.h>

'
);


pp_addpm( <<'EOD' );

=head2 random_cluster

=for sig

  Signature: (byte [o]cluster(o,c); int obs=>o; int clu=>c)

=for ref

creates masks for random mutually exclusive clusters. accepts two parameters, num_obs and num_cluster. extra parameter turns into extra dim in mask. may loop a long time if num_cluster approaches num_obs because empty cluster is not allowed.

=for usage

    my $cluster = random_cluster( $num_obs, $num_cluster );

=cut

  # can't be called on pdl
sub random_cluster {
  my ($obs, $clu) = @_;
    # extra param in @_ made into extra dim
  my $cluster = zeroes @_;
  do {
    $cluster->inplace->_random_cluster();
  } while (PDL::any $cluster->sumover == 0 );
  return $cluster;
}

EOD

pp_def('_random_cluster',
  Pars  => 'byte a(o,c); byte [o]b(o,c)',
  Inplace   => 1,
  GenericTypes => [U],
  Code  => '
if ($SIZE(c) > $SIZE(o))
  barf("more cluster than obs!");
/* threading w time only srand produces identical clusters */
int r;
srand( time( NULL ) + r++);
int nc = $SIZE(c);
loop (o) %{
  int cl = rand() % nc;
  loop (c) %{
    $b() = (c == cl)? 1 : 0;
  %}
%}

  ',

  Doc   => undef,

);

pp_def('assign',
  Pars  => 'data(o,v); centroid(c,v); byte [o]cluster(o,c)',
  GenericTypes => [F,D],
  HandleBad => 1,
  Code  => '

$GENERIC(centroid) ssc, ssmin;
int cl = 0;

loop (o) %{
  ssmin = -1;
  loop (c) %{
    ssc = 0;
    loop (v) %{
      ssc += pow($data() - $centroid(), 2);
    %}
/* notice that if multiple ssc == ssmin the 1st is taken as cluster */
    if (ssmin < 0 || ssmin > ssc) {
      cl = c;
      ssmin = ssc;
    }
  %}
  loop (c) %{
    $cluster() = (c == cl)? 1 : 0;
  %}
%}

  ',
  BadCode  => '

$GENERIC(centroid) ssc, ssmin;
long cl, nvc;
cl = 0;

loop (o) %{
  ssmin = -1;
  loop (c) %{
    ssc = 0;
    nvc = 0;
    loop (v) %{
      if ($ISGOOD( $data() ) && $ISGOOD( $centroid() )) {
        ssc += pow($data() - $centroid(), 2);
        nvc ++;
      }
    %}
    if (nvc) {
      ssc /= nvc;
    }
    else {
/* taking advantage of the fact that 1st valid ssmin takes precedence */
/* so ssc has no effect if there is already ssmin. or it is -1 */
      ssc = ssmin;
    }
/* notice that if multiple ssc == ssmin the 1st is taken as cluster */
    if (ssmin < 0 || ssmin > ssc) {
      cl = c;
      ssmin = ssc;
    }
  %}
  loop (c) %{
    if (ssmin >= 0) {
      $cluster() = (c == cl)? 1 : 0;
    }
    else {
      $SETBAD($cluster());
    }
  %}
%}

  ',
  Doc   => '

=for ref

takes data pdl dim [obs x var] and centroid pdl dim [cluster x var] and returns mask dim [obs x cluster] to cluster membership. an obs is assigned to the first cluster with the smallest distance (ie sum squared error) to cluster centroid. with bad value, obs is assigned by smallest mean squared error across variables.

=for usage

    perldl> $centroid = ones 2, 3
    perldl> $centroid(0,) .= 0
    perldl> p $centroid
    [
     [0 1]
     [0 1]
     [0 1]
    ]

    perldl> $b = qsort( random 4, 3 )
    perldl> p $b
    [
     [0.022774068 0.032513883  0.13890034  0.30942479]
     [ 0.16943853  0.50262636  0.56251531   0.7152271]
     [ 0.23964483  0.59932745  0.60967495  0.78452117]
    ]
      # notice that 1st 3 obs in $b are on average closer to 0 
      # and last obs closer to 1
    perldl> p $b->assign( $centroid )
    [
     [1 1 1 0]    # cluster 0 membership
     [0 0 0 1]    # cluster 1 membership
    ]
  ',

);

pp_def('centroid',
  Pars  => 'data(o,v); cluster(o,c); float+ [o]m(c,v); float+ [o]ss(c,v)',
  GenericTypes => [F,D],
  HandleBad => 1,
  Code  => '
$GENERIC(m) s[ $SIZE(c) ][ $SIZE(v) ], s2[ $SIZE(c) ][ $SIZE(v) ];
long n[ $SIZE(c) ];

loop (c) %{
  loop (v) %{
    s[c][v]  = 0.0;
    s2[c][v] = 0.0;
  %}
  n[c] = 0;
  loop (o) %{
    if ($cluster()) {
      n[c] ++;
      loop (v) %{
        s[c][v]  += $data();
        s2[c][v] += pow($data(), 2);
      }
    %}
  %}

  if (n[c]) {
    loop (v) %{
      $m()  = s[c][v] / n[c];
      $ss() = s2[c][v] - pow(s[c][v] / n[c], 2) * n[c];
    %}
  }
  else {
    barf("please make sure there is no empty cluster!");
  }
%}

  ',
  BadCode  => '
$GENERIC(m) s[ $SIZE(c) ][ $SIZE(v) ], s2[ $SIZE(c) ][ $SIZE(v) ];
long n[ $SIZE(c) ][ $SIZE(v) ];

loop (c) %{
  loop (v) %{
    s[c][v]  = 0.0;
    s2[c][v] = 0.0;
    n[c][v]  = 0;
  %}
  loop (o) %{
    if ($ISGOOD($cluster()) && $cluster()) {
      loop (v) %{
        if ($ISGOOD( $data() )) {
          s[c][v]  += $data();
          s2[c][v] += pow($data(), 2);
          n[c][v]  ++;
        }
      }
    %}
  %}

  loop (v) %{
    if (n[c][v]) {
      $m()  = s[c][v] / n[c][v];
      $ss() = s2[c][v] / n[c][v] - pow(s[c][v] / n[c][v], 2);
    }
    else {
      $SETBAD($m());
      $SETBAD($ss());
    }
  %}
%}

  ',
  Doc   => '
=for ref

takes data dim [obs x var] and mask dim [obs x cluster] and returns average value and ss (ms when data contains bad values) dim [cluster x var] for data where mask = 1. multiple cluster membership for an obs is fine but quits if run into empty cluster ie no qualified element in a mask.

=for usage

      # data is 10 obs x 3 var
    perldl> p $d = sequence 10, 3
    [
     [ 0  1  2  3  4  5  6  7  8  9]
     [10 11 12 13 14 15 16 17 18 19]
     [20 21 22 23 24 25 26 27 28 29]
    ]
      # create two clusters by value on 1st var
    perldl> p $a = $d( ,(0)) <= 5
    [1 1 1 1 1 1 0 0 0 0]
 
    perldl> p $b = $d( ,(0)) > 5
    [0 0 0 0 0 0 1 1 1 1]

    perldl> p $c = cat $a, $b
    [
     [1 1 1 1 1 1 0 0 0 0]
     [0 0 0 0 0 0 1 1 1 1]
    ]

    perldl> p $d->centroid($c)
      # mean for 2 cluster x 3 var
    [
     [ 2.5  7.5]
     [12.5 17.5]
     [22.5 27.5]
    ]
      # ss for 2 cluster x 3 var
    [
     [17.5    5]
     [17.5    5]
     [17.5    5]
    ]

  ',

);

pp_addpm(<<'EOD');

=head2 kmeans

=for ref

Implements classic kmeans cluster analysis. Tries several rounds of random-seeding and clustering, returns the best results in terms of R2. Stops when change in R2 is smaller than set criterion.

Alternatively, if a centroid is provided, clustering will proceed from the centroid and there is no random-seeding or multiple tries.

kmeans supports bad value*.

=for options

Default options (case insensitive):

    V       => 1,          # prints simple status
    FULL    => 0,          # returns results for all seeding rounds

    CNTRD   => PDL->null,  # clu x var. optional. disables next 3 opts

    NTRY    => 5,          # num of seeding rounds
    NSEED   => 1000,       # num of starting seeds, use n obs up to NSEED
    NCLUS   => 8,          # num of clusters

    R2CRT   => .001,       # stop criterion for R2 change

=for usage

Usage:

    # suppose we have 4 person's ratings on 5 movies

    perldl> p $rating = ushort( ceil( random(4, 5) * 5 ) ) 
    [
     [3 2 2 3]
     [2 4 5 4]
     [5 3 2 3]
     [3 3 1 5]
     [4 3 3 2]
    ]

    # we want to put the 4 persons into 2 groups

    perldl> %k = $rating->kmeans( {NCLUS=>2} )

    # by default prints back options used
    # as well as info for all tries and iterations

    CNTRD   => Null
    FULL    => 0
    NCLUS   => 2
    NSEED   => 1000
    NTRY    => 5
    R2CRT   => 0.001
    V       => 1
    ss/ms total:    20.5
    iter 0 R2 [0.46341463 0.46341463 0.46341463 0.46341463 0.024390244]
    iter 1 R2 [0.46341463 0.46341463 0.46341463 0.46341463 0.46341463]

    perldl> p "$_\t$k{$_}\n" for (sort keys %k)

    R2      0.463414634146341
    centroid    # mean ratings for 2 group x 5 movies
    [
     [  2   3]
     [4.5   3]
     [2.5   4]
     [  2   4]
     [  3   3]
    ]
    
    cluster    # 4 persons' membership in two groups
    [
     [0 1 1 0]
     [1 0 0 1]
    ]
    
    n       [2 2]  # cluster size
    ss
    [
     [  0   0]
     [0.5   2]
     [0.5   2]
     [  2   2]
     [  0   2]
    ]

Now, for the valiant, kmeans is threadable. Say you gathered 10 persons' ratings on 5 movies from 2 countries, so the data is dim [10,5,2], and you want to put the 10 persons from each country into 3 clusters, just specify NCLUS => [3,1], and there you have it. The key is for NCLUS to include $data->ndims - 1 numbers. The 1 in [3,1] turns into a dummy dim, so the 3-cluster operation is repeated on both countries. See stats_kmeans.t for an example w 4D data.

*With bad value, R2 is based on average of variances instead of sum squared error. What's minimized is the average variance across clusters as compared to the original variance with all obs in one cluster. R2 in this case does not have the usual meaning of proportion of variance accounted for, but it does serve the purpose of minimizing variance.

=cut

*kmeans = \&PDL::kmeans;
sub PDL::kmeans {
  my ($self, $opt) = @_;
  my %opt = (
    V          => 1,
    FULL       => 0,

    CNTRD      => PDL->null,
      # next 3 opt not used if centroid is provided
    NTRY       => 5,
    NSEED      => 1000,
    NCLUS      => 3,

    R2CRT      => .001,
  );
  $opt    and $opt{uc $_} = $opt->{$_} for (keys %$opt);
  $opt{CNTRD}->nelem and $opt{NTRY} = 1;
  $opt{V} and print STDERR "$_\t=> $opt{$_}\n" for (sort keys %opt);
 
    # not avg across var, but sum up the ss across var
  my $ss_ms    = $self->badflag?  'ms' : 'ss';
  my $ss_total = $self->badflag? $self->var->average : $self->ss->sumover;
  $opt{V} and print STDERR "$ss_ms total:\t$ss_total\n";

  my ($cluster, $centroid, $ss_cv, $R2_this, $R2_last, $R2_change);

    # NTRY made into extra dim in $cluster for threading
  my $nseed = pdl($self->dim(0), $opt{NSEED})->min;
  my @nclus = (ref $opt{NCLUS} eq 'ARRAY')? @{$opt{NCLUS}} : ($opt{NCLUS});
  $cluster
    = $opt{CNTRD}->nelem ?
      $self->assign( $opt{CNTRD} )
    : random_cluster($nseed, @nclus, $opt{NTRY} )
    ;

  ($centroid, $ss_cv) = $self(0:$nseed - 1, )->centroid( $cluster );
  my $ss_seed = $self->badflag?
                $self(0:$nseed-1, )->var->average
              : $self(0:$nseed-1, )->ss->sumover
              ;
  $R2_this    = $self->badflag? 1 - $ss_cv->average->average / $ss_seed
              :                 1 - $ss_cv->sumover->sumover / $ss_seed
              ;

  my $iter = 0;
  do {
    $opt{V} and print STDERR join(' ',('iter', $iter++, 'R2', $R2_this)) . "\n";
    $R2_last = $R2_this;
   
    $cluster = $self->assign( $centroid );
    ($centroid, $ss_cv) = $self->centroid( $cluster );
    
    $R2_this = $self->badflag? 1 - $ss_cv->average->average / $ss_total
             :                 1 - $ss_cv->sumover->sumover / $ss_total
             ;
    $R2_change = $R2_this - $R2_last;
  }
  while (PDL::any $R2_change > $opt{R2CRT});

  $opt{FULL} and 
    return (
      centroid => PDL::squeeze( $centroid ),
      cluster  => PDL::squeeze( $cluster ),
      n        => PDL::squeeze( $cluster )->sumover,
      R2       => PDL::squeeze( $R2_this ), 
      $ss_ms   => PDL::squeeze( $ss_cv ),
    );

    # xchg(-1,0) leaves it as was if single dim--unlike transpose
  my $i_best = $R2_this->xchg(-1,0)->maximum_ind;

  $R2_this->getndims == 1 and
    return (
      centroid => $centroid->dice_axis(-1,$i_best)->squeeze,
      cluster  => $cluster->dice_axis(-1,$i_best)->squeeze,
      n        => $cluster->dice_axis(-1,$i_best)->squeeze->sumover,
      R2       => $R2_this->dice_axis(-1,$i_best)->squeeze, 
      $ss_ms   => $ss_cv->dice_axis(-1,$i_best)->squeeze,
    );

  # now for threading beyond 2D data

  # can't believe i'm using a perl loop :P

  $i_best = $i_best->flat->sever;
  my @i_best = map { $opt{NTRY} * $_ + $i_best(($_)) }
               0 .. $i_best->nelem - 1;

#print "\$i_best\t$i_best\n";
#print "\@i_best\t@i_best\n";
#print $centroid->info . "centroid\n";
#print $cluster->info . "cluster\n";
#print $R2_this->info . "R2\n";

  return (
    centroid =>
      $centroid->clump(2..$centroid->ndims-1)->dice_axis(-1,\@i_best)->squeeze,
    cluster  =>
      $cluster->clump(2..$cluster->ndims-1)->dice_axis(-1,\@i_best)->squeeze,
    n        =>
$cluster->clump(2..$cluster->ndims-1)->dice_axis(-1,\@i_best)->squeeze->sumover,
    R2       =>
      $R2_this->clump(0..$R2_this->ndims-1)->dice_axis(-1,\@i_best)->squeeze, 
    $ss_ms   =>
      $ss_cv->clump(2..$ss_cv->ndims-1)->dice_axis(-1,\@i_best)->squeeze,
  );
}

=head1 METHODS

=head2 iv_cluster

=for ref

Turns an independent variable into a cluster pdl. Returns cluster pdl and level-to-pdl_index mapping in list context and cluster pdl only in scalar context.

This is the method used for mean and var in anova. The difference between iv_cluster and dummy_code is that iv_cluster returns pdl dim (obs x level) whereas dummy_code returns pdl dim (obs x (level - 1)).

=for usage

Usage:

    perldl> @bake = qw( y y y n n n )
   
    # accepts @ ref or 1d pdl

    perldl> p $bake = iv_cluster( \@bake )
    [
     [1 1 1 0 0 0]
     [0 0 0 1 1 1]
    ]
    
    perldl> p $rating = sequence 6
    [0 1 2 3 4 5]

    perldl> p $rating->centroid( $bake )
    # mean for each iv level
    [
     [1 4]
    ]
    # ss
    [
     [2 2]
    ]

=cut

*iv_cluster = \&PDL::iv_cluster;
sub PDL::iv_cluster {
  my ($var_ref) = @_;

    # pdl->uniq puts elem in order. so instead list it to maintain old order
  if (ref $var_ref eq 'PDL') {
    $var_ref = $var_ref->squeeze;
    $var_ref->getndims > 1 and
      croak "multidim pdl passed for single var!";
    $var_ref = [ list $var_ref ];
  }

  my ($var, $map_ref) = _array_to_pdl( $var_ref );
  my $var_a = zeroes byte, $var->nelem, $var->max + 1;

  for my $l (0 .. $var->max) {
    my $v = $var_a( ,$l);
    $v->index( which $var == $l ) .= 1;
  }

  return wantarray? ($var_a, $map_ref) : $var_a;
}

sub _array_to_pdl {
  my ($var_ref) = @_;

  my (%level, $l);
  $l = 0;
  for (@$var_ref) {
    !exists $level{$_} and $level{$_} = $l ++;
  } 
  return wantarray? (pdl( map { $level{$_} } @$var_ref ), \%level)
        :            pdl( map { $level{$_} } @$var_ref )
        ;
}

=head1 	REFERENCES

Romesburg, H.C. (1984). Cluster Analysis for Researchers. NC: Lulu Press.

Wikipedia (retrieved June, 2009). K-means clustering. http://en.wikipedia.org/wiki/K-means_algorithm

=head1 AUTHOR

Copyright (C) 2009 Maggie J. Xiong <maggiexyz users.sourceforge.net>

All rights reserved. There is no warranty. You are allowed to redistribute this software / documentation as described in the file COPYING in the PDL distribution.

=cut

EOD


pp_done();
