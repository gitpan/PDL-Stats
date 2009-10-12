
#
# GENERATED WITH PDL::PP! Don't modify!
#
package PDL::Stats::Kmeans;

@EXPORT_OK  = qw(  random_cluster iv_cluster PDL::PP _random_cluster PDL::PP assign PDL::PP centroid );
%EXPORT_TAGS = (Func=>[@EXPORT_OK]);

use PDL::Core;
use PDL::Exporter;
use DynaLoader;



   
   @ISA    = ( 'PDL::Exporter','DynaLoader' );
   push @PDL::Core::PP, __PACKAGE__;
   bootstrap PDL::Stats::Kmeans ;





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







=head1 FUNCTIONS



=cut





=head2 random_cluster

=for sig

  Signature: (byte [o]cluster(o,c); int obs=>o; int clu=>c)

=for ref

Creates masks for random mutually exclusive clusters. Accepts two parameters, num_obs and num_cluster. Extra parameter turns into extra dim in mask. May loop a long time if num_cluster approaches num_obs because empty cluster is not allowed.

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





*_random_cluster = \&PDL::_random_cluster;




=head2 assign

=for sig

  Signature: (data(o,v); centroid(c,v); byte [o]cluster(o,c))



=for ref

Takes data pdl dim [obs x var] and centroid pdl dim [cluster x var] and returns mask dim [obs x cluster] to cluster membership. An obs is assigned to the first cluster with the smallest distance (ie sum squared error) to cluster centroid. With bad value, obs is assigned by smallest mean squared error across variables.

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
  

=for bad

assign does handle bad values.
It will set the bad-value flag of all output piddles if the flag is set for any of the input piddles.


=cut






*assign = \&PDL::assign;




=head2 centroid

=for sig

  Signature: (data(o,v); cluster(o,c); float+ [o]m(c,v); float+ [o]ss(c,v))


=for ref

Takes data dim [obs x var] and mask dim [obs x cluster], returns mean and ss (ms when data contains bad values) dim [cluster x var], using data where mask = 1. Multiple cluster membership for an obs is okay. If a cluster is empty all means and ss are set to zero for that cluster.

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

  

=for bad

centroid does handle bad values.
It will set the bad-value flag of all output piddles if the flag is set for any of the input piddles.


=cut






*centroid = \&PDL::centroid;



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
    NSEED   => 1000,       # num of initial seeds, use NSEED up to max obs
    NCLUS   => 8,          # num of clusters

    R2CRT   => .001,       # stop criterion for R2 change

=for usage

Usage:

    # suppose we have 4 person's ratings on 5 movies

    perldl> p $rating = ceil( random(4, 5) * 5 ) 
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

    CNTRD	=> Null
    FULL	=> 0
    NCLUS	=> 2
    NSEED	=> 4
    NTRY	=> 5
    R2CRT	=> 0.001
    V	        => 1
    ss total:	20.5
    iter 0 R2 [0.024390244 0.024390244 0.26829268  0.4796748  0.4796748]
    iter 1 R2 [0.46341463 0.46341463  0.4796748  0.4796748  0.4796748]

    perldl> p "$_\t$k{$_}\n" for (sort keys %k)

    R2      0.479674796747968
    centroid       # mean ratings for 2 group x 5 movies
    [
     [         3  2.3333333]
     [         2  4.3333333]
     [         5  2.6666667]
     [         3          3]
     [         4  2.6666667]
    ]
 
    cluster        # 4 persons' membership in two groups
    [
     [1 0 0 0]
     [0 1 1 1]
    ]
    
    n       [1 3]  # cluster size
    ss
    [
     [         0 0.66666667]
     [         0 0.66666667]
     [         0 0.66666667]
     [         0          8]
     [         0 0.66666667]
    ]

Now, for the valiant, kmeans is threadable. Say you gathered 10 persons' ratings on 5 movies from 2 countries, so the data is dim [10,5,2], and you want to put the 10 persons from each country into 3 clusters, just specify NCLUS => [3,1], and there you have it. The key is for NCLUS to include $data->ndims - 1 numbers. The 1 in [3,1] turns into a dummy dim, so the 3-cluster operation is repeated on both countries. Similarly, when seeding, CNTRD needs to have ndims that at least match the data ndims. Extra dims in CNTRD will lead to threading (convenient if you want to try out different centroid locations, for example, but you will have to hand pick the best result). See stats_kmeans.t for examples w 3D and 4D data.

*With bad value, R2 is based on average of variances instead of sum squared error. What's minimized is the average variance across clusters as compared to the original variance with all obs in one cluster. R2 in this case does not have the usual meaning of proportion of variance accounted for, but it does serve the purpose of minimizing variance. **With LOTS bad values, ie VERY sparse data, R2 may bounce around instead of monotonously decreasing. May be good idea to fill_m etc before kmeans instead.

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
  if ($opt{CNTRD}->nelem) {
    $opt{NTRY}  = 1;
    $opt{NSEED} = $self->dim(0);
  }
  else {
    $opt{NSEED} = pdl($self->dim(0), $opt{NSEED})->min;
  }
  $opt{V} and print STDERR "$_\t=> $opt{$_}\n" for (sort keys %opt);
 
  my $ss_ms = $self->badflag?  'ms' : 'ss';
  my $ss_total
    = $self->badflag?  $self->var->average : $self->ss->sumover;
  $opt{V} and print STDERR "overall $ss_ms:\t$ss_total\n";

  my ($cluster, $centroid, $ss_cv, $R2_this, $R2_last, $R2_change);

    # NTRY made into extra dim in $cluster for threading
  my @nclus = (ref $opt{NCLUS} eq 'ARRAY')? @{$opt{NCLUS}} : ($opt{NCLUS});
  $cluster
    = $opt{CNTRD}->nelem ?
      $self->assign( $opt{CNTRD}->dummy(-1) )  # put dummy(-1) to match NTRY
    : random_cluster($opt{NSEED}, @nclus, $opt{NTRY} )
    ;

  ($centroid, $ss_cv) = $self(0:$opt{NSEED} - 1, )->centroid( $cluster );
  my $ss_seed = $self->badflag?
                $self(0:$opt{NSEED}-1, )->var->average
              : $self(0:$opt{NSEED}-1, )->ss->sumover
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

    # xchg/mv(-1,0) leaves it as was if single dim--unlike transpose
  my $i_best = $R2_this->mv(-1,0)->maximum_ind;

  $R2_this->getndims == 1 and
    return (
      centroid => $centroid->dice_axis(-1,$i_best)->sever->squeeze,
      cluster  => $cluster->dice_axis(-1,$i_best)->sever->squeeze,
      n        => $cluster->dice_axis(-1,$i_best)->sever->squeeze->sumover,
      R2       => $R2_this->dice_axis(-1,$i_best)->sever->squeeze, 
      $ss_ms   => $ss_cv->dice_axis(-1,$i_best)->sever->squeeze,
    );

  # now for threading beyond 2D data

  # can't believe i'm using a perl loop :P

  $i_best = $i_best->flat->sever;
  my @i_best = map { $opt{NTRY} * $_ + $i_best(($_)) }
               0 .. $i_best->nelem - 1;

  my @shapes;
  for ($centroid, $cluster, $R2_this) {
    my @dims = $_->dims;
    pop @dims;
    push @shapes, \@dims;
  }

  $cluster = $cluster->mv(-1,2)->clump(2..$cluster->ndims-1)->dice_axis(2,\@i_best)->reshape( @{ $shapes[1] } )->sever,

  return (
    centroid =>
$centroid->mv(-1,2)->clump(2..$centroid->ndims-1)->dice_axis(2,\@i_best)->reshape( @{ $shapes[0] } )->sever,

    cluster  => $cluster,
    n        => $cluster->sumover,

    R2       =>
$R2_this->mv(-1,0)->clump(0..$R2_this->ndims-1)->dice_axis(0,\@i_best)->reshape( @{ $shapes[2] } )->sever,

    $ss_ms   => 
$ss_cv->mv(-1,2)->clump(2..$ss_cv->ndims-1)->dice_axis(2,\@i_best)->reshape( @{ $shapes[0] } )->sever,
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

=head2 pca_cluster

Assgin variables to components ie clusters based on pca loadings or scores. One way to seed kmeans (see Ding & He, 2004, and Su & Dy, 2004 for other ways of using pca with kmeans). Variables are assigned to their most associated component. Note that some components may not have any variable that is most associated with them, so the returned number of clusters may be smaller than NCOMP.

Default options (case insensitive):

  V     => 1,
  ABS   => 1,   # high pos and neg loadings on a comp in same cluster
  NCOMP => 10,  # max number of components to consider

Usage:

    # say we need to cluster a group of documents
  ($data, $idd, $idw) = get_data 'doc_word_info.txt';

  perldl> %p = $data->pca;
  perldl> $cluster  = $p{loading}->pca_cluster;

    # pca clusters var while kmeans clusters obs. hence transpose
  perldl> ($m, $ss) = $data->transpose->centroid( $cluster );
  perldl> %k = $data->transpose->kmeans( { cntrd=>$m } );

    # take a look at cluster 0 doc ids
  perldl> p join("\n", @$idd[ list which $k{cluster}->( ,0) ]);

=cut

*pca_cluster = \&PDL::pca_cluster;
sub PDL::pca_cluster {
  my ($self, $opt) = @_;

  my %opt = (
    V     => 1,
    ABS   => 1,   # high pos and neg loadings on a comp in same cluster
    NCOMP => 10,  # max number of components to consider
  );
  $opt and $opt{uc $_} = $opt->{$_} for (keys %$opt);

  $opt{NCOMP} = pdl($opt{NCOMP}, $self->dim(1))->min;

  my $c = $self->( ,0:$opt{NCOMP}-1)->transpose->abs->maximum_ind;

  if ($opt{ABS}) {
    $c = $c->iv_cluster;
  }
  else {
    my @c = map { ($self->($_,$c($_)) >= 0)? $c($_)*2 : $c($_)*2 + 1 }
                ( 0 .. $c->dim(0)-1 );
    $c = iv_cluster( \@c );
  }
  $opt{V} and print STDERR "cluster membership mask as " . $c->info . "\n";
  return $c;
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

Ding, C., & He, X. (2004). K-means clustering via principal component analysis. Proceedings of the 21st International Conference on Machine Learning, 69, 29.

Su, T., & Dy, J. (2004). A deterministic method for initializing K-means clustering. 16th IEEE International Conference on Tools with Artificial Intelligence, 784-786.

Romesburg, H.C. (1984). Cluster Analysis for Researchers. NC: Lulu Press.

Wikipedia (retrieved June, 2009). K-means clustering. http://en.wikipedia.org/wiki/K-means_algorithm

=head1 AUTHOR

Copyright (C) 2009 Maggie J. Xiong <maggiexyz users.sourceforge.net>

All rights reserved. There is no warranty. You are allowed to redistribute this software / documentation as described in the file COPYING in the PDL distribution.

=cut



;



# Exit with OK status

1;

		   