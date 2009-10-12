#!/usr/bin/perl 

use strict;
use warnings;
use Test::More;

BEGIN {
    plan tests => 35;
      # 1-2
    use_ok( 'PDL::Stats::Basic' );
    use_ok( 'PDL::Stats::GLM' );
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

my $a = sequence 5;
my $b = pdl(0, 0, 0, 1, 1);

  # 3
is( t_fill_m(), 1 );
sub t_fill_m {
  my $aa = sequence 5;
  $aa = $aa->setvaltobad(0);
  tapprox( $aa->fill_m->sum, 12.5 );
}

  # 4
is( t_fill_rand(), 1 );
sub t_fill_rand {
  my $aa = sequence 5;
  $aa = $aa->setvaltobad(0);
  my $stdv = $aa->fill_rand->stdv;
  tapprox( $stdv, 1.01980390271856 ) || tapprox( $stdv, 1.16619037896906 );
}
  # 5-6
is( tapprox( $a->dev_m->avg, 0 ), 1 );
is( tapprox( $a->stddz->avg, 0 ), 1 );
  # 7-9
is( tapprox( $a->sse($b), 18), 1 );
is( tapprox( $a->mse($b), 3.6), 1 );
is( tapprox( $a->rmse($b), 1.89736659610103 ), 1 );
  # 10
is( tapprox( $b->glue(1,ones(5))->pred_logistic(pdl(1,2))->sum, 4.54753948757851 ), 1 );

my $y = pdl(0, 1, 0, 1, 0);
  # 11-13
is( tapprox( $y->d0(), 6.73011667009256 ), 1 );
is( tapprox( $y->dm( ones(5) * .5 ), 6.93147180559945 ), 1 );
is( tapprox( sum($y->dvrs(ones(5) * .5) ** 2), 6.93147180559945 ), 1 );
  # 14-15
{
  my $a = pdl(ushort, [0,0,1,0,1], [0,0,0,1,1] );
  my $b = cat sequence(5), sequence(5)**2;
  $b = cat $b, $b * 2;
  my %m = $a->ols_t($b->dummy(2));
  my $rsq = pdl( [
                  [ 0.33333333, 0.80952381 ],
                  [ 0.33333333, 0.80952381 ],
                 ],
            );
  is( tapprox( sum( $m{R2} - $rsq ), 0 ), 1 );

  my %m0 = $a->ols_t(sequence(5), {CONST=>0});
  my $b0 = pdl ([ 0.2 ], [ 0.23333333 ]);

  is( tapprox( sum( $m0{b} - $b0 ), 0 ), 1 );
}

  # 16
is( tapprox( t_ols(), 0 ), 1 );
sub t_ols {
  my $a = sequence 5;
  my $b = pdl(0,0,0,1,1);
  my %m = $a->ols($b);
  my %a = (
    F    => 9,
    F_df => pdl(1,3),
    R2   => .75,
    b    => pdl(2.5, 1),
    b_se => pdl(0.83333333, 0.52704628),
    b_t  => pdl(3, 1.8973666),
    ss_total => 10,
    ss_model => 7.5,
  );
  my $sum;
  $sum += sum($a{$_} - $m{$_})
    for (keys %a);
  return $sum;
}

  # 17
is( tapprox( t_r2_change(), 0 ), 1 );
sub t_r2_change {
  my $a = sequence 5, 2;
  my $b = pdl(0,0,0,1,1);
  my $c = pdl(0,0,2,2,2);
  my %m = $a->r2_change( $b, cat $b, $c );
  my %a = (
F_change  => pdl(3, 3),
F_df      => pdl(1, 2),
R2_change => pdl(.15, .15),
  );
  my $sum;
  $sum += sum($a{$_} - $m{$_})
    for (keys %a);
  return $sum;
}

  # 18
is( tapprox( t_pca(), 0 ), 1 );
sub t_pca {
  my $a = sequence 10, 5;
  $a->where($a % 7 == 0) .= 0;

  my %m = $a->pca({PLOT=>0});
  my %a = (
value => pdl(1.59696,1.17391,1.05055,0.603594,0.574989),
var   => pdl(0.319391,0.234782,0.21011,0.120719,0.114998),
  );
  my $sum;
  $sum += sum($a{$_} - $m{$_})
    for (keys %a);
  return $sum / 10;
}

  # 19
is( tapprox( t_pca_sorti(), 0 ), 1 );
sub t_pca_sorti {
  my $a = sequence 10, 5;
  $a->where($a % 7 == 0) .= 0;

  my %m = $a->pca({PLOT=>0});

  my ($iv, $ic) = $m{loading}->pca_sorti;

  return sum($iv - pdl(qw(4 1 0 2 3))) + sum($ic - pdl(qw( 0 1 2 )));
}

  # 20
SKIP: {
  eval { require PDL::Fit::LM; };
  skip 'no PDL::Fit::LM', 1 if $@;

  is( tapprox( t_logistic(), 0 ), 1 );
}
sub t_logistic {
  my $y = pdl( 0, 0, 0, 1, 1 );
  my $x = sequence(5) + 1;
  my %m = $y->logistic( $x );
  my $y_pred = $x->glue(1, ones(5))->pred_logistic( $m{b} );
  return sum( $y - $y_pred, $m{Dm_chisq} - 6.73011667009256 );
}

my $a_bad = sequence 6;
$a_bad->setbadat(-1);
my $b_bad = pdl(0, 0, 0, 0, 1, 1);
$b_bad->setbadat(0);

  # 21 
is( tapprox( $a_bad->dev_m->avg, 0 ), 1 );
is( tapprox( $a_bad->stddz->avg, 0 ), 1 );
  # 23
is( tapprox( $a_bad->sse($b_bad), 23), 1 );
is( tapprox( $a_bad->mse($b_bad), 5.75), 1 );
is( tapprox( $a_bad->rmse($b_bad), 2.39791576165636 ), 1 );
  # 26
is( tapprox( $b_bad->glue(1,ones(6))->pred_logistic(pdl(1,2))->sum, 4.54753948757851 ), 1 );

  # 27
is( tapprox( $b_bad->d0(), 6.73011667009256 ), 1 );
is( tapprox( $b_bad->dm( ones(6) * .5 ), 6.93147180559945 ), 1 );
is( tapprox( sum($b_bad->dvrs(ones(6) * .5) ** 2), 6.93147180559945 ), 1 );

  # 30
is( tapprox( t_effect_code_w(), 0 ), 1 );
sub t_effect_code_w {
  my @a = qw( a a a b b b b c c c );
  my $a = effect_code_w(\@a);
  return sum($a->sumover - pdl byte, (0, 0));
}

  # 31
is( tapprox( t_anova(), 0 ), 1 );
sub t_anova {
  my $d = sequence 60;
  my @a = map {$a = $_; map { $a } 0..14 } qw(a b c d);
  my $b = $d % 3;
  my $c = $d % 2;
  $d(20) .= 10;
  my %m = $d->anova(\@a, $b, $c, {IVNM=>[qw(A B C)], plot=>0});
  my $ans_F = pdl(165.252100840336, 0.0756302521008415);
  my $ans_m = pdl([qw(8 18 38 53)], [qw(8 23 38 53)]);
  return  sum( pdl( @m{'| A | F', '| A ~ B ~ C | F'} ) - $ans_F )
        + sum( $m{'# A ~ B ~ C # m'}->(,2,)->squeeze - $ans_m )
  ;
}

  # 32
is( tapprox( t_anova_1way(), 0 ), 1 );
sub t_anova_1way {
  my $d = pdl qw( 3 2 1 5 2 1 5 3 1 4 1 2 3 5 5 );
  my $a = qsort sequence(15) % 3;
  my %m = $d->anova($a, {plot=>0});
  my $ans_F  = 0.160919540229886;
  my $ans_ms = 0.466666666666669;
  my $ans_m = pdl(qw( 2.6 2.8 3.2 ));
  return  ($m{F} - $ans_F)
        + ($m{ms_model} - $ans_ms )
        + sum( $m{'# IV_0 # m'}->squeeze - $ans_m )
  ;
}

  # 33
is( tapprox( t_anova_bad(), 0 ), 1 );
sub t_anova_bad {
  my $d = sequence 60;
  $d(20) .= 10;
  $d->setbadat(1);
  $d->setbadat(10);
  my @a = map {$a = $_; map { $a } 0..14 } qw(a b c d);
  my $b = sequence(60) % 3;
  my $c = sequence(60) % 2;
  my %m = $d->anova(\@a, $b, $c, {IVNM=>[qw(A B C)], plot=>0, v=>0});
  my $ans_F = pdl( 150.00306433446, 0.17534855325553 );
  my $ans_m = pdl([qw( 4 22 37 52 )], [qw( 10 22 37 52 )]);
  my $ans_se = pdl([qw( 0 6 3 6 )], [qw( 1.7320508 3 6 3 )]);
  return  sum( pdl( @m{'| A | F', '| A ~ B ~ C | F'} ) - $ans_F )
        + sum( $m{'# A ~ B ~ C # m'}->(,2,)->squeeze - $ans_m )
        + sum( $m{'# A ~ B ~ C # se'}->(,2,)->squeeze - $ans_se )
  ;
}

  # 34
{
  my $a = sequence 5, 2;
  $a( ,1) .= 0;
  $a = $a->setvaltobad(0);
  is( $a->fill_m->setvaltobad(0)->nbad, 5, 'fill_m nan to bad');
}
  # 35
{
  my $a = ones 3, 2;
  $a( ,1) .= 2;
  is( which($a->stddz == 0)->nelem, 6, 'stddz nan vs bad');
}
