#!/usr/bin/perl

pp_add_exported('', 'get_data', 'which_id', 
);

pp_addpm({At=>'Top'}, <<'EOD');

use PDL::LiteF;
use PDL::NiceSlice;
use Carp;

=head1 NAME

PDL::Stats::Basic -- basic statistics and related utilities

=head1 DESCRIPTION

The terms FUNCTIONS and METHODS are arbitrarily used to refer to methods that are threadable and methods that are NOT threadable, respectively.

Does not have mean or median function here. see SEE ALSO.

=head1 SYNOPSIS

    use PDL::LiteF;
    use PDL::NiceSlice;
    use PDL::Stats::Basic;

    my $stdv = $data->stdv;

or

    my $stdv = stdv( $data );  

=cut

EOD

pp_addhdr('
#include <math.h>

'
);

pp_def('stdv',
  Pars      => 'a(n); float+ [o]b()',
  GenericTypes => [F, D],
  HandleBad => 1,
  Code      => '
    $GENERIC(b) sa, a2;
    sa = 0; a2 = 0;
    long N = $SIZE(n);
    loop (n) %{
      sa += $a();
      a2 += pow($a(), 2);
    %}
    $b() = sqrt( a2 / N - pow(sa/N,2) );
  ',
  BadCode  => '
    $GENERIC(b) sa, a2;
    sa = 0; a2 = 0;
    long N = 0;
    loop (n) %{
      if ( $ISGOOD($a()) ) {
	sa += $a();
	a2 += pow($a(), 2);
	N  ++;
      }
    %}
    $b() = sqrt( a2 / N - pow(sa/N,2) );
  ',
  Doc      => '

=for ref

Sample standard deviation.

  ',

);

pp_def('stdv_unbiased',
  Pars      => 'a(n); float+ [o]b()',
  GenericTypes => [F, D],
  HandleBad => 1,
  Code      => '
    $GENERIC(b) sa, a2;
    sa = 0; a2 = 0;
    long N = $SIZE(n);
    loop (n) %{
      sa += $a();
      a2 += pow($a(), 2);
    %}
    $b() = pow( a2/(N-1) - pow(sa/N,2) * N/(N-1), .5 );
  ',
  BadCode  => '
    $GENERIC(b) sa, a2;
    sa = 0; a2 = 0;
    long N = 0;
    loop (n) %{
      if ( $ISGOOD($a()) ) {
	sa += $a();
	a2 += pow($a(), 2);
	N  ++;
      }
    %}
    $b() = pow( a2/(N-1) - pow(sa/N,2) * N/(N-1), .5 );
  ',
  Doc      => '

=for ref

Unbiased estimate of population standard deviation.

  ',

);

pp_def('var',
  Pars      => 'a(n); float+ [o]b()',
  GenericTypes => [F, D],
  HandleBad => 1,
  Code      => '
    $GENERIC(b) a2, sa;
    a2 = 0; sa = 0;
    long N = $SIZE(n);
    loop (n) %{
      a2 += pow($a(), 2);
      sa += $a();
    %}
    $b() = a2 / N - pow(sa/N, 2);
  ',
  BadCode  => '
    $GENERIC(b) a2, sa;
    a2 = 0; sa = 0;
    long N = 0;
    loop (n) %{
      if ( $ISGOOD($a()) ) {
        a2 += pow($a(), 2);
        sa += $a();
	N  ++;
      }
    %}
    $b() = a2 / N - pow(sa/N, 2);
  ',
  Doc      => '

=for ref

Sample variance.

  ',

);

pp_def('var_unbiased',
  Pars      => 'a(n); float+ [o]b()',
  GenericTypes => [F, D],
  HandleBad => 1,
  Code      => '
    $GENERIC(b) a2, sa;
    a2 = 0; sa = 0;
    long N = $SIZE(n);
    loop (n) %{
      a2 += pow($a(), 2);
      sa += $a();
    %}
    $b() = (a2 - pow(sa/N, 2) * N) / (N-1);
  ',
  BadCode  => '
    $GENERIC(b) a2, sa;
    a2 = 0; sa = 0;
    long N = 0;
    loop (n) %{
      if ( $ISGOOD($a()) ) {
        a2 += pow($a(), 2);
        sa += $a();
	N  ++;
      }
    %}
    $b() = (a2 - pow(sa/N, 2) * N) / (N-1);
  ',
  Doc      => '

=for ref

Unbiased estimate of population variance.

  ',

);

pp_def('se',
  Pars      => 'a(n); float+ [o]b()',
  GenericTypes => [F, D],
  HandleBad => 1,
  Code      => '
    $GENERIC(b) sa, a2;
    sa = 0; a2 = 0;
    long N = $SIZE(n);
    loop (n) %{
      sa += $a();
      a2 += pow($a(), 2);
    %}
    $b() = sqrt( (a2/(N-1) - pow(sa/N,2) * N/(N-1)) / N );
  ',
  BadCode  => '
    $GENERIC(b) sa, a2;
    sa = 0; a2 = 0;
    long N = 0;
    loop (n) %{
      if ( $ISGOOD($a()) ) {
	sa += $a();
	a2 += pow($a(), 2);
	N  ++;
      }
    %}
    $b() = sqrt( (a2/(N-1) - pow(sa/N,2) * N/(N-1)) / N );
  ',
  Doc      => '

=for ref

Standard error of the mean. Useful for calculating confidence intervals.

=for usage

    # 95% confidence interval for samples with large N

    $ci_95_upper = $data->average + 1.96 * $data->se;
    $ci_95_lower = $data->average - 1.96 * $data->se;

  ',

);

pp_def('ss',
  Pars      => 'a(n); float+ [o]b()',
  GenericTypes => [F, D],
  HandleBad => 1,
  Code      => '
    $GENERIC(b) sa, a2;
    sa = 0; a2 = 0;
    long N = $SIZE(n);
    loop (n) %{
      sa += $a();
      a2 += pow($a(), 2);
    %}
    $b() = a2 - N * pow(sa/N,2);
  ',
  BadCode  => '
    $GENERIC(b) sa, a2;
    sa = 0; a2 = 0;
    long N = 0;
    loop (n) %{
      if ( $ISGOOD($a()) ) {
	sa += $a();
	a2 += pow($a(), 2);
	N  ++;
      }
    %}
    $b() = a2 - N * pow(sa/N,2);
  ',
  Doc      => '

=for ref

Sum of squared deviations from the mean.

  ',

);

pp_def('skew',
  Pars      => 'a(n); float+ [o]b()',
  GenericTypes => [F, D],
  HandleBad => 1,
  Code      => '
    $GENERIC(b) sa, m, d, d2, d3;
    sa = 0; d2 = 0; d3 = 0;
    long N = $SIZE(n);
    loop (n) %{
      sa += $a();
    %}
    m = sa / N;
    loop (n) %{
      d   = $a() - m;
      d2 += pow(d, 2);
      d3 += pow(d, 3);
    %}
    $b() = d3/N / pow(d2/N, 1.5);
  ',
  BadCode  => '
    $GENERIC(b) sa, m, d, d2, d3;
    sa = 0; d2 = 0; d3 = 0;
    long N = 0;
    loop (n) %{
      if ( $ISGOOD($a()) ) {
        sa += $a();
        N ++;
      }
    %}
    m = sa / N;
    loop (n) %{
      if ( $ISGOOD($a()) ) {
	d   = $a() - m;
	d2 += pow(d, 2);
	d3 += pow(d, 3);
      }
    %}
    $b() = d3/N / pow(d2/N, 1.5);
  ',
  Doc      => '

=for ref

Sample skewness, measure of asymmetry in data. skewness == 0 for normal distribution.

  ',

);

pp_def('skew_unbiased',
  Pars      => 'a(n); float+ [o]b()',
  GenericTypes => [F, D],
  HandleBad => 1,
  Code      => '
    $GENERIC(b) sa, m, d, d2, d3;
    sa = 0; d2 = 0; d3 = 0;
    long N = $SIZE(n);
    loop (n) %{
      sa += $a();
    %}
    m = sa / N;
    loop (n) %{
      d   = $a() - m;
      d2 += pow(d, 2);
      d3 += pow(d, 3);
    %}
    $b() = sqrt(N*(N-1)) / (N-2) * d3/N / pow(d2/N, 1.5);
  ',
  BadCode  => '
    $GENERIC(b) sa, m, d, d2, d3;
    sa = 0; d2 = 0; d3 = 0;
    long N = 0;
    loop (n) %{
      if ( $ISGOOD($a()) ) {
        sa += $a();
        N ++;
      }
    %}
    m = sa / N;
    loop (n) %{
      if ( $ISGOOD($a()) ) {
	d   = $a() - m;
	d2 += pow(d, 2);
	d3 += pow(d, 3);
      }
    %}
    $b() = sqrt(N*(N-1)) / (N-2) * d3/N / pow(d2/N, 1.5);
  ',
  Doc      => '

=for ref

Unbiased estimate of population skewness. This is the number in GNumeric Descriptive Statistics.

  ',

);

pp_def('kurt',
  Pars      => 'a(n); float+ [o]b()',
  GenericTypes => [F, D],
  HandleBad => 1,
  Code      => '
    $GENERIC(b) sa, m, d, d2, d4;
    sa = 0; d2 = 0; d4 = 0;
    long N = $SIZE(n);
    loop (n) %{
      sa += $a();
    %}
    m = sa / N;
    loop (n) %{
      d   = $a() - m;
      d2 += pow(d, 2);
      d4 += pow(d, 4);
    %}
    $b() = N * d4 / pow(d2,2) - 3;
  ',
  BadCode  => '
    $GENERIC(b) sa, m, d, d2, d4;
    sa = 0; d2 = 0; d4 = 0;
    long N = 0;
    loop (n) %{
      if ( $ISGOOD($a()) ) {
        sa += $a();
        N ++;
      }
    %}
    m = sa / N;
    loop (n) %{
      if ( $ISGOOD($a()) ) {
	d   = $a() - m;
	d2 += pow(d, 2);
	d4 += pow(d, 4);
      }
    %}
    $b() = N * d4 / pow(d2,2) - 3;
  ',
  Doc      => '

=for ref

Sample kurtosis, measure of "peakedness" of data. kurtosis == 0 for normal distribution. 

  ',

);

pp_def('kurt_unbiased',
  Pars      => 'a(n); float+ [o]b()',
  GenericTypes => [F, D],
  HandleBad => 1,
  Code      => '
    $GENERIC(b) sa, m, d, d2, d4;
    sa = 0; d2 = 0; d4 = 0;
    long N = $SIZE(n);
    loop (n) %{
      sa += $a();
    %}
    m = sa / N;
    loop (n) %{
      d   = $a() - m;
      d2 += pow(d, 2);
      d4 += pow(d, 4);
    %}
    $b() = ((N-1)*N*(N+1) * d4 / pow(d2,2) - 3 * pow(N-1,2)) / ((N-2)*(N-3));
  ',
  BadCode  => '
    $GENERIC(b) sa, m, d, d2, d4;
    sa = 0; d2 = 0; d4 = 0;
    long N = 0;
    loop (n) %{
      if ( $ISGOOD($a()) ) {
        sa += $a();
        N ++;
      }
    %}
    m = sa / N;
    loop (n) %{
      if ( $ISGOOD($a()) ) {
	d   = $a() - m;
	d2 += pow(d, 2);
	d4 += pow(d, 4);
      }
    %}
    $b() = ((N-1)*N*(N+1) * d4 / pow(d2,2) - 3 * pow(N-1,2)) / ((N-2)*(N-3));
  ',
  Doc      => '

=for ref

Unbiased estimate of population kurtosis. This is the number in GNumeric Descriptive Statistics.

  ',

);


pp_def('cov',
  Pars      => 'a(n); b(n); float+ [o]c()',
  GenericTypes => [F, D],
  HandleBad => 1,
  Code      => '
    $GENERIC(c) ab, sa, sb, cov;
    ab = 0; sa = 0; sb = 0;
    long N = $SIZE(n);
    loop (n) %{
      ab += $a() * $b();
      sa += $a();
      sb += $b();
    %}
    $c() = ab / N - (sa/N) * (sb/N);
  ',
  BadCode  => '
    $GENERIC(c) ab, sa, sb, cov;
    ab = 0; sa = 0; sb = 0;
    long N = 0;
    loop (n) %{
      if ( $ISBAD($a()) || $ISBAD($b()) ) { }
      else {
	ab += $a() * $b();
	sa += $a();
	sb += $b();
	N  ++;
      }
    %}
    $c() = ab / N - (sa/N) * (sb/N);
  ',
  Doc      => '

=for ref

Sample covariance. see B<corr> for ways to call

  ',

);

pp_def('corr',
  Pars      => 'a(n); b(n); float+ [o]c()',
  GenericTypes => [F, D],
  HandleBad => 1,
  Code      => '
    $GENERIC(c) ab, sa, sb, a2, b2, cov, va, vb;
    ab = 0; sa = 0; sb = 0; a2 = 0; b2 = 0;
    long N = $SIZE(n);
    if (N >= 2 ) {
      loop (n) %{
	ab += $a() * $b();
	sa += $a();
	sb += $b();
	a2 += pow($a(), 2);
	b2 += pow($b(), 2);
      %}
/*  in fact cov * N, va * N, and vb * N  */
      cov = ab - (sa * sb) / N;
      va  = a2 - pow(sa,2) / N;
      vb  = b2 - pow(sb,2) / N;
      $c() = cov / sqrt( va * vb );
    }
    else {
      barf( "too few N" );
    }
  ',
  BadCode  => '
    $GENERIC(c) ab, sa, sb, a2, b2, cov, va, vb;
    ab = 0; sa = 0; sb = 0; a2 = 0; b2 = 0;
    long N = 0;
    loop (n) %{
      if ( $ISBAD($a()) || $ISBAD($b()) ) { }
      else {
	ab += $a() * $b();
	sa += $a();
	sb += $b();
	a2 += pow($a(), 2);
	b2 += pow($b(), 2);
	N  ++;
      }
    %}
    if ( N >= 2 ) {
      cov = ab - (sa * sb) / N;
      va  = a2 - pow(sa,2) / N;
      vb  = b2 - pow(sb,2) / N;
      $c() = cov / sqrt( va * vb );
    }
    else {
      $SETBAD($c());
    }
  ',
  Doc      => '

=for ref

Pearson correlation coefficient. r = cov(X,Y) / (stdv(X) * stdv(Y)).

=for usage 

Usage:

    perldl> $a = random 5, 3
    perldl> $b = sequence 5,3
    perldl> p $a->corr($b)

    [0.20934208 0.30949881 0.26713007]

for square corr table

    perldl> p $a->corr($a->dummy(1))

    [
     [           1  -0.41995259 -0.029301192]
     [ -0.41995259            1  -0.61927619]
     [-0.029301192  -0.61927619            1]
    ]

but it is easier and faster to use B<corr_table>.

  ',

);

pp_def('corr_table',
  Pars      => 'a(n,m); float+ [o]c(m,m)',
  HandleBad => 1,
  Code      => '

long N, M;
N = $SIZE(n); M = $SIZE(m);
$GENERIC(a) a_, b_;
$GENERIC(c) ab, sa, sb, a2, b2, cov, va, vb, r;

if (N >= 2 ) {
  long i, j;
  for (i=0; i<M; i++) {
    for (j=i+1; j<M; j++) {
      ab = 0; sa = 0; sb = 0; a2 = 0; b2 = 0;
      loop (n) %{
        a_ = $a(n=>n,m=>i);
        b_ = $a(n=>n,m=>j);
    	ab += a_ * b_;
    	sa += a_;
    	sb += b_;
    	a2 += pow(a_, 2);
    	b2 += pow(b_, 2);
      %}
      cov = ab - (sa * sb) / N;
      va  = a2 - pow(sa,2) / N;
      vb  = b2 - pow(sb,2) / N;
      r   = cov / sqrt( va * vb );
      $c(m0=>i, m1=>j) = r;
      $c(m0=>j, m1=>i) = r;
    }
    $c(m0=>i, m1=>i) = 1.0;
  }
}
else {
  barf( "too few N" );
}

  ',
  BadCode  => '

if ($SIZE(n) >= 2 ) {
  $GENERIC(a) a_, b_;
  $GENERIC(c) ab, sa, sb, a2, b2, cov, va, vb, r;
  long N, M, i, j;
  M = $SIZE(m);
  for (i=0; i<M; i++) {
    for (j=i+1; j<M; j++) {
      ab = 0; sa = 0; sb = 0; a2 = 0; b2 = 0; N=0;
      loop (n) %{
        if ($ISBAD($a(n=>n, m=>i)) || $ISBAD($a(n=>n, m=>j))) { }
        else { 
          a_ = $a(n=>n,m=>i);
          b_ = $a(n=>n,m=>j);
          ab += a_ * b_;
          sa += a_;
          sb += b_;
          a2 += pow(a_, 2);
          b2 += pow(b_, 2);
          N ++;
        }
      %}
      if (N>=2) {
        cov = ab - (sa * sb) / N;
        va  = a2 - pow(sa,2) / N;
        vb  = b2 - pow(sb,2) / N;
        r   = cov / sqrt( va * vb );
        $c(m0=>i, m1=>j) = r;
        $c(m0=>j, m1=>i) = r;
      }
      else {
        $SETBAD($c(m0=>i, m1=>j));
        $SETBAD($c(m0=>j, m1=>i));
      }
    }
    N=0;
    loop (n) %{
      if ($ISGOOD($a(n=>n,m=>i)))
        N ++;
      if (N>=2)
        break;
    %}
    if (N>=2) {  $c(m0=>i, m1=>i) = 1.0;  }
    else      {  $SETBAD($c(m0=>i, m1=>i)); }
  }
}
else {
  barf( "too few N" );
}

  ',
  Doc      => '

=for ref

Square Pearson correlation table. Gives the same result as threading using B<corr> but it calculates only half the square, hence much faster. And it is easier to use with higher dimension pdls.

=for usage

Usage:

    # 5 obs x 3 var, 2 such data tables
 
    perldl> $a = random 5, 3, 2
    
    perldl> p $a->corr_table
    [
     [
     [          1 -0.69835951 -0.18549048]
     [-0.69835951           1  0.72481605]
     [-0.18549048  0.72481605           1]
    ]
    [
     [          1  0.82722569 -0.71779883]
     [ 0.82722569           1 -0.63938828]
     [-0.71779883 -0.63938828           1]
     ]
    ]

for the same result using B<corr>,

    perldl> p $a->dummy(2)->corr($a->dummy(1)) 

This is also how to use B<t_corr> and B<n_pair> with such a table.

  ',

);

pp_def('t_corr',
  Pars      => 'r(); n(); [o]t()',
  GenericTypes => [F, D],
  HandleBad => 1,
  Code      => '
    $t() = $r() / pow( (1 - pow($r(), 2)) / ($n() - 2) , .5);
  ',
  BadCode   => '
    if ($ISBAD(r()) || $ISBAD(n()) ) {
      $SETBAD( $t() );
    }
    else {
      $t() = $r() / pow( (1 - pow($r(), 2)) / ($n() - 2) , .5);
    }
  ',
  Doc       => '

=for usage

    $corr   = $data->corr( $data->dummy(1) );
    $n      = $data->n_pair( $data->dummy(1) );
    $t_corr = $corr->t_corr( $n );

    use PDL::GSL::CDF;

    $p_2tail = 2 * (1 - gsl_cdf_tdist_P( $t_corr->abs, $n-2 ));

=for ref

t significance test for Pearson correlations.

  ',

);

pp_def('n_pair',
  Pars      => 'a(n); b(n); int [o]c()',
  GenericTypes => [L],
  HandleBad => 1,
  Code      => '
    $c() = $SIZE(n);
  ',
  BadCode   => '
    long N = 0;
    loop(n) %{
      if ( $ISBAD($a()) || $ISBAD($b()) ) { }
      else {
        N ++;
      }
    %}
    $c() = N;
  ',
  Doc       => '

=for ref

Returns the number of good pairs between 2 lists. Useful with B<corr> (esp. when bad values are involved)

  ',

);

pp_def('corr_dev',
  Pars      => 'a(n); b(n); float+ [o]c()',
  GenericTypes => [F, D],
  HandleBad => 1,
  Code      => '
    $GENERIC(c) ab, a2, b2, cov, va, vb;
    ab = 0; a2 = 0; b2 = 0;
    long N = $SIZE(n);
    if (N >= 2 ) {
      loop (n) %{
	ab += $a() * $b();
	a2 += pow($a(), 2);
	b2 += pow($b(), 2);
      %}
      cov = ab / N;
      va  = a2 / N;
      vb  = b2 / N;
      $c() = cov / sqrt( va * vb );
    }
    else {
      barf( "too few N" );
    }
  ',
  BadCode   => '
    $GENERIC(c) ab, a2, b2, cov, va, vb;
    ab = 0; a2 = 0; b2 = 0;
    long N = 0;
    loop (n) %{
      if ( $ISBAD($a()) || $ISBAD($b()) ) { }
      else {
	ab += $a() * $b();
	a2 += pow($a(), 2);
	b2 += pow($b(), 2);
        N  ++;
      }
    %}
    if (N >= 2) {
      cov = ab / N;
      va  = a2 / N;
      vb  = b2 / N;
      $c() = cov / sqrt( va * vb );
    }
    else {
      $SETBAD($c());
    }
  ',
  Doc       => '

=for usage

    $corr = $a->dev_m->corr_dev($b->dev_m);

=for ref

Calculates correlations from B<dev_m> vals. Seems faster than doing B<corr> from original vals when data pdl is big

  ',
);

pp_def('t_test',
  Pars      => 'a(n); b(m); float+ [o]t(); [o]d()',
  GenericTypes => [F, D],
  HandleBad => 1,
  Code      => '
    $GENERIC(t) N, M, sa, sb, a2, b2, va, vb, sdiff;
    sa = 0; sb = 0; a2 = 0; b2 = 0;
    N = $SIZE(n);
    M = $SIZE(m);
    if (N >=2 && M >= 2 ) {
      loop (n) %{
        sa += $a();
	a2 += pow($a(), 2);
      %}
      loop (m) %{
        sb += $b();
	b2 += pow($b(), 2);
      %}

      $d() = N + M - 2;

      va = (a2 - pow(sa/N, 2) * N) / (N-1);
      vb = (b2 - pow(sb/M, 2) * M) / (M-1);
      sdiff = sqrt( (1/N + 1/M) * ((N-1)*va + (M-1)*vb) / $d() );
      $t() = (sa/N - sb/M) / sdiff;
    }
    else {
      barf( "too few N" );
    }
  ',
  BadCode   => '
    $GENERIC(t) N, M, sa, sb, a2, b2, va, vb, sdiff;
    sa = 0; sb = 0; a2 = 0; b2 = 0;
    N = 0;
    M = 0;
    loop (n) %{
      if ( $ISGOOD($a()) ) {
        sa += $a();
	a2 += pow($a(), 2);
        N ++;
      }
    %}
    loop (m) %{
      if ( $ISGOOD($b()) ) {
        sb += $b();
	b2 += pow($b(), 2);
        M ++;
      }
    %}
    if (N >= 2 && M >= 2) {
      $d() = N + M - 2;

      va = (a2 - pow(sa/N, 2) * N) / (N-1);
      vb = (b2 - pow(sb/M, 2) * M) / (M-1);
      sdiff = sqrt( (1/N + 1/M) * ((N-1)*va + (M-1)*vb) / $d() );
      $t() = (sa/N - sb/M) / sdiff;
    }
    else {
      $SETBAD($t());
      $SETBAD($d());
    }
  ',
  Doc       => '

=for usage

    my ($t, $df) = t_test( $pdl1, $pdl2 );

    use PDL::GSL::CDF;

    my $p_2tail = 2 * (1 - gsl_cdf_tdist_P( $t->abs, $df ));

=for ref

Independent sample t-test, assuming equal var.

  ',
);

pp_def('t_test_nev',
  Pars      => 'a(n); b(m); float+ [o]t(); [o]d()',
  GenericTypes => [F, D],
  HandleBad => 1,
  Code      => '
    $GENERIC(t) N, M, sa, sb, a2, b2, se_a_2, se_b_2, sdiff;
    sa = 0; sb = 0; a2 = 0; b2 = 0;
    N = $SIZE(n);
    M = $SIZE(m);
    if (N >=2 && M >= 2 ) {
      loop (n) %{
        sa += $a();
	a2 += pow($a(), 2);
      %}
      loop (m) %{
        sb += $b();
	b2 += pow($b(), 2);
      %}

      se_a_2 = (a2 - pow(sa/N,2)*N) / (N*(N-1));
      se_b_2 = (b2 - pow(sb/M,2)*M) / (M*(M-1));
      sdiff = sqrt( se_a_2 + se_b_2 );

      $t() = (sa/N - sb/M) / sdiff;
      $d() = pow(se_a_2 + se_b_2, 2)
           / ( pow(se_a_2,2) / (N-1) + pow(se_b_2,2) / (M-1) )
           ;
    }
    else {
      barf( "too few N" );
    }
  ',
  BadCode   => '
    $GENERIC(t) N, M, sa, sb, a2, b2, se_a_2, se_b_2, sdiff;
    sa = 0; sb = 0; a2 = 0; b2 = 0;
    N = 0;
    M = 0;
    loop (n) %{
      if ( $ISGOOD($a()) ) {
        sa += $a();
	a2 += pow($a(), 2);
        N ++;
      }
    %}
    loop (m) %{
      if ( $ISGOOD($b()) ) {
        sb += $b();
	b2 += pow($b(), 2);
        M ++;
      }
    %}
    if (N >= 2 && M >= 2) {
      se_a_2 = (a2 - pow(sa/N,2)*N) / (N*(N-1));
      se_b_2 = (b2 - pow(sb/M,2)*M) / (M*(M-1));
      sdiff = sqrt( se_a_2 + se_b_2 );

      $t() = (sa/N - sb/M) / sdiff;
      $d() = pow(se_a_2 + se_b_2, 2)
           / ( pow(se_a_2,2) / (N-1) + pow(se_b_2,2) / (M-1) )
           ;
    }
    else {
      $SETBAD($t());
      $SETBAD($d());
    }
  ',
  Doc       => '

=for ref

Independent sample t-test, NOT assuming equal var. ie Welch two sample t test. Df follows Welch-Satterthwaite equation instead of Satterthwaite (1946, as cited by Hays, 1994, 5th ed.). It matches GNumeric, which matches R.

=for usage

    my ($t, $df) = $pdl1->t_test( $pdl2 );

  ',
);

pp_def('t_test_paired',
  Pars      => 'a(n); b(n); float+ [o]t(); [o]d()',
  GenericTypes => [F, D],
  HandleBad => 1,
  Code      => '
    $GENERIC(t) N, diff, s_dif, diff2;
    s_dif = 0; diff2 = 0;
    N = $SIZE(n);
    if (N >=2 ) {
      loop (n) %{
        diff = $a() - $b();
        s_dif += diff;
	diff2 += pow(diff, 2);
      %}

      $d() = N - 1;
      $t() = s_dif / sqrt( N * ( diff2 - pow(s_dif/N,2)*N ) / (N-1) );
    }
    else {
      barf( "too few N" );
    }
  ',
  BadCode   => '
    $GENERIC(t) N, diff, s_dif, diff2;
    s_dif = 0; diff2 = 0;
    N = 0;
    loop (n) %{
      if ( $ISBAD($a()) || $ISBAD($b()) ) { }
      else {
        diff = $a() - $b();
        s_dif += diff;
	diff2 += pow(diff, 2);
        N ++;
      }
    %}
    if (N >= 2 ) {
      $d() = N - 1;
      $t() = s_dif / sqrt( N * ( diff2 - pow(s_dif/N,2)*N ) / (N-1) );
    }
    else {
      $SETBAD($t());
      $SETBAD($d());
    }
  ',
  Doc       => '

=for ref

Paired sample t-test.

  ',
);

pp_addpm(<<'EOD');

=head1 METHODS

=head2 get_data

=for ref

Reads either file or file handle*. Returns observation x variable pdl and var and obs ids if specified. Ids in perl @ ref to allow for non-numeric ids. Other non-numeric entries are treated as missing, which are filled with $opt{MISSN} then set to BAD*. Can specify num of data rows to read from top but not arbitrary range.

*If passed handle, it will not be closed here.

*PDL::Bad::setvaltobad only works consistently with the default TYPE double before PDL-2.4.4_04.

=for options

Default options (case insensitive):

    V       => 1,        # prints simple status
    TYPE    => double,
    C_ID    => 1,
    R_ID    => 1,
    R_VAR   => 0,        # set to 1 if var in rows
    SEP     => "\t",     # can take regex qr//
    MISSN   => -999,
    NROW    => '',

=for usage

Usage:

    ($data, $idv, $ido) = get_data( \*STDIN, { TYPE=>long } );

    $data = get_data( 'zcat big_data.txt.gz |' );

=cut

sub get_data {
    # returns obs x var data matrix and var and obs ids
  my ($src, $opt) = @_;

  my $fh_in;
  if ($src =~ /STDIN/ or ref $src eq 'GLOB') { $fh_in = $src }
  else                                       { open $fh_in, $src or croak "$!" }

  my %opt = ( V       => 1,
              TYPE    => double,
              C_ID    => 1,
              R_ID    => 1,
              R_VAR   => 0,
              SEP     => "\t",
              MISSN   => -999,
              NROW    => '',
            );
  $opt and $opt{uc $_} = $opt->{$_} for (keys %$opt);
  $opt{V} and print STDERR "reading $src for data and id... ";
  
  local $PDL::undefval = $opt{MISSN};

  my $id_c = [];     # match declaration of $id_r for return purpose
  $opt{C_ID} and do {
    chomp( $id_c = <$fh_in> );
    my @entries = split $opt{SEP}, $id_c;
    $opt{R_ID} and shift @entries;
    $id_c = \@entries;
  };

  my ($c_row, $id_r, $data, @data) = (0, [], PDL->null);
  while (<$fh_in>) {
    chomp;
    my @entries = split /$opt{SEP}/, $_, -1;

    $opt{R_ID} and push @$id_r, shift @entries;
   
    for (@entries) { $_ = $opt{MISSN} unless defined $_ and /\d\b/ }

    push @data, pdl( $opt{TYPE}, \@entries );
    $c_row ++;
    last
      if $opt{NROW} and $c_row == $opt{NROW};
  }
  # not closing $fh_in here in case it's passed from outside. letting it close by going out of scope if opened here. 

  $data = pdl $opt{TYPE}, @data;
  @data = ();
    # rid of last col unless there is data there
  $data = $data(0:$data->getdim(0)-2, )->sever
    unless ( nelem $data(-1, )->where($data(-1, ) != $opt{MISSN}) ); 

  my ($idv, $ido) = ($id_r, $id_c);
    # var in columns instead of rows
  $opt{R_VAR} == 0
    and ($data, $idv, $ido) = ($data->inplace->transpose, $id_c, $id_r);

  if ($opt{V}) {
    print STDERR "OK.\ndata as PDL dim v x o: " . $data->info . "\n";
    $idv and print STDERR "$_\t$$idv[$_]\n" for (0..$#$idv);
  }
 
  $data = $data->setvaltobad( $opt{MISSN} );
  $data->check_badflag;
  return wantarray? (@$idv? ($data, $idv, $ido) : ($data, $ido)) : $data;
}

=head2 which_id

=for ref

Lookup specified var (obs) id in $idv ($ido) (see B<get_data>) and return index in $idv as pdl if found. Useful for selecting data by var (obs) id.

=for usage

    my $ind = which_id $ido, ['2c_1', 'vq_1'];

    my $data_subset = $data( $ind, );

=cut

sub which_id {
  my ($id, $id_s) = @_;

  my %ind;
  @ind{ @$id } = ( 0 .. $#$id );

  my @ind_select;
  for (@$id_s) {
    defined( $ind{$_} ) and push @ind_select, $ind{$_};
  }
  return pdl @ind_select;
}

=head1 SEE ALSO

L<PDL::Basic> (hist for frequency counts)

L<PDL::Ufunc> (sum, avg, median, min, max, etc.)

L<PDL::GSL::CDF> (various cumulative distribution functions)

=head1 	REFERENCES

Hays, W.L. (1994). Statistics (5th ed.). Fort Worth, TX: Harcourt Brace College Publishers.

=head1 AUTHOR

Copyright (C) 2009 Maggie J. Xiong <maggiexyz users.sourceforge.net>

All rights reserved. There is no warranty. You are allowed to redistribute this software / documentation as described in the file COPYING in the PDL distribution.

=cut

EOD

pp_done();
