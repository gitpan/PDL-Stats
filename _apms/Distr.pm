
#
# GENERATED WITH PDL::PP! Don't modify!
#
package PDL::Stats::Distr;

@EXPORT_OK  = qw(  PDL::PP mme_beta PDL::PP pdf_beta PDL::PP mme_binomial PDL::PP pmf_binomial PDL::PP mle_exp PDL::PP pdf_exp PDL::PP mme_gamma PDL::PP pdf_gamma PDL::PP mle_gaussian PDL::PP pdf_gaussian PDL::PP mle_geo PDL::PP pmf_geo PDL::PP mle_geosh PDL::PP pmf_geosh PDL::PP mle_lognormal PDL::PP mme_lognormal PDL::PP pdf_lognormal PDL::PP mme_nbd PDL::PP pmf_nbd PDL::PP mme_pareto PDL::PP pdf_pareto PDL::PP mle_poisson PDL::PP pmf_poisson );
%EXPORT_TAGS = (Func=>[@EXPORT_OK]);

use PDL::Core;
use PDL::Exporter;
use DynaLoader;



   
   @ISA    = ( 'PDL::Exporter','DynaLoader' );
   push @PDL::Core::PP, __PACKAGE__;
   bootstrap PDL::Stats::Distr ;





use strict;
use warnings;

use Carp;
use PDL::LiteF;

my $PGPLOT;
if ( grep { -e "$_/PGPLOT.pm"  } @INC ) {
  require PDL::Graphics::PGPLOT::Window;
  PDL::Graphics::PGPLOT::Window->import( 'pgwin' );
  $PGPLOT = 1;
}

=head1 NAME

PDL::Stats::Distr -- parameter estimations and probability density functions for distributions.

=head1 DESCRIPTION

Parameter estimate is maximum likelihood estimate when there is closed form estimate, otherwise it is method of moments estimate.

=head1 SYNOPSIS

    use PDL::LiteF;
    use PDL::Stats::Distr;

      # do a frequency (probability) plot with fitted normal curve

    my ($xvals, $hist) = $data->hist;

      # turn frequency into probability
    $hist /= $data->nelem;

      # get maximum likelihood estimates of normal curve parameters
    my ($m, $v) = $data->mle_gaussian();

      # fitted normal curve probabilities
    my $p = $xvals->pdf_gaussian($m, $v);

    use PDL::Graphics::PGPLOT::Window;
    my $win = pgwin( Dev=>"/xs" );

    $win->bin( $hist );
    $win->hold;
    $win->line( $p, {COLOR=>2} );
    $win->close;

Or, play with different distributions with B<plot_distr> :)

    $data->plot_distr( 'gaussian', 'lognormal' );

=cut







=head1 FUNCTIONS



=cut






=head2 mme_beta

=for sig

  Signature: (a(n); float+ [o]alpha(); float+ [o]beta())



=for usage

    my ($a, $b) = $data->mme_beta();

=for ref

beta distribution. pdf: f(x; a,b) = 1/B(a,b) x^(a-1) (1-x)^(b-1)

  

=for bad

mme_beta does handle bad values.
It will set the bad-value flag of all output piddles if the flag is set for any of the input piddles.


=cut






*mme_beta = \&PDL::mme_beta;




=head2 pdf_beta

=for sig

  Signature: (x(); a(); b(); float+ [o]p())



=for ref

probability density function for beta distribution. x defined on [0,1].

  

=for bad

pdf_beta does handle bad values.
It will set the bad-value flag of all output piddles if the flag is set for any of the input piddles.


=cut






*pdf_beta = \&PDL::pdf_beta;




=head2 mme_binomial

=for sig

  Signature: (a(n); int [o]n_(); float+ [o]p())



=for usage

    my ($n, $p) = $data->mme_binomial;

=for ref

binomial distribution. pmf: f(k; n,p) = (n k) p^k (1-p)^(n-k) for k = 0,1,2..n 

  

=for bad

mme_binomial does handle bad values.
It will set the bad-value flag of all output piddles if the flag is set for any of the input piddles.


=cut






*mme_binomial = \&PDL::mme_binomial;




=head2 pmf_binomial

=for sig

  Signature: (ushort x(); ushort n(); p(); float+ [o]out())



=for ref

probability mass function for binomial distribution.

  

=for bad

pmf_binomial does handle bad values.
It will set the bad-value flag of all output piddles if the flag is set for any of the input piddles.


=cut






*pmf_binomial = \&PDL::pmf_binomial;




=head2 mle_exp

=for sig

  Signature: (a(n); float+ [o]l())



=for usage

    my $lamda = $data->mle_exp;

=for ref

exponential distribution. mle same as method of moments estimate.

  

=for bad

mle_exp does handle bad values.
It will set the bad-value flag of all output piddles if the flag is set for any of the input piddles.


=cut






*mle_exp = \&PDL::mle_exp;




=head2 pdf_exp

=for sig

  Signature: (x(); l(); float+ [o]p())



=for ref

probability density function for exponential distribution.

  

=for bad

pdf_exp does handle bad values.
It will set the bad-value flag of all output piddles if the flag is set for any of the input piddles.


=cut






*pdf_exp = \&PDL::pdf_exp;




=head2 mme_gamma

=for sig

  Signature: (a(n); float+ [o]shape(); float+ [o]scale())



=for usage

    my ($shape, $scale) = $data->mme_gamma();

=for ref

two-parameter gamma distribution

  

=for bad

mme_gamma does handle bad values.
It will set the bad-value flag of all output piddles if the flag is set for any of the input piddles.


=cut






*mme_gamma = \&PDL::mme_gamma;




=head2 pdf_gamma

=for sig

  Signature: (x(); a(); t(); float+ [o]p())



=for ref

probability density function for two-parameter gamma distribution.

  

=for bad

pdf_gamma does handle bad values.
It will set the bad-value flag of all output piddles if the flag is set for any of the input piddles.


=cut






*pdf_gamma = \&PDL::pdf_gamma;




=head2 mle_gaussian

=for sig

  Signature: (a(n); float+ [o]m(); float+ [o]v())



=for usage

    my ($m, $v) = $data->mle_gaussian();

=for ref

gaussian aka normal distribution. same results as $data->average and $data->var. mle same as method of moments estimate.

  

=for bad

mle_gaussian does handle bad values.
It will set the bad-value flag of all output piddles if the flag is set for any of the input piddles.


=cut






*mle_gaussian = \&PDL::mle_gaussian;




=head2 pdf_gaussian

=for sig

  Signature: (x(); m(); v(); float+ [o]p())



=for ref

probability density function for gaussian distribution.

  

=for bad

pdf_gaussian does handle bad values.
It will set the bad-value flag of all output piddles if the flag is set for any of the input piddles.


=cut






*pdf_gaussian = \&PDL::pdf_gaussian;




=head2 mle_geo

=for sig

  Signature: (a(n); float+ [o]p())



=for ref

geometric distribution. mle same as method of moments estimate.

  

=for bad

mle_geo does handle bad values.
It will set the bad-value flag of all output piddles if the flag is set for any of the input piddles.


=cut






*mle_geo = \&PDL::mle_geo;




=head2 pmf_geo

=for sig

  Signature: (ushort x(); p(); float+ [o]out())



=for ref

probability mass function for geometric distribution. x >= 0.

  

=for bad

pmf_geo does handle bad values.
It will set the bad-value flag of all output piddles if the flag is set for any of the input piddles.


=cut






*pmf_geo = \&PDL::pmf_geo;




=head2 mle_geosh

=for sig

  Signature: (a(n); float+ [o]p())



=for ref

shifted geometric distribution. mle same as method of moments estimate.

  

=for bad

mle_geosh does handle bad values.
It will set the bad-value flag of all output piddles if the flag is set for any of the input piddles.


=cut






*mle_geosh = \&PDL::mle_geosh;




=head2 pmf_geosh

=for sig

  Signature: (ushort x(); p(); float+ [o]out())



=for ref

probability mass function for shifted geometric distribution. x >= 1.

  

=for bad

pmf_geosh does handle bad values.
It will set the bad-value flag of all output piddles if the flag is set for any of the input piddles.


=cut






*pmf_geosh = \&PDL::pmf_geosh;




=head2 mle_lognormal

=for sig

  Signature: (a(n); float+ [o]m(); float+ [o]v())



=for usage

    my ($m, $v) = $data->mle_lognormal();

=for ref

lognormal distribution. maximum likelihood estimation.

  

=for bad

mle_lognormal does handle bad values.
It will set the bad-value flag of all output piddles if the flag is set for any of the input piddles.


=cut






*mle_lognormal = \&PDL::mle_lognormal;




=head2 mme_lognormal

=for sig

  Signature: (a(n); float+ [o]m(); float+ [o]v())



=for usage

    my ($m, $v) = $data->mme_lognormal();

=for ref

lognormal distribution. method of moments estimation.

  

=for bad

mme_lognormal does handle bad values.
It will set the bad-value flag of all output piddles if the flag is set for any of the input piddles.


=cut






*mme_lognormal = \&PDL::mme_lognormal;




=head2 pdf_lognormal

=for sig

  Signature: (x(); m(); v(); float+ [o]p())



=for ref

probability density function for lognormal distribution. x > 0. v > 0.

  

=for bad

pdf_lognormal does handle bad values.
It will set the bad-value flag of all output piddles if the flag is set for any of the input piddles.


=cut






*pdf_lognormal = \&PDL::pdf_lognormal;




=head2 mme_nbd

=for sig

  Signature: (a(n); float+ [o]r(); float+ [o]p())



=for usage

    my ($r, $p) = $data->mme_nbd();

=for ref

negative binomial distribution. pmf: f(x; r,p) = (x+r-1  r-1) p^r (1-p)^x for x=0,1,2...

  

=for bad

mme_nbd does handle bad values.
It will set the bad-value flag of all output piddles if the flag is set for any of the input piddles.


=cut






*mme_nbd = \&PDL::mme_nbd;




=head2 pmf_nbd

=for sig

  Signature: (ushort x(); r(); p(); float+ [o]out())



=for ref

probability mass function for negative binomial distribution.

  

=for bad

pmf_nbd does handle bad values.
It will set the bad-value flag of all output piddles if the flag is set for any of the input piddles.


=cut






*pmf_nbd = \&PDL::pmf_nbd;




=head2 mme_pareto

=for sig

  Signature: (a(n); float+ [o]k(); float+ [o]xm())



=for usage

    my ($k, $xm) = $data->mme_pareto();

=for ref

pareto distribution. pdf: f(x; k,xm) = k xm^k / x^(k+1) for x >= xm > 0.

  

=for bad

mme_pareto does handle bad values.
It will set the bad-value flag of all output piddles if the flag is set for any of the input piddles.


=cut






*mme_pareto = \&PDL::mme_pareto;




=head2 pdf_pareto

=for sig

  Signature: (x(); k(); xm(); float+ [o]p())



=for ref

probability density function for pareto distribution. x >= xm > 0.

  

=for bad

pdf_pareto does handle bad values.
It will set the bad-value flag of all output piddles if the flag is set for any of the input piddles.


=cut






*pdf_pareto = \&PDL::pdf_pareto;




=head2 mle_poisson

=for sig

  Signature: (a(n); float+ [o]l())



=for usage

    my $lamda = $data->mle_poisson();

=for ref

poisson distribution. pmf: f(x;l) = e^(-l) * l^x / x!

  

=for bad

mle_poisson does handle bad values.
It will set the bad-value flag of all output piddles if the flag is set for any of the input piddles.


=cut






*mle_poisson = \&PDL::mle_poisson;




=head2 pmf_poisson

=for sig

  Signature: (ushort x(); l(); float+ [o]p())



=for ref

probability mass function for poisson distribution.

  

=for bad

pmf_poisson does handle bad values.
It will set the bad-value flag of all output piddles if the flag is set for any of the input piddles.


=cut






*pmf_poisson = \&PDL::pmf_poisson;


;


=head2 plot_distr

=for ref

Plots data distribution. When given specific distribution(s) to fit, returns % ref to sum log likelihood and parameter values under fitted distribution(s). See FUNCTIONS above for available distributions. 

=for options

Default options (case insensitive):

    MAXBN => 20, 
      # see PDL::Graphics::PGPLOT::Window for next options
    WIN   => undef,   # pgwin object. not closed here if passed
                      # allows comparing multiple distr in same plot
                      # set env before passing WIN
    DEV   => '/xs',   # open and close dev for plotting if no WIN
    COLOR => 1,       # color for data distr

=for usage

Usage:

      # yes it threads :)
    my $data = grandom( 500, 3 )->abs;
      # ll on plot is sum across 3 data curves
    my ($ll, $pars)
      = $data->plot_distr( 'gaussian', 'lognormal', {DEV=>'/png'} );

    print "$_\t$ll->{$_}\n" for (sort keys %$ll);
    print "$_\t@{$pars->{$_}}\n" for (sort keys %$pars);

=cut

*plot_distr = \&PDL::plot_distr;
sub PDL::plot_distr {
  if (!$PGPLOT) {
    carp "No PDL::Graphics::PGPLOT, no plot :(";
    return;
  }
  my ($self, @distr) = @_;

  my %opt = (
    MAXBN => 20, 
    WIN   => undef,     # pgwin object. not closed here if passed
    DEV   => '/xs',     # open and close default win if no WIN
    COLOR => 1,         # color for data distr
  );
  my $opt = pop @distr
    if ref $distr[-1] eq 'HASH';
  $opt and $opt{uc $_} = $opt->{$_} for (keys %$opt);

  $self = $self->squeeze;

    # use int range, step etc for int xvals--pmf compatible
  my $INT = 1
    if grep { /(?:binomial)|(?:geo)|(?:nbd)|(?:poisson)/ } @distr;

  my ($range, $step, $step_int);
  $range = $self->max - $self->min;
  $step  = $range / $opt{MAXBN};
  $step_int = ($range <= $opt{MAXBN})? 1 
            :                          PDL::ceil( $range / $opt{MAXBN} )
            ;
  $opt{MAXBN} = PDL::ceil( $range / $step );

  my $hist = $self->double->histogram($step, $self->min, $opt{MAXBN});
    # turn fre into prob
  $hist /= $self->dim(0);

    # use min to make it pure scalar for sequence
  my $xvals = $self->min + sequence( $opt{MAXBN}->min ) * $step;
  my $xvals_int
    = PDL::ceil($self->min) + sequence( $opt{MAXBN}->min ) * $step_int;
  $xvals_int = $xvals_int->where( $xvals_int <= $xvals->max )->sever;

  my $win = $opt{WIN};
  if (!$win) {
    $win = pgwin( Dev=>$opt{DEV} );
    $win->env($xvals->minmax,0,1, {XTitle=>'bins', YTitle=>'probability'});
  }

  $win->line( $xvals, $hist, { COLOR=>$opt{COLOR} } );

  if (!@distr) {
    $win->close
      unless defined $opt{WIN};
    return;
  }

  my (%ll, %pars, @text, $c);
  $c = $opt{COLOR};        # fitted lines start from ++$c
  for my $distr ( @distr ) {
      # find mle_ or mme_$distr;
    my @funcs = grep { /_$distr$/ } (keys %PDL::Stats::Distr::);
    if (!@funcs) {
      carp "Do not recognize $distr distribution!";
      next;
    }
      # might have mle and mme for a distr. sort so mle comes first 
    @funcs = sort @funcs;
    my ($f_para, $f_prob) = @funcs[0, -1];

    eval {
      my @paras = $self->$f_para();
      $pars{$distr} = \@paras;
  
      @paras = map { $_->dummy(0) } @paras;
      $ll{$distr} = $self->$f_prob( @paras )->log->sumover;
      push @text, sprintf "$distr  LL = %.2f", $ll{$distr}->sum;

      if ($f_prob =~ /^pdf/) { 
        $win->line( $xvals, $xvals->$f_prob(@paras), {COLOR=>++$c} );
      }
      else {
        $win->points( $xvals_int, $xvals_int->$f_prob(@paras), {COLOR=>++$c} );
      }
    };
    carp $@ if $@;
  }
  $win->legend(\@text, ($xvals->min + $xvals->max)/2, .95,
               {COLOR=>[$opt{COLOR}+1 .. $c], TextFraction=>.75} );
  $win->close
    unless defined $opt{WIN};
  return (\%ll, \%pars);
}

=head1 DEPENDENCIES

GSL - GNU Scientific Library

=head1 SEE ALSO

PDL::Graphics::PGPLOT

PDL::GSL::CDF

=head1 AUTHOR

Copyright (C) 2009 Maggie J. Xiong <maggiexyz users.sourceforge.net>

All rights reserved. There is no warranty. You are allowed to redistribute this software / documentation as described in the file COPYING in the PDL distribution.

=cut





# Exit with OK status

1;

		   