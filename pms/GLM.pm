
#
# GENERATED WITH PDL::PP! Don't modify!
#
package PDL::Stats::GLM;

@EXPORT_OK  = qw(  ols_t anova anova_rptd dummy_code effect_code effect_code_w ols r2_change logistic pca pca_sorti plot_means plot_scree PDL::PP fill_m PDL::PP fill_rand PDL::PP dev_m PDL::PP stddz PDL::PP sse PDL::PP mse PDL::PP rmse PDL::PP pred_logistic PDL::PP d0 PDL::PP dm PDL::PP dvrs );
%EXPORT_TAGS = (Func=>[@EXPORT_OK]);

use PDL::Core;
use PDL::Exporter;
use DynaLoader;



   
   @ISA    = ( 'PDL::Exporter','DynaLoader' );
   push @PDL::Core::PP, __PACKAGE__;
   bootstrap PDL::Stats::GLM ;





use strict;
use warnings;

use Carp;
use PDL::LiteF;
use PDL::MatrixOps;
use PDL::NiceSlice;
use PDL::Stats::Basic;
use PDL::Stats::Kmeans;

my $CDF;
if ( grep { -e "$_/PDL/GSL/CDF.pm"  } @INC ) {
  require PDL::GSL::CDF;
  $CDF = 1;
}

my $SLATEC;
if ( grep { -e "$_/PDL/Slatec.pm"  } @INC ) {
  require PDL::Slatec;
  $SLATEC = 1;
}

my $PGPLOT;
if ( grep { -e "$_/PGPLOT.pm"  } @INC ) {
  require PDL::Graphics::PGPLOT::Window;
  PDL::Graphics::PGPLOT::Window->import( 'pgwin' );
  $PGPLOT = 1;
}

=head1 NAME

PDL::Stats::GLM -- general linear modeling methods and logistic regression

=head1 DESCRIPTION

The terms FUNCTIONS and METHODS are arbitrarily used to refer to methods that are threadable and methods that are NOT threadable, respectively. FUNCTIONS except B<ols_t> support bad value. B<PDL::Slatec> strongly recommended for most METHODS, and it is required for B<logistic>.

P-values, where appropriate, are provided if PDL::GSL::CDF is installed.

=head1 SYNOPSIS

    use PDL::LiteF;
    use PDL::NiceSlice;
    use PDL::Stats::GLM;

    # do a multiple linear regression and plot the residuals

    my $y  = random 10;

    my $x1 = sequence 10;
    my $x2 = $x1 ** 2;
    my $iv = cat $x1, $x2;

    my %m  = $y->ols( $iv );
    print "$_\t$m{$_}\n" for (sort keys %m);

    use PDL::Graphics::PGPLOT::Window;

    my $win = pgwin( 'xs' );
    $win->points( $y - $m{y_pred} );

=cut







=head1 FUNCTIONS



=cut






=head2 fill_m

=for sig

  Signature: (a(n); float+ [o]b(n))



=for ref

Replaces bad values with sample mean. Can be done inplace.

=for usage

     perldl> p $data
     [
      [  5 BAD   2 BAD]
      [  7   3   7 BAD]
     ]

     perldl> p $data->fill_m
     [
      [      5     3.5       2     3.5]
      [      7       3       7 5.66667]
     ] 

  

=for bad


The output pdl badflag is cleared. 
  

=cut






*fill_m = \&PDL::fill_m;




=head2 fill_rand

=for sig

  Signature: (a(n); [o]b(n))



=for ref

Replaces bad values with random sample (with replacement) of good observations from the same variable. Can be done inplace.

=for usage

    perldl> p $data
    [
     [  5 BAD   2 BAD]
     [  7   3   7 BAD]
    ]
    
    perldl> p $data->fill_rand
    
    [
     [5 2 2 5]
     [7 3 7 7]
    ]

  

=for bad


The output pdl badflag is cleared. 
  

=cut






*fill_rand = \&PDL::fill_rand;




=head2 dev_m

=for sig

  Signature: (a(n); float+ [o]b(n))



=for ref

Replaces values with deviations from the mean. Can be done inplace.

  

=for bad

dev_m does handle bad values.
It will set the bad-value flag of all output piddles if the flag is set for any of the input piddles.


=cut






*dev_m = \&PDL::dev_m;




=head2 stddz

=for sig

  Signature: (a(n); float+ [o]b(n))


=for ref

Standardize ie replace values with z_scores based on sample standard deviation from the mean (replace with 0s if stdv==0). Can be done inplace.

  

=for bad

stddz does handle bad values.
It will set the bad-value flag of all output piddles if the flag is set for any of the input piddles.


=cut






*stddz = \&PDL::stddz;




=head2 sse

=for sig

  Signature: (a(n); b(n); float+ [o]c())



=for ref

Sum of squared errors between actual and predicted values.

  

=for bad

sse does handle bad values.
It will set the bad-value flag of all output piddles if the flag is set for any of the input piddles.


=cut






*sse = \&PDL::sse;




=head2 mse

=for sig

  Signature: (a(n); b(n); float+ [o]c())



=for ref

Mean of squared errors between actual and predicted values, ie variance around predicted value.

  

=for bad

mse does handle bad values.
It will set the bad-value flag of all output piddles if the flag is set for any of the input piddles.


=cut






*mse = \&PDL::mse;




=head2 rmse

=for sig

  Signature: (a(n); b(n); float+ [o]c())



=for ref

Root mean squared error, ie stdv around predicted value.

  

=for bad

rmse does handle bad values.
It will set the bad-value flag of all output piddles if the flag is set for any of the input piddles.


=cut






*rmse = \&PDL::rmse;




=head2 pred_logistic

=for sig

  Signature: (a(n,m); b(m); float+ [o]c(n))



=for ref

Calculates predicted prob value for logistic regression.

=for usage

    # glue constant then apply coeff returned by the logistic method

    $pred = $x->glue(1,ones($x->dim(0)))->pred_logistic( $m{b} );

  

=for bad

pred_logistic does handle bad values.
It will set the bad-value flag of all output piddles if the flag is set for any of the input piddles.


=cut






*pred_logistic = \&PDL::pred_logistic;




=head2 d0

=for sig

  Signature: (a(n); float+ [o]c())


=for usage

    my $d0 = $y->d0();

=for ref

Null deviance for logistic regression.

  

=for bad

d0 does handle bad values.
It will set the bad-value flag of all output piddles if the flag is set for any of the input piddles.


=cut






*d0 = \&PDL::d0;




=head2 dm

=for sig

  Signature: (a(n); b(n); float+ [o]c())


=for usage

    my $dm = $y->dm( $y_pred );

      # null deviance
    my $d0 = $y->dm( ones($y->nelem) * $y->avg );

=for ref

Model deviance for logistic regression.

  

=for bad

dm does handle bad values.
It will set the bad-value flag of all output piddles if the flag is set for any of the input piddles.


=cut






*dm = \&PDL::dm;




=head2 dvrs

=for sig

  Signature: (a(); b(); float+ [o]c())



=for ref

Deviance residual for logistic regression.

  

=for bad

dvrs does handle bad values.
It will set the bad-value flag of all output piddles if the flag is set for any of the input piddles.


=cut






*dvrs = \&PDL::dvrs;



=head2 ols_t

=for ref

Threaded version of ordinary least squares regression. The price of threading was losing significance tests for coefficients (but see B<r2_change>). The fitting function was shamelessly copied then modified from PDL::Fit::Linfit. Uses PDL::Slatec when possible but otherwise uses PDL::MatrixOps. Intercept is LAST of coeff if CONST => 1.

ols_t does not handle bad values. consider B<fill_m> or B<fill_rand> if there are bad values. Default options (case insensitive):

=for options

    CONST   => 1,

=for usage

Usage:

    # DV, 2 person's ratings for top-10 box office movies
    # ascending sorted by box office numbers

    perldl> p $y = qsort ceil( random(10, 2)*5 )    
    [
     [1 1 2 4 4 4 4 5 5 5]
     [1 2 2 2 3 3 3 3 5 5]
    ]

    # model with 2 IVs, a linear and a quadratic trend component

    perldl> $x = cat sequence(10), sequence(10)**2

    # suppose our novice modeler thinks this creates 3 different models
    # for predicting movie ratings

    perldl> p $x = cat $x, $x * 2, $x * 3
    [
     [
      [ 0  1  2  3  4  5  6  7  8  9]
      [ 0  1  4  9 16 25 36 49 64 81]
     ]
     [
      [  0   2   4   6   8  10  12  14  16  18]
      [  0   2   8  18  32  50  72  98 128 162]
     ]
     [
      [  0   3   6   9  12  15  18  21  24  27]
      [  0   3  12  27  48  75 108 147 192 243]
     ]
    ]

    perldl> p $x->info
    PDL: Double D [10,2,3]

    # insert a dummy dim between IV and the dim (model) to be threaded

    perldl> %m = $y->ols_t( $x->dummy(2) )

    perldl> p "$_\t$m{$_}\n" for (sort keys %m)

    # 2 persons' ratings, eached fitted with 3 "different" models

    F
    [
     [ 38.314159  25.087209]
     [ 38.314159  25.087209]
     [ 38.314159  25.087209]
    ]

    # df is the same across dv and iv models
 
    F_df    [2 7]
    F_p
    [
     [0.00016967051 0.00064215074]
     [0.00016967051 0.00064215074]
     [0.00016967051 0.00064215074]
    ]
    
    R2
    [
     [ 0.9162963 0.87756762]
     [ 0.9162963 0.87756762]
     [ 0.9162963 0.87756762]
    ]

    b
    [  # linear      quadratic     constant
     [
      [  0.99015152 -0.056818182   0.66363636]    # person 1
      [  0.18939394  0.022727273          1.4]    # person 2
     ]
     [
      [  0.49507576 -0.028409091   0.66363636]
      [  0.09469697  0.011363636          1.4]
     ]
     [
      [  0.33005051 -0.018939394   0.66363636]
      [ 0.063131313 0.0075757576          1.4]
     ]
    ]

    # our novice modeler realizes at this point that
    # the 3 models only differ in the scaling of the IV coefficients 
    
    ss_model
    [
     [ 20.616667  13.075758]
     [ 20.616667  13.075758]
     [ 20.616667  13.075758]
    ]
    
    ss_residual
    [
     [ 1.8833333  1.8242424]
     [ 1.8833333  1.8242424]
     [ 1.8833333  1.8242424]
    ]
    
    ss_total        [22.5 14.9]
    y_pred
    [
     [
      [0.66363636  1.5969697  2.4166667  3.1227273  3.7151515  4.1939394  4.5590909  4.8106061  4.9484848  4.9727273]
    ...

=cut

*ols_t = \&PDL::ols_t;
sub PDL::ols_t {
    # y [n], ivs [n x attr] pdl
  my ($y, $ivs, $opt) = @_;
  my %opt = ( CONST => 1 );
  $opt and $opt{uc $_} = $opt->{$_} for (keys %$opt);

  $y = $y->squeeze;
  $ivs = $ivs->dummy(1) if $ivs->getndims == 1;
    # set up ivs and const as ivs
  $opt{CONST} and
    $ivs = $ivs->glue( 1, ones($ivs->dim(0)) );

  # Internally normalise data
  # (double) it or ushort y and sequence iv won't work right
  my $ymean = $y->abs->sumover->double / $y->dim(0);
  $ymean->where( $ymean==0 ) .= 1;
  my $y2 = $y / $ymean->dummy(0);
 
  # Do the fit
     
  my $Y = $ivs x $y2->dummy(0);

  my $C;
    # somehow PDL::Slatec gives weird numbers when CONST=>0
  if ( $SLATEC ) {
#  if ( $opt{CONST} and $SLATEC ) {
    $C = PDL::Slatec::matinv( $ivs x $ivs->xchg(0,1) );
  }
  else {
    $C = inv( $ivs x $ivs->xchg(0,1) );
  }

    # Fitted coefficients vector
  my $coeff = PDL::squeeze( $C x $Y );

  $coeff = $coeff->dummy(0)
    if $coeff->getndims == 1 and $y->getndims > 1;
  $coeff *= $ymean->dummy(0);        # Un-normalise

  return $coeff
    unless wantarray; 

  my %ret;

    # ***$coeff x $ivs looks nice but produces nan on successive tries***
  $ret{y_pred} = sumover( $coeff->dummy(1) * $ivs->xchg(0,1) );
  $ret{ss_total} = $opt{CONST}? $y->ss : sumover( $y ** 2 );
  $ret{ss_residual} = $y->sse( $ret{y_pred} );
  $ret{ss_model} = $ret{ss_total} - $ret{ss_residual};
  $ret{R2} = $ret{ss_model} / $ret{ss_total};

  my $n_var = $opt{CONST}? $ivs->dim(1) - 1 : $ivs->dim(1);
  $ret{F_df} = pdl( $n_var, $y->dim(0) - $ivs->dim(1) );
  $ret{F}
    = $ret{ss_model} / $ret{F_df}->(0) / ($ret{ss_residual} / $ret{F_df}->(1));
  $ret{F_p} = 1 - $ret{F}->gsl_cdf_fdist_P( $ret{F_df}->dog )
    if $CDF;

  for (keys %ret) { ref $ret{$_} eq 'PDL' and $ret{$_} = $ret{$_}->squeeze };

  $ret{b} = $coeff;

  return %ret;
}

=head2 r2_change

=for ref

Significance test for the incremental change in R2 when new variable(s) are added to an ols regression model. Returns the change stats as well as stats for both models. Base on B<ols_t>. (One way to make up for the lack of significance tests for coeffs in ols_t).

=for options

Default options (case insensitive): 

    CONST   => 1,

=for usage

Usage:

    # suppose these are two persons' ratings for top 10 box office movies
    # ascending sorted by box office

    perldl> p $y = qsort ceil(random(10, 2) * 5)
    [
     [1 1 2 2 2 3 4 4 4 4]
     [1 2 2 3 3 3 4 4 5 5]
    ]

    # first IV is a simple linear trend

    perldl> p $x1 = sequence 10
    [0 1 2 3 4 5 6 7 8 9]

    # the modeler wonders if adding a quadratic trend improves the fit

    perldl> p $x2 = sequence(10) ** 2
    [0 1 4 9 16 25 36 49 64 81]

    # two difference models are given in two pdls
    # each as would be pass on to ols_t
    # the 1st model includes only linear trend
    # the 2nd model includes linear and quadratic trends
    # when necessary use dummy dim so both models have the same ndims

    perldl> %c = $y->r2_change( $x1->dummy(1), cat($x1, $x2) )

    perldl> p "$_\t$c{$_}\n" for (sort keys %c)
      #              person 1   person 2
    F_change        [0.72164948 0.071283096]
      # df same for both persons
    F_df    [1 7]
    F_p     [0.42370145 0.79717232]
    R2_change       [0.0085966043 0.00048562549]
    model0  HASH(0x8c10828)
    model1  HASH(0x8c135c8)
   
    # the answer here is no.

=cut

*r2_change = \&PDL::r2_change;
sub PDL::r2_change {
  my ($self, $ivs0, $ivs1, $opt) = @_;
  $ivs0->getndims == 1 and $ivs0 = $ivs0->dummy(1);

  my %ret;

  $ret{model0} = { $self->ols_t( $ivs0, $opt ) };
  $ret{model1} = { $self->ols_t( $ivs1, $opt ) };

  $ret{R2_change} = $ret{model1}->{R2} - $ret{model0}->{R2};
  $ret{F_df}
    = pdl($ivs1->dim(1) - $ivs0->dim(1),
          $ret{model1}->{F_df}->((1)) );
  $ret{F_change}
    = $ret{R2_change} * $ret{F_df}->((1))
    / ( (1-$ret{model1}->{R2}) * $ret{F_df}->((0)) );
  $ret{F_p} = 1 - $ret{F_change}->gsl_cdf_fdist_P( $ret{F_df}->dog )
    if $CDF;

  for (keys %ret) { ref $ret{$_} eq 'PDL' and $ret{$_} = $ret{$_}->squeeze };

  return %ret;
}

=head1 METHODS

=head2 anova

=for ref

Analysis of variance. Uses type III sum of squares for unbalanced data.

anova supports bad value in the dependent variable.

=for options

Default options (case insensitive):

    IVNM   => undef,   # auto filled as ['IV_0', 'IV_1', ... ]
    PLOT   => 1,       # plots highest order effect
    V      => 1,       # carps if bad value in dv

=for usage

Usage:

    # suppose this is ratings for 12 apples

    perldl> p $y = qsort ceil( random(12)*5 )
    [1 1 2 2 2 3 3 4 4 4 5 5]
    
    # IV for types of apple

    perldl> p $a = sequence(12) % 3 + 1
    [1 2 3 1 2 3 1 2 3 1 2 3]

    # IV for whether we baked the apple
    
    perldl> @b = qw( y y y y y y n n n n n n )

    perldl> %m = $y->anova( $a, \@b, { IVNM=>['apple', 'bake'] } )
    
    perldl> p "$_\t$m{$_}\n" for (sort keys %m)
    # apple # m
    [
     [2.5   3 3.5]
    ]
    
    # apple # se
    [
     [0.64549722 0.91287093 0.64549722]
    ]
    
    # apple ~ bake # m
    [
     [1.5 1.5 2.5]
     [3.5 4.5 4.5]
    ]
    
    # apple ~ bake # se
    [
     [0.5 0.5 0.5]
     [0.5 0.5 0.5]
    ]
    
    # bake # m
    [
     [ 1.8333333  4.1666667]
    ]
    
    # bake # se
    [
     [0.30731815 0.30731815]
    ]
    
    F       7.6
    F_df    [5 6]
    F_p     0.0141586545851857
    ms_model        3.8
    ms_residual     0.5
    ss_model        19
    ss_residual     3
    ss_total        22
    | apple | F     2
    | apple | F_df  [2 6]
    | apple | F_p   0.216
    | apple | ms    1
    | apple | ss    2
    | apple ~ bake | F      0.666666666666667
    | apple ~ bake | F_df   [2 6]
    | apple ~ bake | F_p    0.54770848985725
    | apple ~ bake | ms     0.333333333333334
    | apple ~ bake | ss     0.666666666666667
    | bake | F      32.6666666666667
    | bake | F_df   [1 6]
    | bake | F_p    0.00124263849516693
    | bake | ms     16.3333333333333
    | bake | ss     16.3333333333333

=cut

*anova = \&PDL::anova;
sub PDL::anova {
  my $opt = pop @_
    if ref $_[-1] eq 'HASH';
  my ($self, @ivs_raw) = @_;
  croak "Mismatched number of elements in DV and IV. Are you passing IVs the old-and-abandoned way?"
    if (ref $ivs_raw[0] eq 'ARRAY') and (@{ $ivs_raw[0] } != $self->nelem);

  for (@ivs_raw) {
    croak "too many dims in IV!"
      if ref $_ eq 'PDL' and $_->squeeze->ndims > 1;
  }

  my %opt = (
    IVNM   => undef,   # auto filled as ['IV_0', 'IV_1', ... ]
    PLOT   => 1,       # plots highest order effect
    V      => 1,       # carps if bad value in dv
  );
  $opt and $opt{uc $_} = $opt->{$_} for (keys %$opt);
  $opt{IVNM} ||= [ map { "IV_$_" } (0 .. $#ivs_raw) ];
  my @idv = @{ $opt{IVNM} };

  my %ret;

  $self = $self->squeeze;
  my $igood = which $self->isgood;
  carp $igood->nelem . " good values in DV"
    if $igood->nelem < $self->nelem and $opt{V};
  $self = $self( $igood )->sever;
  $self->badflag(0);
    # create new vars here so we don't mess up original caller @
  my @pdl_ivs_raw
    = map { my $var
              = (ref $_ eq 'PDL')? [list $_($igood)] : [@$_[list $igood]];
            scalar PDL::Stats::Kmeans::_array_to_pdl $var;
          } @ivs_raw;

  my ($ivs_ref, $i_cmo_ref)
    = _effect_code_ivs( \@pdl_ivs_raw );

  ($ivs_ref, $i_cmo_ref, my( $idv, $ivs_cm_ref ))
    = _add_interactions( $ivs_ref, $i_cmo_ref, \@idv, \@pdl_ivs_raw );

    # add const here
  my $ivs = PDL->null->glue( 1, @$ivs_ref );
  $ivs = $ivs->glue(1, ones $ivs->dim(0));

  my $b_full = $self->ols_t( $ivs, {CONST=>0} );

  $ret{ss_total} = $self->ss;
  $ret{ss_residual} = $self->sse( sumover( $b_full * $ivs->xchg(0,1) ) );
  $ret{ss_model} = $ret{ss_total} - $ret{ss_residual};
  $ret{F_df} = pdl($ivs->dim(1) - 1, $self->nelem - ($ivs->dim(1) - 1) -1);
  $ret{ms_model} = $ret{ss_model} / $ret{F_df}->(0);
  $ret{ms_residual} = $ret{ss_residual} / $ret{F_df}->(1);
  $ret{F} = $ret{ms_model} / $ret{ms_residual};
  $ret{F_p} = 1 - $ret{F}->gsl_cdf_fdist_P( $ret{F_df}->dog )
    if $CDF;

  # get IV ss from $ivs_ref instead of $ivs pdl

  for my $k (0 .. $#$ivs_ref) {
    my (@G, $G, $b_G);
    @G = grep { $_ != $k } (0 .. $#$ivs_ref);
 
    if (@G) {
      $G = PDL->null->glue( 1, @$ivs_ref[@G] );
      $G = $G->glue(1, ones $G->dim(0));
    }
    else {
      $G = ones( $self->dim(0) );
    }
    $b_G = $self->ols_t( $G, {CONST=>0} );

    $ret{ "| $idv->[$k] | ss" }
      = $self->sse( sumover($b_G * $G->transpose) ) - $ret{ss_residual};
    $ret{ "| $idv->[$k] | F_df" }
      = pdl( $ivs_ref->[$k]->dim(1), $ret{F_df}->(1)->copy )->squeeze;
    $ret{ "| $idv->[$k] | ms" }
      = $ret{ "| $idv->[$k] | ss" } / $ret{ "| $idv->[$k] | F_df" }->(0);
    $ret{ "| $idv->[$k] | F" }
      = $ret{ "| $idv->[$k] | ms" } / $ret{ms_residual};
    $ret{ "| $idv->[$k] | F_p" }
      = 1 - $ret{ "| $idv->[$k] | F" }->gsl_cdf_fdist_P( $ret{ "| $idv->[$k] | F_df" }->dog )
      if $CDF;
  }

  for (keys %ret) { $ret{$_} = $ret{$_}->squeeze };

  my $cm_ref = _cell_means( $self, $ivs_cm_ref, $i_cmo_ref, $idv, \@pdl_ivs_raw );
    # sort bc we can't count on perl % internal key order implementation
  @ret{ sort keys %$cm_ref } = @$cm_ref{ sort keys %$cm_ref };

  my $highest = join(' ~ ', @{ $opt{IVNM} });
  $cm_ref->{"# $highest # m"}->plot_means(
                       {SE=>$cm_ref->{"# $highest # se"}, IVNM=>$idv, } )
    if $opt{PLOT};

  return %ret;
}

sub _old_interface_check {
  my ($n, $ivs_ref) = @_;
  return 1
    if (ref $ivs_ref->[0][0] eq 'ARRAY') and (@{ $ivs_ref->[0][0] } != $n);
}

sub _effect_code_ivs {
  my $ivs = shift;

  my (@i_iv, @i_cmo);
  for (@$ivs) {
    my ($e, $map) = effect_code($_->squeeze);
    my $var = ($e->getndims == 1)? $e->dummy(1) : $e;
    push @i_iv, $var;
    my @indices = sort { $a<=>$b } values %$map;
    push @i_cmo, pdl @indices;
  }
  return \@i_iv, \@i_cmo;
}

sub _add_interactions {
  my ($var_ref, $i_cmo_ref, $idv, $raw_ref) = @_;

    # append info re inter to main effects
  my (@inter, @idv_inter, @inter_cm, @inter_cmo);
  for my $nway ( 2 .. @$var_ref ) {
    my $iter_idv = _combinations( $nway, [0..$#$var_ref] );

    while ( my @v = &$iter_idv() ) {
      my $i = ones( $var_ref->[0]->dim(0), 1 );
      for (@v) {
        $i = $i * $var_ref->[$_]->dummy(1);
        $i = $i->clump(1,2);
      }
      push @inter, $i;

      my $e = join( ' ~ ', @$idv[@v] );
      push @idv_inter, $e;

        # now prepare for cell mean
      my @i_cm = ();
      for my $o ( 0 .. $raw_ref->[0]->dim(0) - 1 ) {
        my @cell = map { $_($o)->squeeze } @$raw_ref[@v];
        push @i_cm, join('', @cell); 
      }
      my ($inter, $map) = effect_code( \@i_cm );
      push @inter_cm, $inter;

        # get the order to put means in correct multi dim pdl pos
        # this is order in var_e dim(1)
      my @levels = sort { $map->{$a} <=> $map->{$b} } keys %$map;
        # this is order needed for cell mean
      my @i_cmo  = sort { reverse($levels[$a]) cmp reverse($levels[$b]) }
                        0 .. $#levels;
      push @inter_cmo, pdl @i_cmo;
    }
  }
    # append info re inter to main effects
  return ([@$var_ref, @inter], [@$i_cmo_ref, @inter_cmo],
          [@$idv, @idv_inter], [@$var_ref, @inter_cm]     );
}

sub _cell_means {
  my ($data, $ivs_ref, $i_cmo_ref, $ids, $raw_ref) = @_;

  my %ind_id;
  @ind_id{ @$ids } = 0..$#$ids;

  my %cm;
  my $i = 0;
  for (@$ivs_ref) {
    my $last = zeroes $_->dim(0);
    my $i_neg = which $_( ,0) == -1;
    $last($i_neg) .= 1;
    $_->where($_ == -1) .= 0;
    $_ = $_->glue(1, $last);

    my @v = split ' ~ ', $ids->[$i];
    my @shape = map { $raw_ref->[$_]->uniq->nelem } @ind_id{@v};

    my ($m, $ss) = $data->centroid( $_ );
    $m  = $m($i_cmo_ref->[$i])->sever;
    $ss = $ss($i_cmo_ref->[$i])->sever;
    $m = $m->reshape(@shape);
    $m->getndims == 1 and $m = $m->dummy(1);
    my $se = sqrt( ($ss/($_->sumover - 1)) / $_->sumover )->reshape(@shape);
    $se->getndims == 1 and $se = $se->dummy(1);
    $cm{ "# $ids->[$i] # m" }  = $m;
    $cm{ "# $ids->[$i] # se" } = $se;
    $i++;
  }
  return \%cm;
}

  # http://www.perlmonks.org/?node_id=371228
sub _combinations {
  my ($num, $arr) = @_;

  return sub { return }
    if $num == 0 or $num > @$arr;

  my @pick;

  return sub {
    return @$arr[ @pick = ( 0 .. $num - 1 ) ]
      unless @pick;
    
    my $i = $#pick;
    $i-- until $i < 0 or $pick[$i]++ < @$arr - $num + $i;
    return if $i < 0;

    @pick[$i .. $#pick] = $pick[$i] .. $#$arr;
    
    return @$arr[@pick];
  };
}

=head2 anova_rptd

Repeated measures anova. Uses type III sum of squares. Automatically removes bad data listwise, ie remove a subject's data if there is any cell missing for the subject.

Between-subject factor support upcoming. Stay tuned.

Default options (case insensitive):

    V      => 1,       # carps if bad value in dv
    IVNM   => undef,   # auto filled as ['IV_0', 'IV_1', ... ]
    PLOT   => 1,       # plots highest order effect

Usage:

    Some fictional data: recall_w_beer_and_wings.txt
  
    Subject Beer    Wings   Recall
    Alex    1       1       8
    Alex    1       2       9
    Alex    1       3       5
    Alex    2       1       7
    Alex    2       2       9
    Alex    2       3       12
    Brian   1       1       12
    Brian   1       2       13
    Brian   1       3       14
    Brian   2       1       16
    Brian   2       2       13
    Brian   2       3       14
    ...
  
      # get_data allows text only in 1st row and col
    my ($data, $idv, $subj) = get_data 'recall_w_beer_and_wings.txt';
  
    my ($b, $w, $dv) = $data->dog;
      # subj and ivs can be 1d pdl or @ ref
    my %m = $dv->anova_rptd( $subj, $b, $w, {ivnm=>['beer', 'wings']} );
  
    print "$_\t$m{$_}\n" for (sort keys %m);
  
    # beer # m
    [
     [ 10.333333      12.75]
    ]
  
    # beer # se
    [
     [  0.987293  1.4622782]
    ]
  
    # beer ~ wings # m
    [
     [  9.5    11]
     [10.75 12.25]
     [10.75    15]
    ]
  
    # beer ~ wings # se
    [
     [       1.5  3.2403703]
     [  1.652019  2.1360009]
     [ 2.3228933  2.3804761]
    ]
  
    # wings # m
    [
     [ 10.25   11.5 12.875]
    ]
  
    # wings # se
    [
     [  1.677051  1.2817399  1.7365555]
    ]
  
    ss_residual	23.5833333333333
    ss_subject	309.125
    ss_total	445.958333333333
    | beer | F	5.90866510538642
    | beer | F_p	0.0932654283732719
    | beer | df	1
    | beer | ms	35.0416666666667
    | beer | ss	35.0416666666667
    | beer || err df	3
    | beer || err ms	5.93055555555555
    | beer || err ss	17.7916666666667
    | beer ~ wings | F	1.28268551236749
    | beer ~ wings | F_p	0.343728237549026
    | beer ~ wings | df	2
    | beer ~ wings | ms	5.04166666666667
    | beer ~ wings | ss	10.0833333333333
    | beer ~ wings || err df	6
    | beer ~ wings || err ms	3.93055555555556
    | beer ~ wings || err ss	23.5833333333333
    | wings | F	3.63736263736264
    | wings | F_p	0.0923372901981468
    | wings | df	2
    | wings | ms	13.7916666666667
    | wings | ss	27.5833333333333
    | wings || err df	6
    | wings || err ms	3.79166666666666
    | wings || err ss	22.75

=cut

*anova_rptd = \&PDL::anova_rptd;
sub PDL::anova_rptd {
  my $opt = pop @_
    if ref $_[-1] eq 'HASH';
  my ($self, $subj, @ivs_raw) = @_;

  for (@ivs_raw) {
    croak "too many dims in IV!"
      if ref $_ eq 'PDL' and $_->squeeze->ndims > 1
  }

  my %opt = (
    V      => 1,       # carps if bad value in dv
    IVNM   => undef,   # auto filled as ['IV_0', 'IV_1', ... ]
    BTWN   => [],      # not implemented yet
    PLOT   => 1,       # plots highest order effect
  );
  $opt and $opt{uc $_} = $opt->{$_} for (keys %$opt);
  $opt{IVNM} ||= [ map { "IV_$_" } 0 .. $#ivs_raw ];
  my @idv = @{ $opt{IVNM} };

  my %ret;

    # create new vars here so we don't mess up original caller @
  my ($sj, @pdl_ivs_raw)
    = map { my $var = (ref $_ eq 'PDL')? [list $_] : $_;
            scalar PDL::Stats::Kmeans::_array_to_pdl $var;
          } ( $subj, @ivs_raw );

    # delete bad data listwise ie remove subj if any cell missing
  $self = $self->squeeze;
  my $ibad = which $self->isbad;
  my $sj_bad = $sj($ibad)->uniq;
  if ($sj_bad->nelem) {
    carp $sj_bad->nelem . " subjects with missing data removed"
      if $opt{V};
    $sj = $sj->setvaltobad($_)
      for (list $sj_bad);
    my $igood = which $sj->isgood;
    for ($self, $sj, @pdl_ivs_raw) {
      $_ = $_( $igood )->sever;
      $_->badflag(0);
    }
  }
    # code for ivs and cell mean in diff @s: effect_code vs iv_cluster
  my ($ivs_ref, $i_cmo_ref)
    = _effect_code_ivs( \@pdl_ivs_raw );

  ($ivs_ref, $i_cmo_ref, my( $idv, $ivs_cm_ref))
    = _add_interactions( $ivs_ref, $i_cmo_ref, \@idv, \@pdl_ivs_raw );

    # matches $ivs_ref, with an extra last pdl for subj effect
  my $err_ref
    = _add_errors( $sj, $ivs_ref, $idv, \@ivs_raw, \%opt );

    # stitch together
  my $ivs = PDL->null->glue( 1, @$ivs_ref );
  $ivs = $ivs->glue(1, grep { defined $_ } @$err_ref);
  $ivs = $ivs->glue(1, ones $ivs->dim(0));

  my $b_full = $self->ols_t( $ivs, {CONST=>0} );

  $ret{ss_total} = $self->ss;
  $ret{ss_residual} = $self->sse( sumover( $b_full * $ivs->xchg(0,1) ) );

  my @full = (@$ivs_ref, @$err_ref);
  EFFECT: for my $k (0 .. $#full) {
    my $e = ($k > $#$ivs_ref)?  '| err' : '';
    my $i = ($k > $#$ivs_ref)?  $k - @$ivs_ref : $k;

    if (!defined $full[$k]) {
      $ret{ "| $idv->[$i] |$e ss" } = $ret{ss_residual};
        # highest ord inter for purely within design, (p-1)*(q-1)*(n-1)
      $ret{ "| $idv->[$i] |$e df" }
        = pdl(map { $_->dim(1) } @full[0 .. $#ivs_raw, -1])->prodover;
      $ret{ "| $idv->[$i] |$e ms" }
        = $ret{ "| $idv->[$i] |$e ss" } / $ret{ "| $idv->[$i] |$e df" };
      next EFFECT;
    }

    my (@G, $G, $b_G);
    @G = grep { $_ != $k and defined $full[$_] } (0 .. $#full);
 
    next EFFECT
      unless @G;

    $G = PDL->null->glue( 1, @full[@G] );
    $G = $G->glue(1, ones $G->dim(0));
    $b_G = $self->ols_t( $G, {CONST=>0} );

    if ($k == $#full) {
      $ret{ss_subject}
        = $self->sse(sumover($b_G * $G->transpose)) - $ret{ss_residual};
    }
    else {
      $ret{ "| $idv->[$i] |$e ss" }
        = $self->sse(sumover($b_G * $G->transpose)) - $ret{ss_residual};
      $ret{ "| $idv->[$i] |$e df" }
        = $full[$k]->dim(1);
      $ret{ "| $idv->[$i] |$e ms" }
        = $ret{ "| $idv->[$i] |$e ss" } / $ret{ "| $idv->[$i] |$e df" };
    }
  }
    # have all iv, inter, and error effects. get F and F_p
  for (0 .. $#$ivs_ref) {
    $ret{ "| $idv->[$_] | F" }
      = $ret{ "| $idv->[$_] | ms" } / $ret{ "| $idv->[$_] || err ms" };
    $ret{ "| $idv->[$_] | F_p" }
      = 1 - $ret{ "| $idv->[$_] | F" }->gsl_cdf_fdist_P(
        $ret{ "| $idv->[$_] | df" }, $ret{ "| $idv->[$_] || err df" } )
      if $CDF;
  }

  for (keys %ret) {ref $ret{$_} eq 'PDL' and $ret{$_} = $ret{$_}->squeeze};

  my $cm_ref
    = _cell_means( $self, $ivs_cm_ref, $i_cmo_ref, $idv, \@pdl_ivs_raw );
    # sort bc we can't count on perl % internal key order implementation
  @ret{ sort keys %$cm_ref } = @$cm_ref{ sort keys %$cm_ref };

  my $highest = join(' ~ ', @{ $opt{IVNM} });
  $cm_ref->{"# $highest # m"}->plot_means(
                        {SE=>$cm_ref->{"# $highest # se"}, IVNM=>$idv, } )
    if $opt{PLOT};

  return %ret;
}

sub _add_errors {
  my ($subj, $ivs_ref, $idv, $raw_ivs, $opt) = @_;

  # code (btwn group) subjects. Rutherford (2001) pp 101-102 

  my (@grp, %grp_s);
  for my $n (0 .. $subj->nelem - 1) {
    my $s = '';
    $s .= $_->[$n]
      for (@$raw_ivs[@{ $opt->{BTWN} }]);
    push @grp, $s;                 # group membership
    $s .= $subj($n);               # keep track of total uniq subj
    $grp_s{$s} = 1;
  }
  my $grp = PDL::Stats::Kmeans::iv_cluster \@grp;

  my $spdl = zeroes $subj->dim(0), keys(%grp_s) - $grp->dim(1);
  my ($d0, $d1) = (0, 0);
  for my $g (0 .. $grp->dim(1)-1) {
    my $gsub = $subj( which $grp( ,$g) )->effect_code;
    my ($nobs, $nsub) = $gsub->dims;
    $spdl($d0:$d0+$nobs-1, $d1:$d1+$nsub-1) .= $gsub;
    $d0 += $nobs;
    $d1 += $nsub;
  }

  # if btwn factor involved, or highest order inter for within factors
  # elem is undef, so that
  # @errors ind matches @$ivs_ref, with an extra elem at the end for subj

    # mark btwn factors to exclude *subj error terms
  my @qr = map { "(?:$idv->[$_])" } @{ $opt->{BTWN} };
  my $qr = join('|', @qr);

  my @errors = map
    { my @fs = split '~', $idv->[$_];
        # btwn factors involved
      if ($qr and $idv->[$_] =~ /$qr/)                            { undef }
        # highest order interaction of within factors
      elsif ( @fs == @$raw_ivs - @{$opt->{BTWN}} )                { undef }
      else            { PDL::clump($ivs_ref->[$_] * $spdl->dummy(1), 1,2) }
    } 0 .. $#$ivs_ref;

  push @errors, $spdl;

  return \@errors;
}

=head2 dummy_code

=for ref

Dummy coding of nominal variable (perl @ ref or 1d pdl) for use in regression.

=for usage

    perldl> @a = qw(a a a b b b c c c)
    perldl> p $a = dummy_code(\@a)
    [
     [1 1 1 0 0 0 0 0 0]
     [0 0 0 1 1 1 0 0 0]
    ]

=cut

sub dummy_code {
  my ($var_ref) = @_;

  my $var_e = effect_code( $var_ref );

  return $var_e->where( $var_e == -1 ) .= 0;
}

=head2 effect_code

=for ref

Unweighted effect coding of nominal variable (perl @ ref or 1d pdl) for use in regression. returns in @ context coded pdl and % ref to level - pdl->dim(1) index.

=for usage

    my @var = qw( a a a b b b c c c );
    my ($var_e, $map) = effect_code( \@var );

    print $var_e . $var_e->info . "\n";
    
    [
     [ 1  1  1  0  0  0 -1 -1 -1]
     [ 0  0  0  1  1  1 -1 -1 -1]
    ]    
    PDL: Double D [9,2]

    print "$_\t$map->{$_}\n" for (sort keys %$map)
    a       0
    b       1
    c       2

=cut

*effect_code = \&PDL::effect_code;
sub PDL::effect_code {
  my ($var_ref) = @_;

    # pdl->uniq puts elem in order. so instead list it to maintain old order
  if (ref $var_ref eq 'PDL') {
    $var_ref = $var_ref->squeeze;
    $var_ref->getndims > 1 and
      croak "multidim pdl passed for single var!";
    $var_ref = [ list $var_ref ];
  }

  my ($var, $map_ref) = PDL::Stats::Kmeans::_array_to_pdl( $var_ref );
  my $var_e = zeroes float, $var->nelem, $var->max;

  for my $l (0 .. $var->max - 1) {
    my $v = $var_e( ,$l);
    $v->index( which $var == $l ) .= 1;
    $v->index( which $var == $var->max ) .= -1;
  }

  return wantarray? ($var_e, $map_ref) : $var_e;
}

=head2 effect_code_w

=for ref

Weighted effect code for nominal variable. returns in @ context coded pdl and % ref to level - pdl->dim(1) index.

=for usage

    perldl> @a = qw( a a b b b c c )
    perldl> p $a = effect_code_w(\@a)
    [
     [   1    1    0    0    0   -1   -1]
     [   0    0    1    1    1 -1.5 -1.5]
    ]

=cut

*effect_code_w = \&PDL::effect_code_w;
sub PDL::effect_code_w {
  my ($var_ref) = @_;

  my ($var_e, $map_ref) = effect_code( $var_ref );

  if ($var_e->sum == 0) {
    return wantarray? ($var_e, $map_ref) : $var_e;
  }

  for (0..$var_e->dim(1)-1) {
    my $factor = $var_e( ,$_);
    my $pos = which $factor == 1;
    my $neg = which $factor == -1;
    my $w = $pos->nelem / $neg->nelem;
    $factor($neg) *= $w;
  }

  return wantarray? ($var_e, $map_ref) : $var_e;
}

=head2 ols

=for ref

Ordinary least squares regression, aka linear regression. Unlike B<ols_t>, ols returns the full model in list context with various stats, but is not threadable. $ivs should be pdl dims $y->nelem or $y->nelem x n_iv. Intercept is LAST of the coeffs if CONST=>1.

=for options

Default options (case insensitive): 

    CONST  => 1,

=for usage

Usage:

    # suppose this is a person's ratings for top 10 box office movies
    # ascending sorted by box office

    perldl> p $y = qsort ceil( random(10) * 5 )
    [1 1 2 2 2 2 4 4 5 5]

    # construct IV with linear and quadratic component

    perldl> p $x = cat sequence(10), sequence(10)**2
    [
     [ 0  1  2  3  4  5  6  7  8  9]
     [ 0  1  4  9 16 25 36 49 64 81]
    ]

    perldl> %m = $y->ols( $x )

    perldl> p "$_\t$m{$_}\n" for (sort keys %m)

    F       40.4225352112676
    F_df    [2 7]
    F_p     0.000142834216344756
    R2      0.920314253647587
 
    # coeff  linear     quadratic  constant
 
    b       [0.21212121 0.03030303 0.98181818]
    b_p     [0.32800118 0.20303404 0.039910509]
    b_se    [0.20174693 0.021579989 0.38987581]
    b_t     [ 1.0514223   1.404219  2.5182844]
    ss_model        19.8787878787879
    ss_residual     1.72121212121212
    ss_total        21.6
    y_pred  [0.98181818  1.2242424  1.5272727  1.8909091  2.3151515 2.8  3.3454545  3.9515152  4.6181818  5.3454545]
 
=cut

*ols = \&PDL::ols;
sub PDL::ols {
    # y [n], ivs [n x attr] pdl
  my ($y, $ivs, $opt) = @_;
  my %opt = ( CONST => 1 );
  $opt and $opt{uc $_} = $opt->{$_} for (keys %$opt);

  $y = $y->squeeze;
  $y->getndims > 1 and
    croak "use ols_t for threaded version";

  $ivs = $ivs->dummy(1) if $ivs->getndims == 1;
    # set up ivs and const as ivs
  $opt{CONST} and
    $ivs = $ivs->glue( 1, ones($ivs->dim(0)) );

  # Internally normalise data
  
  my $ymean = (abs($y)->sum)/($y->nelem);
  $ymean = 1 if $ymean == 0;
  my $y2 = $y / $ymean;
 
  # Do the fit
     
  my $Y = $ivs x $y2->dummy(0);

  my $C;
  if ( $SLATEC ) {
    $C = PDL::Slatec::matinv( $ivs x $ivs->xchg(0,1) );
  }
  else {
    $C = inv( $ivs x $ivs->xchg(0,1) );
  }

    # Fitted coefficients vector
  my $coeff = PDL::squeeze( $C x $Y );
     $coeff *= $ymean;        # Un-normalise

  return $coeff
    unless wantarray;

  my %ret;

  $ret{b} = $coeff;
    # ***$coeff x $ivs looks nice but produces nan on successive tries***
  $ret{y_pred} = sumover( $coeff * $ivs->transpose );
  $ret{ss_total} = $opt{CONST}? $y->ss : sum( $y ** 2 );
  $ret{ss_residual} = $y->sse( $ret{y_pred} );
  $ret{ss_model} = $ret{ss_total} - $ret{ss_residual};
  $ret{R2} = $ret{ss_model} / $ret{ss_total};

  my $n_var = $opt{CONST}? $ivs->dim(1) - 1 : $ivs->dim(1);
  $ret{F_df} = pdl( $n_var, $y->nelem - $ivs->dim(1) );
  $ret{F} = $ret{ss_model} / $ret{F_df}->(0)
          / ( $ret{ss_residual} / $ret{F_df}->(1) );
  $ret{F_p} = 1 - $ret{F}->gsl_cdf_fdist_P( $ret{F_df}->dog )
    if $CDF;

  my $se_b = ones( $coeff->dims? $coeff->dims : 1 );

  $opt{CONST} and 
    $se_b(-1) .= sqrt( $ret{ss_residual} / $ret{F_df}->(1) * $C(-1,-1) );

    # get the se for bs by successivly regressing each iv by the rest ivs
  if ($ivs->dim(1) > 1) {
    for my $k (0 .. $n_var-1) {
      my @G = grep { $_ != $k } (0 .. $n_var-1);
      my $G = $ivs->dice_axis(1, \@G);
      $opt{CONST} and
        $G = $G->glue( 1, ones($ivs->dim(0)) );
      my $b_G = $ivs( ,$k)->ols( $G, {CONST=>0} );

      my $ss_res_k = $ivs( ,$k)->squeeze->sse( sumover($b_G * $G->transpose) );

      $se_b($k) .= sqrt( $ret{ss_residual} / $ret{F_df}->(1) / $ss_res_k );
    }
  }
  else {
    $se_b(0)
      .= sqrt( $ret{ss_residual} / $ret{F_df}->(1) / sum( $ivs( ,0)**2 ) );
  }

  $ret{b_se} = $se_b;
  $ret{b_t} = $ret{b} / $ret{b_se};
  $ret{b_p} = 2 * ( 1 - $ret{b_t}->abs->gsl_cdf_tdist_P( $ret{F_df}->(1) ) )
    if $CDF;

  for (keys %ret) { ref $ret{$_} eq 'PDL' and $ret{$_} = $ret{$_}->squeeze };

  return %ret;
}

=head2 logistic

=for ref

Logistic regression with maximum likelihood estimation using PDL::Fit::LM (requires PDL::Slatec. Hence loaded with "require" in the sub instead of "use" at the beginning). Do not supply the constant vector in $x. It is included in the model and returned as last of coeff. Returns full model in list context and coeff in scalar context.

***NOTE: the results here are qualitatively similar to but not identical with results from R, because different algorithms are used for the nonlinear parameter fit. Use with discretion***

=for options

Default options (case insensitive):

    INITP => zeroes( $x->dim(1) + 1 ),
    MAXIT => 1000,
    EPS   => 1e-7,

=for usage

Usage:

    # suppose this is whether a person had rented 10 movies
    # ascending sorted by box office

    perldl> p $y = qsort ushort( random(10)*2 )
    [0 0 0 0 0 0 1 1 1 1]
   
    # IV is box office ranking
 
    perldl> p $x = sequence(10)
    [0 1 2 3 4 5 6 7 8 9]

    perldl> %m = $y->logistic( $x )

    perldl> p "$_\t$m{$_}\n" for (sort keys %m)
    D0      13.4602333401851
    Dm      0
    Dm_chisq        13.4602333401851
    Dm_df   1
    Dm_p    0.00024367344985432
    #         ranking     constant
    b       [ 74.586318  -410.7816]
    b_chisq [ 13.460233  13.065797]
    b_p     [0.00024367345 0.00030073723]
    iter    154
    y_pred  [3.9794114e-179 9.8230271e-147 2.4247772e-114 5.9854711e-82 1.477491e-49 3.6471308e-17 1 1 1 1]
 
=cut

*logistic = \&PDL::logistic;
sub PDL::logistic {
  require PDL::Fit::LM;              # uses PDL::Slatec

  my ( $self, $ivs, $opt ) = @_;
  
  $self = $self->squeeze;
    # make compatible w multiple var cases
  $ivs->getndims == 1 and $ivs = $ivs->dummy(1);
  $self->dim(0) != $ivs->dim(0) and
    carp "mismatched n btwn DV and IV!";

  my %opt = (
    INITP => zeroes( $ivs->dim(1) + 1 ),
    MAXIT => 1000,
    EPS   => 1e-7,
  );
  $opt and $opt{uc $_} = $opt->{$_} for (%$opt);
    # not using it atm
  $opt{WT} = 1;

    # Use lmfit. Fourth input argument is reference to user-defined
    # copy INITP so we have the original value when needed 
  my ($yfit,$coeff,$cov,$iter)
    = PDL::Fit::LM::lmfit($ivs, $self, $opt{WT}, \&_logistic, $opt{INITP}->copy,
      { Maxiter=>$opt{MAXIT}, Eps=>$opt{EPS} } );
    # apparently at least coeff is child of some pdl
    # which is changed in later lmfit calls
  $yfit  = $yfit->copy;
  $coeff = $coeff->copy;

  return $coeff unless wantarray;

  my %ret;

  my $n0 = $self->where($self == 0)->nelem;
  my $n1 = $self->nelem - $n0;

  $ret{D0} = -2*($n0 * log($n0 / $self->nelem) + $n1 * log($n1 / $self->nelem));
  $ret{Dm} = sum( $self->dvrs( $yfit ) ** 2 );
  $ret{Dm_chisq} = $ret{D0} - $ret{Dm};
  $ret{Dm_df} = $ivs->dim(1);
  $ret{Dm_p}
    = 1 - PDL::GSL::CDF::gsl_cdf_chisq_P( $ret{Dm_chisq}, $ret{Dm_df} )
    if $CDF;

  my $coeff_chisq = zeroes $opt{INITP}->nelem;

  if ( $ivs->dim(1) > 1 ) {
    for my $k (0 .. $ivs->dim(1)-1) {
      my @G = grep { $_ != $k } (0 .. $ivs->dim(1)-1);
      my $G = $ivs->dice_axis(1, \@G);
  
      my $init = $opt{INITP}->dice([ @G, $opt{INITP}->dim(0)-1 ])->copy;
      my $y_G
        = PDL::Fit::LM::lmfit( $G, $self, $opt{WT}, \&_logistic, $init,
        { Maxiter=>$opt{MAXIT}, Eps=>$opt{EPS} } );
  
      $coeff_chisq($k) .= $self->dm( $y_G ) - $ret{Dm};
    }
  }
  else {
      # d0 is, by definition, the deviance with only intercept
    $coeff_chisq(0) .= $ret{D0} - $ret{Dm};
  }

  my $y_c
      = PDL::Fit::LM::lmfit( $ivs, $self, $opt{WT}, \&_logistic_no_intercept, $opt{INITP}->(0:-2)->copy,
      { Maxiter=>$opt{MAXIT}, Eps=>$opt{EPS} } );

  $coeff_chisq(-1) .= $self->dm( $y_c ) - $ret{Dm};

  $ret{b} = $coeff;
  $ret{b_chisq} = $coeff_chisq;
  $ret{b_p} = 1 - $ret{b_chisq}->gsl_cdf_chisq_P( 1 )
    if $CDF;
  $ret{y_pred} = $yfit;
  $ret{iter} = $iter;

  for (keys %ret) { ref $ret{$_} eq 'PDL' and $ret{$_} = $ret{$_}->squeeze };

  return %ret;
}

sub _logistic {
  my ($x,$par,$ym,$dyda) = @_;

    # $b and $c are fit parameters slope and intercept
  my $b = $par(0 : $x->dim(1) - 1)->sever;
  my $c = $par(-1)->sever;
    
    # Write function with dependent variable $ym,
    # independent variable $x, and fit parameters as specified above.
    # Use the .= (dot equals) assignment operator to express the equality 
    # (not just a plain equals)
  $ym .= 1 / ( 1 + exp( -1 * (sumover($b * $x->transpose) + $c) ) );

  my (@dy) = map {$dyda -> slice(",($_)") } (0 .. $par->dim(0)-1);

    # Partial derivative of the function with respect to each slope 
    # fit parameter ($b in this case). Again, note .= assignment 
    # operator (not just "equals")
  $dy[$_] .= $x( ,$_) * $ym * (1 - $ym)
    for (0 .. $b->dim(0)-1);

    # Partial derivative of the function re intercept par
  $dy[-1] .= $ym * (1 - $ym);
}

sub _logistic_no_intercept {
  my ($x,$par,$ym,$dyda) = @_;
    
  my $b = $par(0 : $x->dim(1) - 1)->sever;

    # Write function with dependent variable $ym,
    # independent variable $x, and fit parameters as specified above.
    # Use the .= (dot equals) assignment operator to express the equality 
    # (not just a plain equals)
  $ym .= 1 / ( 1 + exp( -1 * sumover($b * $x->transpose) ) );

  my (@dy) = map {$dyda -> slice(",($_)") } (0 .. $par->dim(0)-1);

    # Partial derivative of the function with respect to each slope 
    # fit parameter ($b in this case). Again, note .= assignment 
    # operator (not just "equals")
  $dy[$_] .= $x( ,$_) * $ym * (1 - $ym)
    for (0 .. $b->dim(0)-1);
}

=head2 pca

=for ref

Principal component analysis. $data is pdl dim obs x var. Output loading (corr between var and component) and score are pdls dim var x component. value and variance are pdls dim component.

Based on corr instead of cov (bad values are ignored pair-wise. OK when bad values are few but otherwise probably should fill_m etc before pca). Use PDL::Slatec::eigsys() if installed, otherwise use PDL::MatrixOps::eigens_sym(). Added loadings and descending sorted component by $value (ie variance accouted for).

=for options

Default options (case insensitive):

    PLOT  => 1,       # scree plot for var accounted for

=for usage

Usage:

    my $data   = random 100, 20;       # 100 obs on 20 var
    my %result = $data->pca;
    print "$_\t$result{$_}\n" for (keys %result);

=cut

*pca = \&PDL::pca;
sub PDL::pca {
  my ($self, $opt) = @_;

  my %opt = (
    PLOT  => 1,
  );
  $opt and $opt{uc $_} = $opt->{$_} for (keys %$opt);

  my $var_var = $self->corr_table;

    # value is axis pdl and score is var x axis
  my ($value, $score);
  if ( $SLATEC ) {
    ($value, $score) = $var_var->PDL::Slatec::eigsys;
  }
  else {
    ($score, $value) = $var_var->eigens_sym;
      # compatibility with PDL::Slatec::eigsys
    $score = $score->inplace->transpose->sever;
  }

  my $ind_sorted = pdl reverse list qsorti $value;
  $score = $score->inplace->dice_axis(1, $ind_sorted)->sever;
  $value = $value->inplace->dice($ind_sorted)->sever;

    # var x axis
  my $loading = $score * sqrt( $value->transpose );
  my $var     = $value / $self->dim(1);

  $var->plot_scree
    if $opt{PLOT};

  return ( loading=>$loading, value=>$value, score=>$score, var=>$var ); 
}

=head2 pca_sorti

Determine by which vars a component is best represented. Descending sort vars by size of association with that component. Returns sorted var and relevant component indices.

=for options

Default options (case insensitive):

    NCOMP => 10,     # maximum number of components to consider

=for usage

Usage:

      # let's see if we replicated the Osgood et al. (1957) study
    perldl> ($data, $idv, $ido) = get_data 'osgood_exp.csv', {v=>0}

      # select a subset of var to do pca
    perldl> $ind = which_id $idv, [qw( ACTIVE BASS BRIGHT CALM FAST GOOD HAPPY HARD LARGE HEAVY )]
    perldl> $data = $data( ,$ind)->sever
    perldl> @$idv = @$idv[list $ind]

    perldl> %m = $data->pca
 
    perldl> ($iv, $ic) = $m{loading}->pca_sorti()

    perldl> p "$idv->[$_]\t" . $m{loading}->($_,$ic)->flat . "\n" for (list $iv)
             #   COMP0     COMP1    COMP2    COMP3
    HAPPY	[0.860191 0.364911 0.174372 -0.10484]
    GOOD	[0.848694 0.303652 0.198378 -0.115177]
    CALM	[0.821177 -0.130542 0.396215 -0.125368]
    BRIGHT	[0.78303 0.232808 -0.0534081 -0.0528796]
    HEAVY	[-0.623036 0.454826 0.50447 0.073007]
    HARD	[-0.679179 0.0505568 0.384467 0.165608]
    ACTIVE	[-0.161098 0.760778 -0.44893 -0.0888592]
    FAST	[-0.196042 0.71479 -0.471355 0.00460276]
    LARGE	[-0.241994 0.594644 0.634703 -0.00618055]
    BASS	[-0.621213 -0.124918 0.0605367 -0.765184]
    
=cut

*pca_sorti = \&PDL::pca_sorti;
sub PDL::pca_sorti {
    # $self is pdl (var x component)
  my ($self, $opt) = @_;

  my %opt = (
    NCOMP => 10,     # maximum number of components to consider
  );
  $opt and $opt{uc $_} = $opt->{$_} for (keys %$opt);

  my $ncomp = pdl($opt{NCOMP}, $self->dim(1))->min;
  $self = $self->dice_axis( 1, pdl(0..$ncomp-1) );
  
  my $icomp = $self->transpose->abs->maximum_ind;
 
    # sort between comp
  my $ivar_sort = $icomp->qsorti;
  $self = $self($ivar_sort, )->sever;

    # sort within comp
  my $ic = $icomp($ivar_sort)->iv_cluster;
  for my $comp (0 .. $ic->dim(1)-1) {
    my $i = $self(which($ic( ,$comp)), $comp)->qsorti;
      # descending sort by size
    $i = pdl(reverse list $i);
    $ivar_sort(which $ic( ,$comp))
      .= $ivar_sort(which $ic( ,$comp))->($i)->sever;
  }
  return wantarray? ($ivar_sort, pdl(0 .. $ic->dim(1)-1)) : $ivar_sort;
}

=head2 plot_means

Plots means anova style. Can handle up to 4-way interactions (ie 4D pdl).

=for options

Default options (case insensitive):

    IVNM  => ['IV_0', 'IV_1', 'IV_2', 'IV_3'],
    DVNM  => 'DV',
    AUTO  => 1,       # auto set dims to be on x-axis, line, panel
                      # if set 0, dim 0 goes on x-axis, dim 1 as lines
                      # dim 2+ as panels
      # see PDL::Graphics::PGPLOT::Window for next options
    WIN   => undef,   # pgwin object. not closed here if passed
                      # allows comparing multiple lines in same plot
                      # set env before passing WIN
    DEV   => '/xs',         # open and close dev for plotting if no WIN
    SIZE  => 480,           # individual square panel size in pixels
    SYMBL => [0, 4, 7, 11], 

=for usage

Usage:

      # see anova for mean / se pdl structure
    $mean->plot_means( $se, {IVNM=>['apple', 'bake']} );
  
Or like this:

    $m{'# apple ~ bake # m'}->plot_means;

=cut

*plot_means = \&PDL::plot_means;
sub PDL::plot_means {
  my $opt = pop @_
    if ref $_[-1] eq 'HASH';
  my ($self, $se) = @_;
  if (!$PGPLOT) {
    carp "No PDL::Graphics::PGPLOT, no plot :(";
    return;
  }
  $self = $self->squeeze;
  if ($self->ndims > 4) {
    carp "data is > 4D!";
    return;
  }

  my %opt = (
    IVNM => ['IV_0', 'IV_1', 'IV_2', 'IV_3'],
    DVNM => 'DV',
    AUTO  => 1,             # auto set vars to be on X axis, line, panel
    WIN   => undef,         # PDL::Graphics::PGPLOT::Window object
    DEV   => '/xs',
    SIZE  => 480,           # individual square panel size in pixels
    SYMBL => [0, 4, 7, 11], # ref PDL::Graphics::PGPLOT::Window 
  );
  $opt and $opt{uc $_} = $opt->{$_} for (keys %$opt);

    # decide which vars to plot as x axis, lines, panels
    # put var w most levels on x axis
    # put var w least levels on diff panels
  my @iD = 0..3;
  my @dims = (1, 1, 1, 1);
    # splice ARRAY,OFFSET,LENGTH,LIST
  splice @dims, 0, $self->ndims, $self->dims;
  $self = $self->reshape(@dims)->sever;
  $se = $se->reshape(@dims)->sever
    if defined $se;
  @iD = reverse list qsorti pdl @dims
    if $opt{AUTO};

    # $iD[0] on x axis
    # $iD[1] as separate lines
  my $nx = $self->dim($iD[2]);    # n xpanels
  my $ny = $self->dim($iD[3]);    # n ypanels
  
  my $w = $opt{WIN};
  if (!defined $w) {
    $w = pgwin(DEV=>$opt{DEV}, NX=>$nx, NY=>$ny,
                 SIZE=>[$opt{SIZE}*$nx, $opt{SIZE}*$ny], UNIT=>3);
  }

  my ($min, $max) = $self->minmax;
  my $range = $max - $min;
  my $p = 0;   # panel

  for my $y (0..$self->dim($iD[3])-1) {
    for my $x (0..$self->dim($iD[2])-1) {
      $p ++;
      my $tl = '';
         $tl = $opt{IVNM}->[$iD[2]] . " $x"  if $self->dim($iD[2]) > 1;
         $tl.= ' ' . $opt{IVNM}->[$iD[3]] . " $y"  if $self->dim($iD[3]) > 1;
      $w->env( 0, $self->dim($iD[0])-1, $min - 2*$range/5, $max + $range/5,
             { XTitle=>$opt{IVNM}->[$iD[0]], YTitle=>$opt{DVNM}, Title=>$tl,                 PANEL=>$p, AXIS=>['BCNT', 'BCNST'], Border=>1, 
              } )
        unless $opt{WIN};
  
      my (@legend, @color);
      for (0 .. $self->dim($iD[1]) - 1) {
        push @legend, $opt{IVNM}->[$iD[1]] . " $_"
          if ($self->dim($iD[1]) > 1);
        push @color, $_ + 2;    # start from red
        $w->points( sequence($self->dim($iD[0])),
        $self->dice_axis($iD[3],$y)->dice_axis($iD[2],$x)->dice_axis($iD[1],$_),
                      $opt{SYMBL}->[$_],
                    { PANEL=>$p, CHARSIZE=>2, COLOR=>$_+2, PLOTLINE=>1, } );
        $w->errb( sequence($self->dim($iD[0])),
        $self->dice_axis($iD[3],$y)->dice_axis($iD[2],$x)->dice_axis($iD[1],$_),
        $se->dice_axis($iD[3],$y)->dice_axis($iD[2],$x)->dice_axis($iD[1],$_),
                    { PANEL=>$p, CHARSIZE=>2, COLOR=>$_+2 }  )
          if defined $se;
      }
      if ($self->dim($iD[1]) > 1) {
        $w->legend( \@legend, ($self->dim($iD[0])-1)/1.6, $min - $range/10,
                   { COLOR=>\@color } );
        $w->legend( \@legend, ($self->dim($iD[0])-1)/1.6, $min - $range/10,
                   { COLOR=>\@color, SYMBOL=>[ @{$opt{SYMBL}}[0..$#color] ] } );
      }
    }
  }
  $w->close
    unless $opt{WIN};

  return;
}

=head2 plot_scree

Scree plot. Plots proportion of variance accounted for by PCA components.

=for options

Default options (case insensitive):

  NCOMP => 20,      # max number of components to plot
  CUT   => 0,       # set TRUE to plot suggested cutoff line for screes
    # see PDL::Graphics::PGPLOT::Window for next options
  WIN   => undef,   # pgwin object. not closed here if passed
                    # allows comparing multiple lines in same plot
                    # set env before passing WIN
  DEV   => '/xs',   # open and close dev for plotting if no WIN
  SIZE  => 480,     # plot size in pixels
  COLOR => 1,

=for usage

Usage:

  # variance should be in descending order
 
  $pca{var}->plot_scree( {ncomp=>16} );

Or, because NCOMP is used so often, it is allowed a shortcut,

  $pca{var}->plot_scree( 16 );

=cut

*plot_scree = \&PDL::plot_scree;
sub PDL::plot_scree {
  if (!$PGPLOT) {
    carp "No PDL::Graphics::PGPLOT, no plot :(";
    return;
  }
  my $opt = pop @_
    if ref $_[-1] eq 'HASH';
  my ($self, $ncomp) = @_;
  my %opt = (
    NCOMP => 20,      # max number of components to plot
    CUT   => 0,       # set TRUE to plot suggested cutoff line for screes
      # see PDL::Graphics::PGPLOT::Window for next options
    WIN   => undef,   # pgwin object. not closed here if passed
                      # allows comparing multiple lines in same plot
                      # set env before passing WIN
    DEV   => '/xs',   # open and close dev for plotting if no WIN
    SIZE  => 480,     # plot size in pixels
    COLOR => 1,
  );
  $opt and $opt{uc $_} = $opt->{$_} for (keys %$opt);
  $ncomp and $opt{NCOMP} = $ncomp;

  my $win = $opt{WIN};
  $ncomp = ($self->dim(0) < $opt{NCOMP})? $self->dim(0) : $opt{NCOMP};

  if (!$win) {
   $win = pgwin(DEV=>$opt{DEV}, SIZE=>[$opt{SIZE}, $opt{SIZE}], UNIT=>3);
   $win->env(0, $ncomp-1, 0, $self->max,
     {XTitle=>'Compoment', YTitle=>'Proportion of Variance Accounted for',
     AXIS=>['BCNT', 'BCNST'], Border=>1, });
  }
  $win->points(sequence($ncomp), $self(0:$ncomp-1, ),
        {CHARSIZE=>2, COLOR=>$opt{COLOR}, PLOTLINE=>1} );
  if ($opt{CUT}) {
    my $iscree = PDL::Stats::Kmeans::_scree_ind $self(0:$ncomp-1);
    $win->line( pdl($iscree-.5, $iscree-.5), pdl(-.05, $self->max+.05),
                {COLOR=>15} );
  }
  $win->close
    unless $opt{WIN};
  return;
}

=head1 SEE ALSO

L<PDL::Fit::Linfit>

L<PDL::Fit::LM>

=head1 REFERENCES

Cohen, J., Cohen, P., West, S.G., & Aiken, L.S. (2003). Applied Multiple Regression/correlation Analysis for the Behavioral Sciences (3rd ed.). Mahwah, NJ: Lawrence Erlbaum Associates Publishers.

Hosmer, D.W., & Lemeshow, S. (2000). Applied Logistic Regression (2nd ed.). New York, NY: Wiley-Interscience. 

Osgood C.E., Suci, G.J., & Tannenbaum, P.H. (1957). The Measurement of Meaning. Champaign, IL: University of Illinois Press.

Rutherford, A. (2001). Introducing Anova and Ancova: A GLM Approach (1st ed.). Thousand Oaks, CA: Sage Publications.

The GLM procedure: unbalanced ANOVA for two-way design with interaction. (2008). SAS/STAT(R) 9.2 User's Guide. Retrieved June 18, 2009 from http://support.sas.com/

=head1 AUTHOR

Copyright (C) 2009 Maggie J. Xiong <maggiexyz users.sourceforge.net>

All rights reserved. There is no warranty. You are allowed to redistribute this software / documentation as described in the file COPYING in the PDL distribution.

=cut



;



# Exit with OK status

1;

		   