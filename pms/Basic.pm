
#
# GENERATED WITH PDL::PP! Don't modify!
#
package PDL::Stats::Basic;

@EXPORT_OK  = qw(  get_data which_id PDL::PP stdv PDL::PP stdv_unbiased PDL::PP var PDL::PP var_unbiased PDL::PP se PDL::PP ss PDL::PP skew PDL::PP skew_unbiased PDL::PP kurt PDL::PP kurt_unbiased PDL::PP cov PDL::PP corr PDL::PP t_corr PDL::PP n_pair PDL::PP corr_dev PDL::PP t_test PDL::PP t_test_nev PDL::PP t_test_paired );
%EXPORT_TAGS = (Func=>[@EXPORT_OK]);

use PDL::Core;
use PDL::Exporter;
use DynaLoader;



   
   @ISA    = ( 'PDL::Exporter','DynaLoader' );
   push @PDL::Core::PP, __PACKAGE__;
   bootstrap PDL::Stats::Basic ;





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







=head1 FUNCTIONS



=cut






=head2 stdv

=for sig

  Signature: (a(n); float+ [o]b())



=for ref

Sample standard deviation.

  

=for bad

stdv does handle bad values.
It will set the bad-value flag of all output piddles if the flag is set for any of the input piddles.


=cut






*stdv = \&PDL::stdv;




=head2 stdv_unbiased

=for sig

  Signature: (a(n); float+ [o]b())



=for ref

Unbiased estimate of population standard deviation.

  

=for bad

stdv_unbiased does handle bad values.
It will set the bad-value flag of all output piddles if the flag is set for any of the input piddles.


=cut






*stdv_unbiased = \&PDL::stdv_unbiased;




=head2 var

=for sig

  Signature: (a(n); float+ [o]b())



=for ref

Sample variance.

  

=for bad

var does handle bad values.
It will set the bad-value flag of all output piddles if the flag is set for any of the input piddles.


=cut






*var = \&PDL::var;




=head2 var_unbiased

=for sig

  Signature: (a(n); float+ [o]b())



=for ref

Unbiased estimate of population variance.

  

=for bad

var_unbiased does handle bad values.
It will set the bad-value flag of all output piddles if the flag is set for any of the input piddles.


=cut






*var_unbiased = \&PDL::var_unbiased;




=head2 se

=for sig

  Signature: (a(n); float+ [o]b())



=for usage

    # 95% confidence interval for samples with large N

    $ci_95_upper = $data->average + 1.96 * $data->se;
    $ci_95_lower = $data->average - 1.96 * $data->se;

=for ref

Standard error of the mean. Useful for calculating confidence intervals.

  

=for bad

se does handle bad values.
It will set the bad-value flag of all output piddles if the flag is set for any of the input piddles.


=cut






*se = \&PDL::se;




=head2 ss

=for sig

  Signature: (a(n); float+ [o]b())



=for ref

sum of squared deviations from the mean

  

=for bad

ss does handle bad values.
It will set the bad-value flag of all output piddles if the flag is set for any of the input piddles.


=cut






*ss = \&PDL::ss;




=head2 skew

=for sig

  Signature: (a(n); float+ [o]b())



=for ref

sample skewness. measure of asymmetry in data. skewness == 0 for normal distribution.

  

=for bad

skew does handle bad values.
It will set the bad-value flag of all output piddles if the flag is set for any of the input piddles.


=cut






*skew = \&PDL::skew;




=head2 skew_unbiased

=for sig

  Signature: (a(n); float+ [o]b())



=for ref

unbiased estimate of population skewness. this is the number in GNumeric Descriptive Statistics.

  

=for bad

skew_unbiased does handle bad values.
It will set the bad-value flag of all output piddles if the flag is set for any of the input piddles.


=cut






*skew_unbiased = \&PDL::skew_unbiased;




=head2 kurt

=for sig

  Signature: (a(n); float+ [o]b())



=for ref

sample kurtosis. measure of "peakedness" of data. kurtosis == 0 for normal distribution. 

  

=for bad

kurt does handle bad values.
It will set the bad-value flag of all output piddles if the flag is set for any of the input piddles.


=cut






*kurt = \&PDL::kurt;




=head2 kurt_unbiased

=for sig

  Signature: (a(n); float+ [o]b())



=for ref

unbiased estimate of population kurtosis. this is the number in GNumeric Descriptive Statistics.

  

=for bad

kurt_unbiased does handle bad values.
It will set the bad-value flag of all output piddles if the flag is set for any of the input piddles.


=cut






*kurt_unbiased = \&PDL::kurt_unbiased;




=head2 cov

=for sig

  Signature: (a(n); b(n); float+ [o]c())



=for ref

sample covariance. see B<corr> for ways to call

  

=for bad

cov does handle bad values.
It will set the bad-value flag of all output piddles if the flag is set for any of the input piddles.


=cut






*cov = \&PDL::cov;




=head2 corr

=for sig

  Signature: (a(n); b(n); float+ [o]c())



=for usage 

    perldl> $a = random 5, 3
    perldl> $b = sequence 5,3
    perldl> p $a->corr($b)

    [0.20934208 0.30949881 0.26713007]

for square corr table

    perldl> p $a->corr($a->dummy(1,1))

    [
     [           1  -0.41995259 -0.029301192]
     [ -0.41995259            1  -0.61927619]
     [-0.029301192  -0.61927619            1]
    ]

=for ref

pearson correlation coefficient. r = cov(X,Y) / (stdv(X) * stdv(Y)).

  

=for bad

corr does handle bad values.
It will set the bad-value flag of all output piddles if the flag is set for any of the input piddles.


=cut






*corr = \&PDL::corr;




=head2 t_corr

=for sig

  Signature: (r(); n(); [o]t())



=for usage

    $corr   = $data->corr( $data->dummy(1,1) );
    $n      = $data->n_pair( $data->dummy(1,1) );
    $t_corr = $corr->t_corr( $n );

    use PDL::GSL::CDF;

    $p_2tail = 2 * (1 - gsl_cdf_tdist_P( $t_corr->abs, $n-2 ));

=for ref

t significance test for Pearson correlations.

  

=for bad

t_corr does handle bad values.
It will set the bad-value flag of all output piddles if the flag is set for any of the input piddles.


=cut






*t_corr = \&PDL::t_corr;




=head2 n_pair

=for sig

  Signature: (a(n); b(n); int [o]c())



=for ref

returns the number of good pairs between 2 lists. useful with B<corr> (esp. when bad values are involved)

  

=for bad

n_pair does handle bad values.
It will set the bad-value flag of all output piddles if the flag is set for any of the input piddles.


=cut






*n_pair = \&PDL::n_pair;




=head2 corr_dev

=for sig

  Signature: (a(n); b(n); float+ [o]c())



=for usage

    $corr = $a->dev_m->corr_dev($b->dev_m);

=for ref

calculates correlations from B<dev_m> vals. seems faster than doing B<corr> from original vals when data pdl is big

  

=for bad

corr_dev does handle bad values.
It will set the bad-value flag of all output piddles if the flag is set for any of the input piddles.


=cut






*corr_dev = \&PDL::corr_dev;




=head2 t_test

=for sig

  Signature: (a(n); b(m); float+ [o]t(); [o]d())



=for usage

    my ($t, $df) = t_test( $pdl1, $pdl2 );

    use PDL::GSL::CDF;

    my $p_2tail = 2 * (1 - gsl_cdf_tdist_P( $t->abs, $df ));

=for ref

independent sample t-test, assuming equal var.

  

=for bad

t_test does handle bad values.
It will set the bad-value flag of all output piddles if the flag is set for any of the input piddles.


=cut






*t_test = \&PDL::t_test;




=head2 t_test_nev

=for sig

  Signature: (a(n); b(m); float+ [o]t(); [o]d())



=for usage

    my ($t, $df) = $pdl1->t_test( $pdl2 );

=for ref

independent sample t-test, NOT assuming equal var. ie Welch two sample t test. Df follows Welch-Satterthwaite equation instead of Satterthwaite (1946, as cited by Hays, 1994, 5th ed.). It matches GNumeric, which matches R.

  

=for bad

t_test_nev does handle bad values.
It will set the bad-value flag of all output piddles if the flag is set for any of the input piddles.


=cut






*t_test_nev = \&PDL::t_test_nev;




=head2 t_test_paired

=for sig

  Signature: (a(n); b(n); float+ [o]t(); [o]d())



=for ref

paired sample t-test.

  

=for bad

t_test_paired does handle bad values.
It will set the bad-value flag of all output piddles if the flag is set for any of the input piddles.


=cut






*t_test_paired = \&PDL::t_test_paired;



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



;



# Exit with OK status

1;

		   