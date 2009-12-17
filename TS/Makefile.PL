   use PDL::Core::Dev;
   use ExtUtils::MakeMaker;
   PDL::Core::Dev->import();
   
   $package = ["ts.pp",TS,PDL::Stats::TS];
   %hash = pdlpp_stdargs($package);

   $hash{ 'VERSION_FROM' } = '../Stats.pm';
 
   WriteMakefile( %hash );
   
   sub MY::postamble {pdlpp_postamble($package)};
