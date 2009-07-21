   use PDL::Core::Dev;
   use ExtUtils::MakeMaker;
   PDL::Core::Dev->import();
   
   $package = ["glm.pp",GLM,PDL::Stats::GLM];
   %hash = pdlpp_stdargs($package); 
   
   $hash{ 'VERSION_FROM' } = '../Stats.pm';
 
   WriteMakefile( %hash );
   
   sub MY::postamble {pdlpp_postamble($package)};
