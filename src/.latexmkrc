# This shows how to use the glossaries package
# (http://www.ctan.org/pkg/glossaries) and the glossaries-extra package
# (http://www.ctan.org/pkg/glossaries-extra) with latexmk.

# N.B. There is also the OBSOLETE glossary package
# (http://www.ctan.org/pkg/glossary), which has some differences.  See item 2.

# 1. If you use the glossaries or the glossaries-extra package, then you can use:


# Clean-up test, added by me (NvM) https://tex.stackexchange.com/questions/1226/how-to-make-latexmk-use-makeglossaries/541990#541990
push @generated_exts, 'glo', 'gls', 'glg';
push @generated_exts, 'acn', 'acr', 'alg';
push @generated_exts, 'sls';
$clean_ext .= ' %R.ist %R.xdy';

$cleanup_includes_cusdep_generated = 1; 
$cleanup_includes_generated = 1;

# Add custom dependencies to subroutine
   add_cus_dep('slo', 'sls', 0, 'makeglossaries');
   add_cus_dep( 'acn', 'acr', 0, 'makeglossaries' );
   add_cus_dep( 'glo', 'gls', 0, 'makeglossaries' );
   $clean_ext .= " acr acn alg glo gls glg";

sub makeglossaries {
  system("makeglossaries $_[0]");
}
  # sub makeglossaries {
  #      my ($base_name, $path) = fileparse( $_[0] );
  #      my @args = ( "-q", "-d", $path, $base_name );
  #      if ($silent) { unshift @args, "-q"; }
  #      return system "makeglossaries", "-d", $path, $base_name; 
  #  }






# from the documentation for V. 2.03 of asymptote:
sub asy {return system("asy \"$_[0]\"");}
add_cus_dep("asy","eps",0,"asy");
add_cus_dep("asy","pdf",0,"asy");
add_cus_dep("asy","tex",0,"asy");

# metapost rule from http://tex.stackexchange.com/questions/37134
add_cus_dep('mp', '1', 0, 'mpost');
sub mpost {
  my $file = $_[0];
  my ($name, $path) = fileparse($file);
  pushd($path);
  my $return = system "mpost $name";
  popd();
  return $return;
}


# 2. The above will tend to run makeglossaries more often than needed, since
#    each out of date file will trigger the use of makeglossaries, even
#    though makeglossaries makes all the glossaries.  The following solution
#    solves this, but at the expense of not having the convenience that
#    makeglossaries can change how it makes the glossaries depending on the
#    settings of the glossaries package.
#    A better solution will need more advanced work.
#
#   add_cus_dep( 'acn', 'acr', 0, 'makeglossaries' );
#   add_cus_dep( 'glo', 'gls', 0, 'makeglossaries' );
#   $clean_ext .= " acr acn alg glo gls glg";
#
#   sub makeglossaries {
#       my @args = ( "-s", "$_[0].ist", "-t", "$$Psource.ilg",
#                    "-o", $$Pdest, $$Psource );
#       if ($silent) { unshift @args, "-q"; }
#       return system "makeindex", @args;
#   }