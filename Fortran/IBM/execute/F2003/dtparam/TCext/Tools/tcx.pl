#!/usr/bin/perl -w
#
#  To Extend the given Test Case using Gaby's Tool providing all of the
#  possible variations detected.
#
#  Usage: $ tcx.pl [-p <Programmer>] [-1 <PrimaryFunctionTested>]       \
#                  [-2 <SecondaryFunctionTested>] [-f <FeatureName>]    \
#                  [-s <CompilerStanza>] [-vf] <OriginalTestCasePath>
#
#  -vf  Save .vf File Extensions (.cols/.lines files) for later processing.
#
#  All other options (-p/-1/-2/-f/-s) allow for edits to the Extended .f
#  File Header that identify changes from the Original Test Case.
#
#  a) The Original Test Case is Filtered (as if it had been extended) and
#     processed to provide a Generic Input File.
#  b) An initial Test Case Extension is performed to determine all possible
#     Options that may be specified using Gaby's Tool.
#  c) Option Variations are expanded into their -q<Opt>/-qno<Opt> variants,
#     and a list of Possible Options is generated.
#  d) The list of Possible Options is processed to determine all Possible
#     Option Combinations for this Test Case.
#  e) Each Possible Option Combination is processed:
#
#     i)    Run Gaby's Tool capturing the output.
#     ii)   Build a File Suffix for this possible Extended Test Case (refer
#           to "BuildFileSuffix()" below for details).
#     iii)  Filter the Extended .f (refer to "Filter()" below for details).
#     iv)   Dump the Filtered Extended .f file.
#     v)    Process the .f file to provide a Generic .f file -- .f.gen --
#           (refer to "GenericFile()" below for details).
#     vi)   Use the Generic .f file to compare against other Test Case
#           Extension Variations to ensure this Extension is Unique; if not,
#           delete this Variation and continue.
#     vii)  Rename the .cols/.lines files if the "-vf" Option was specified.
#     viii) Save the Generic File for future Unique Comparisons.
#
#  f) Delete any unwanted files.
#
#  Assumptions:
#  o  Uses Gaby's Test Case Extension Tool ("ftcx_dtp").
#  o  This Tool is available via $PATH.
#  o  All extended Test Cases will reside in the Current Directory.
#

use Getopt::Long;
use File::Basename;

#
#  ftcx_dtp() - To Execute the "ftcx_dtp(Gaby)" Command with the given
#  arguments. (STDERR is redirected to /dev/null).
#
#  Returns:  An array with the Extended Test Case.
#
sub
ftcx_dtp(\@)
{
	return( `ftcx_dtp @_ 2>/dev/null` );
}


#
#  Filter() - To Parse/Filter the Extended Test Case, and provide some
#  consistency in the header.
#
#  Returns:  An array with the Filtered Extended Test Case.
#
sub
Filter($$$$$$$$$$)
{
	my( $ftcxOutputRef ) = $_[ 0 ];
	my( $optLinesRef ) = $_[ 1 ];
	my( $oldbase ) = $_[ 2 ];
	my( $newbase ) = $_[ 3 ];
	my( $programmer ) = $_[ 4 ];
	my( $date ) = $_[ 5 ];
	my( $primaryFunction ) = $_[ 6 ];
	my( $secondaryFunction ) = $_[ 7 ];
	my( $reference ) = $_[ 8 ];
	my( $stanza ) = $_[ 9 ];

	my( @filteredOutput ) = ();

	my( $tcn ) = '';
	my( $primFunc ) = '';
	my( $filePositionMarker ) = 0;

	my( @localOutput );
	@localOutput = @$ftcxOutputRef;

	my( $line );
	foreach $line ( @localOutput )
	{
		chomp( $line );
		$line =~ s/\s+$//;

		if ($line =~ /^! ftcx_dtp/)
		{
			push(@$optLinesRef, $line);
		}

		elsif ($line =~ /^! opt variations/)
		{
			push(@$optLinesRef, $line);
			next;
		}

		elsif ($line =~ /^!#+$/)
		{
			next;
		}

		elsif ($line =~ /^! %/)
		{
			next;
		}

		elsif ($line =~ /^! SCCS/)
		{
			next;
		}

		elsif (($line =~ /^! Checkin Date/)  ||
				($line =~ /^! Extract Date/))
		{
			next;
		}

		elsif (($line =~ /^! \*+$/)  &&
				($filePositionMarker == 0))
		{
			$filePositionMarker = 1;
			next;
		}

		elsif ($line =~ /^! \*+$/)
		{
			$line =~ s/ /\*/;
			$line =~ s/$/\*\*/;
		}

		elsif (($line =~ /^!\*  =+$/)  &&
				(($filePositionMarker == 1)  ||
				 ($filePositionMarker == 3)))
		{
			$filePositionMarker++;	# 2/4
			$line =~ s/ =/==/;
			$line =~ s/$/==/;
		}

		elsif (($line =~ /^!\*  =+$/)  &&
				($filePositionMarker == 2))
		{
			$filePositionMarker = 3;
			next;
		}

		elsif ($line =~ /^!\* =+$/)
		{
			$line =~ s/$/==/;

			if (($line =~ /^!\* =+$/)  &&
				($filePositionMarker == 6))
			{
				my( $prevLine ) = pop( @filteredOutput );
				push(@filteredOutput, $prevLine);

				if ($prevLine ne '!*')
				{
					push(@filteredOutput, '!*');
				}
			}
		}

		elsif ($line =~ /TEST CASE TITLE/)
		{
			( $tcn ) = ($line =~ /TEST CASE TITLE\s+:\s+(\S.*)$/);

			$line =~ s/TITLE/NAME /;
			$line =~ s/(TEST CASE NAME\s+:)\s+\S.*$/$1/;
			$line .= " $newbase";
		}

		elsif ($line =~ /PROGRAMMER/)
		{
			my( $prog ) = '';
			( $prog ) = ($line =~ /PROGRAMMER\s+:\s+(\S.*)$/);

			if (( !defined( $tcn ) )  ||
				($tcn eq ''))
			{
				$tcn = $oldbase;
			}

			$line =~ s/(PROGRAMMER\s+:)\s+\S.*$/$1/;
			$line .= " $programmer (derived from $tcn";
			push(@filteredOutput, $line);

			$line =~ s/PROGRAMMER(\s+):\s\S.*$/          $1  /;
			$line .= "by $prog)";
		}

		elsif ($line =~ /DATE/)
		{
			my( $origDate ) = '';
			( $origDate ) = ($line =~ /DATE\s+:\s+(\S.*)$/);

			$line =~ s/(DATE\s+:)\s+\S.*$/$1/;
			$line .= " $date";

			if ($origDate ne '')
			{
				$line .= " (original: $origDate)";
			}
		}

		elsif ($line =~ /PRIMARY FUNCTIONS TESTED/)
		{
			( $primFunc ) =
				($line =~ /PRIMARY FUNCTIONS TESTED\s+:\s+(\S.*)$/);

			$line =~ s/(PRIMARY FUNCTIONS TESTED\s+:)\s+\S.*$/$1/;
			$line .= " $primaryFunction";

			$filePositionMarker = 4;
		}

		elsif (($line =~ /^!\*\s+:$/)  &&
				($filePositionMarker == 4))
		{
			$filePositionMarker = 5;
			next;
		}

		elsif ($line =~ /SECONDARY FUNCTIONS TESTED/)
		{
			if (( !defined( $primFunc ) )  ||
				($primFunc eq ''))
			{
				$primFunc = $secondaryFunction;
			}

			$line =~ s/(SECONDARY FUNCTIONS TESTED\s+:)\s+\S.*$/$1/;
			$line .= " $primFunc";
			push(@filteredOutput, $line);

			$line =~ s/SECONDARY FUNCTIONS TESTED/REFERENCE                 /;
			$line =~ s/: .*$/: Feature Number $reference/;
			$line =~ s/(\..*)$/\($1\)/;
		}

		elsif ($line =~ /DRIVER STANZA/)
		{
			my( $origStanza ) = '';
			( $origStanza ) = ($line =~ /DRIVER STANZA\s+:\s+(\S.*)$/);

			$line =~ s/(DRIVER STANZA\s+:)\s+\S.*$/$1/;
			$line .= " $stanza";

			if ($origStanza ne '')
			{
				$line .= " (original: $origStanza)";
			}
		}

		elsif ($line =~ /DESCRIPTION/)
		{
			$filePositionMarker = 6;
		}

		elsif ($filePositionMarker == 6)
		{
			$line =~ s/^(!\*  )\s+/$1/;

			if ($line =~ /KEYWORD\(S\)/)
			{
				my( $prevLine ) = pop( @filteredOutput );
				push(@filteredOutput, $prevLine);

				if ($prevLine ne '!*')
				{
					push(@filteredOutput, '!*');
				}
			}

			elsif ($line =~ /^!2345/)
			{
				$filePositionMarker = 7;

				my( $prevLine ) = pop( @filteredOutput );
				if ($prevLine !~ /^!\* =+$/)
				{
					push(@filteredOutput, $prevLine);
				}
			}
		}

		elsif ($filePositionMarker == 7)
		{
			$line =~ s/$oldbase/$newbase/g;
		}

		push(@filteredOutput, $line);
	}

	return( @filteredOutput );
}


#
#  ParseOpts() - To Parse the Options that "ftcx_dtp" was executed with,
#  and the Alternate Options that might be used with the given Test Case.
#
#  Returns:  An array with the possible variations for each Option (where
#  variations will be of the form "-qopt:-qnoopt" -- or vice versa).
#
sub
ParseOpts($)
{
	my( $optRef ) = $_[ 0 ];

	my( $runOpts ) = $$optRef[ 0 ];
	$runOpts =~ s#^[^\-]*(-[^/]*)/.*$#$1#;
	$runOpts =~ s/\s+$//;

	my( @run ) = ();
	@run = sort( split(/\s+/, $runOpts) );

	my( $altOpts ) = $$optRef[ 1 ];
	$altOpts =~ s/^[^\-]*(-.*)$/$1/;
	$altOpts =~ s/\s+$//;

	my( @opts ) = ();
	@opts = sort( split(/\s+/, $altOpts) );

	$altOpts = join(' ', @opts);
	$altOpts =~ s/(-qreuse=\S+)\s/$1:/g;
	@opts = split(/\s+/, $altOpts);

	my( $i );
	for($i = 0; $i < @opts; $i++)
	{
		my( $o ) = $opts[ $i ];
		if ($o =~ /^-qreuse/)
		{
			my( $qr );
			$qr = join(' ', @run);

			$qr =~ s/(-qreuse=\S+)\s/$1:/g;
			$qr =~ s/^.* (-qreuse=\S+).*$/$1/;

			$qr = "$o:$qr";
			my( @qro ) = sort( split(/:/, $qr) );
			
			$qr = '';
			my( $r );

			foreach $r ( @qro )
			{
				if ($qr !~ /$r/)
				{
					$qr .= ":$r";
				}
			}

			$qr =~ s/^://;

			if (($qr =~ /base/)  &&
				($qr =~ /self/)  &&
				($qr !~ /all/))  
			{
				$qr = "-qreuse=all:$qr";
			}

			elsif ($qr =~ /all/)
			{
				if ($qr !~ /base/)
				{
					$qr =~ s/(-qreuse=)(all:)/$1$2$1base:/;
				}

				if ($qr !~ /self/)
				{
					$qr =~ s/(-qreuse=)(all:)/$1$2$1base:/;
					$qr = "$qr:-qreuse=self";
				}
			}

			if ($qr !~ /none/)
			{
				$qr = "$qr:-qreuse=none";
			}

			@qro = sort( split(/:/, $qr) );
			$o = join(':', @qro);

			$opts[ $i ] = '';
		}

		elsif ($o =~ /^-qno/)
		{
			$o =~ s/^(-q)no/:$1/;
		}

		else
		{
			$o =~ s/^(-q)/:$1no/;
		}

		if ($o ne '')
		{
			$opts[ $i ] .= $o;
		}
	}

	my( $ro );
	foreach $ro ( @run )
	{
		my( @found ) = ();
		@found = grep(/$ro/, @opts);
		if (@found < 1)
		{
			print STDERR "Missed \"$ro\"!\n";
			print STDERR "    \@run = \"@run\"\n";
			print STDERR "    \@opts = \"@opts\"\n";
			print STDERR "    \$altOpts = \"$altOpts\"\n";
		}
	}

	return($runOpts, @opts);
}


#
#  BuildOptList() - To Build a list of all possible Option Sets for
#  "ftcx_dtp".
#
#  Returns:  An array with all of the possible Option Sets for "ftcx_dtp".
#
sub
BuildOptList(\@)
{
	my( $optVar ) = shift( @_ );
	my( @opts ) = @_;

	my( @options ) = ();
	@options = split(/:/, $optVar);

	my( @optList );
	@optList = @options;
	if (@opts > 0)
	{
		@optList = ();

		my( @list ) = ();
		@list = &BuildOptList( @opts );

		my( $opt );
		foreach $opt ( @options )
		{
			my( $ol );
			foreach $ol ( @list )
			{
				my( $optArgs ) = join(' ', $opt, $ol);
				push(@optList, $optArgs);
			}
		}
	}

	return( @optList );
}


#
#  BuildFileSuffix() - To reduce the given set of "ftcx_dtp" Run Options
#  to a consistent Test Case Suffix:
#
#  -qno options are omitted,
#  -qreuse=none is omitted,
#  -qX => X,
#  -qck => c (k is possible with the previous rule),
#  -qd...XX => _dXX,
#  -qreuse=Xxxx => _rX
#
#  Returns:  The resulting values from the above translations in sorted
#  order as a scalar value (with no embedded whitespace).  If the result
#  is Null, "ext" is returned.
#
sub
BuildFileSuffix($)
{
	my( $opts ) = $_[ 0 ];

	$opts =~ s/-qno\S+//g;
	$opts =~ s/-qreuse=none//g;

	$opts =~ s/-qck/-qc/g;

	$opts =~ s/default/_d/g;
	$opts =~ s/deferred/_d/g;

	$opts =~ s/-qreuse=([asbn])[a-z]+/-q_r$1/g;

	$opts =~ s/-q//g;
	$opts =~ s/\s+/ /g;

	my( @optList ) = ();
	@optList = split(/\s+/, $opts);

	my( @underBarOpts ) = ();
	@underBarOpts = sort( grep(/^_/, @optList) );

	my( @nonUnderBarOpts ) = ();
	@nonUnderBarOpts = sort( grep(!/^_/, @optList) );

	$opts = join('', @nonUnderBarOpts, @underBarOpts);
	if ($opts eq '')
	{
		$opts = 'ext';
	}

	return( $opts );
}


#
#  DumpFile() - To write to the File Name passed as the first argument
#  the contents of the array passed as the second argument.
#
#  Returns:  None on Success.  Terminates script on Failure.
#
sub
DumpFile($\@)
{
	my( $name ) = shift( @_ );

	if ( !open(F_SOURCE_FILE, ">$name") )
	{
		print STDERR "open: $!\n";
		print STDERR "    File = \"$name\"\n";
		exit( 1 );
	}

	my( $line );
	foreach $line ( @_ )
	{
		print F_SOURCE_FILE "$line\n";
	}

	close( F_SOURCE_FILE );

	return;
}


#
#  GenericFile() - To convert the given Test Case .f File into a Generic
#  format that will permit trivial comparisons with other Generic Test
#  Case .f files.
#
#  Returns:  None on Success.  Terminates script on Failure.
#
sub
GenericFile($$)
{
	my( $file ) = $_[ 0 ];
	my( $base ) = $_[ 1 ];

	if ( !open(F_FILE, "<$file") )
	{
		print STDERR "open: $!\n";
		print STDERR "    File = \"$file\"\n";
		exit( 1 );
	}

	my( $genFile ) = "${file}.gen";
	if ( !open(GEN_FILE, ">$genFile") )
	{
		print STDERR "open: $!\n";
		print STDERR "    File = \"$genFile\"\n";
		exit( 1 );
	}

	my( $fbase ) = basename($file, '.f');
	my( $gbase ) = $fbase;
	$gbase =~ s/(\d)[a-z_]+$/$1/;

	my( $line );
	while ($line = <F_FILE>)
	{
		chomp( $line );
		if ($line !~ /ftcx_dtp/)
		{
			$line =~ s/($base)[a-z_]+/$1/;
			print GEN_FILE "$line\n";
		}
	}

	close( F_FILE );
	close( GEN_FILE );

	return( $genFile );
}


#
#  UniqueFile() - To compare the given Generic File with the list of
#  files.
#
#  Returns:  1 - on Success (ie: a Unique File), 0 otherwise.
#
sub
UniqueFile($\@)
{
	my( $file ) = shift( @_ );
	my( @list ) = @_;

	my( $ioStatus ) = 1;
	my( $diffFile );

	foreach $diffFile ( @list )
	{
		`diff $file $diffFile 1> /dev/null 2>&1`;
		$ioStatus = ($? >> 8);

		if ($ioStatus == 0)
		{
			last;
		}
	}

	return( $ioStatus );
}


#
#  DeleteFiles() - To Delete any files that exist from the given list of
#  files.
#
#  Returns:  None.
#
sub
DeleteFiles(\@)
{
	my( @list ) = @_;

	my( $file );
	foreach $file ( @list )
	{
		if ( -f $file )
		{
			unlink( $file );
		}
	}

	return;
}


my( $programmer ) = '';
my( $primaryFunctionTested ) = '';
my( $secondaryFunctionTested ) = '';
my( $featureName ) = '';
my( $compilerStanza ) = '';
my( $saveVFFileExtensions ) = 0;

#
#  FIXME! Should include a "-h" option to provide help.
#
my( %optList ) = (	'p=s' => \$programmer,
					'1=s' => \$primaryFunctionTested,
					'2=s' => \$secondaryFunctionTested,
					'f=s' => \$featureName,
					's=s' => \$compilerStanza,
					'vf'  => \$saveVFFileExtensions	);

my( $ioStatus ) = GetOptions( %optList );
if ($ioStatus == 0)
{
	exit( 1 );
}

elsif ( ! -f $ARGV[ 0 ] )
{
	print STDERR "No such file or directory!\n";
	print STDERR "    File = \"$ARGV[ 0 ]\"\n";
	exit( 2 );
}


if (($programmer ne '')  &&
	( !defined( $ENV{ FTCX_DTP_HDR } ) ))
{
	my( $init ) = $programmer;

	$init =~ s/(\w)\w+/$1/g;
	$init =~ s/\s+//g;

	$ENV{FTCX_DTP_HDR} = "! $init DTP extension using:";
}


my( $base ) = basename($ARGV[ 0 ], '.f');


my( @dateElements );
@dateElements = localtime( time( ) );

$dateElements[ 5 ] += 1900;
$dateElements[ 4 ]++;

my( $date ) = sprintf("%4.4d-%02.2d-%02.2d", $dateElements[ 5 ],
							$dateElements[ 4 ], $dateElements[ 3 ]);


#
#  Process Original File -- This will allow us to exclude instances where
#  Gaby's tool doesn't perform any Test Case Extensions later in this script.
#
if ( !open(ORIGINAL_TC, "<$ARGV[ 0 ]") )
{
	print STDERR "open: $!\n";
	print STDERR "    file = \"$ARGV[ 0 ]\"\n";
	exit( 1 );
}

my( @ftcxOutput ) = ();
@ftcxOutput = <ORIGINAL_TC>;
close( ORIGINAL_TC );

my( @optLines ) = ();
my( @filteredOutput ) = ();

@filteredOutput =
	&Filter(\@ftcxOutput, \@optLines, $base, "$base",
				$programmer, $date, $primaryFunctionTested,
				$secondaryFunctionTested, $featureName, $compilerStanza);

unshift(@filteredOutput, $ENV{ FTCX_DTP_HDR }, '');

&DumpFile("${base}.f", @filteredOutput);
my( $genFile ) = &GenericFile("${base}.f", $base);

my( @fileList ) = ();
push(@fileList, $genFile);


#
#  Perform baseline Test Case Extension.  Gaby's tool will provide
#  a list of Alternate Options.  We will use these to generate the
#  full list of possible Test Case Extensions (for this Test Case).
#
@ftcxOutput = &ftcx_dtp( $ARGV[ 0 ] );
if (@ftcxOutput == 0)
{
	print STDERR "No Derived Types to parameterize\n";
	print STDERR "    Test Case = \"$ARGV[ 0 ]\"\n";

	&DeleteFiles($genFile, "${base}.f");

	exit( 3 );
}

@optLines = ();
@filteredOutput =
	&Filter(\@ftcxOutput, \@optLines, $base, "${base}kl",
				$programmer, $date, $primaryFunctionTested,
				$secondaryFunctionTested, $featureName, $compilerStanza);


#
#  Build the list of possible Option combinations for Gaby's tool.
#
my( $runOpts ) = '';
my( @altOpts ) = ();

($runOpts, @altOpts) = &ParseOpts( \@optLines );
my( @ftcxOpts ) = &BuildOptList( @altOpts );


#
#  For each possible Option combination, Extend the Test Case,
#
foreach $runOpts ( @ftcxOpts )
{
	@ftcxOutput = &ftcx_dtp($runOpts, $ARGV[ 0 ]);

	my( $fileSuffix ) = &BuildFileSuffix( $runOpts );
	$fileSuffix = "$base$fileSuffix";

	@filteredOutput =
		&Filter(\@ftcxOutput, \@optLines, $base, $fileSuffix,
					$programmer, $date, $primaryFunctionTested,
					$secondaryFunctionTested, $featureName, $compilerStanza);

	if (@filteredOutput == 0)
	{
		next;
	}

	my( $vfFileBase ) = $fileSuffix;

	$fileSuffix .= '.f';
	if ( -f $fileSuffix )
	{
		print STDERR "File Exists!  \"$fileSuffix\"\n";
		exit( 1 );
	}

	&DumpFile($fileSuffix, @filteredOutput);

	#
	#  Produce a Generic Implementation (used below to compare against
	#  existing Test Case Extensions).
	#
	$genFile = &GenericFile($fileSuffix, $base);

	#
	#  Ensure this Test Case Extension has produced a unique file.
	#  If not, delete the file and it's generic counterpart.
	#
	my( $ioStatus ) = &UniqueFile($genFile, @fileList);
	if ($ioStatus == 0)
	{
		unlink( $genFile );
		unlink( $fileSuffix );
	}

	#
	#  For a Unique Test Case Extension, save the .cols and .lines
	#  files (if required), add the Generic Extension to the list
	#  to compare other Extensions with, and report this Extension
	#  to STDOUT.
	#
	else
	{
		if ($saveVFFileExtensions == 1)
		{
			rename('.cols', "${vfFileBase}.cols");
			rename('.lines', "${vfFileBase}.lines");
		}

		push(@fileList, $genFile);
		print STDOUT "$fileSuffix\n";
	}
}


#
#  Cleanup ...
#
#  Delete all Generic Test Case Extension files, as well as any existing
#  .cols/.lines files and the original Test Case .f source file.
#
#push(@fileList, "${base}.f");

@fileList = ();
push(@fileList, '.cols');
push(@fileList, '.lines');

&DeleteFiles( @fileList );


#
#usage: ftcx_dtp <options> { <input file i> -o <output file i> }...
#
#options (first is the default):
#
# -qk | -qnok:  add kind for a derived type with no kind param components
# -ql | -qnol:  add len for a derived type with no len param components
# -qnock | -qck:  add kind for a character component
# -qnodefaultpv | -qdefaultpv:  use default param values for DT components
# -qdeferredlp | -qnodeferredlp:  use deferred type param for DT
# -qreuse=[all|self|base|none]:  reuse param types from self or base DTs
# -qdebug:  prints out debug statements
# -qlnum:  prints out each line's number, and list of vf lines
# -qdts:  prints out derived-types in current scope
#
