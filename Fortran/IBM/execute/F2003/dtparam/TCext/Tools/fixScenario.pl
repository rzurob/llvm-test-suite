#!/usr/bin/perl
#
#  To perform a standard series of edits on a .scenario file to provide a
#  more consistent look and feel.  User Defined Edits may also be included.
#
#  Usage: $ fixScenario.pl [-l <UserDefinedEdit> [-l ...]] <ScenarioFile>
#
#  -l   User Defined Edit of the form:  "<Pattern>:<Edit>" where the Edit
#       will be performed when the given Pattern is detected for a given
#       Line.  Patterns are Regular Expressions, Edits are of the Perl
#       "s///" command.
#
#       NOTE:  If Pattern matches an existing Pattern, it will over-ride
#              that Pattern.  A list of Default Edits is provided below.
#
#  Along with the Default and User Defined Edits, the following cleanup
#  will be performed:
#
#  o  "-qfree=f90" will be removed from F_COMPILE/XLF2003_COMPILE/*_LINK
#     Commands.
#  o  Obsolete TRUN Environment Variable definitions will be removed if
#     they're not referenced.
#  o  multiple blank lines are eliminated.
#  o  Leading/Trailing blank lines are eliminated.
#
#  The following simple Default Edits are performed:
#
#  Pattern          Edit
#  \$TR_SRC         s/TR_SRC/(tcasedir)/g
#  ^[A-Z_]{8,}\t    s/\t//
#  ^#[A-Z_]{7,}\t   s/\t//
#  ^\t              s/\t= /= /
#  -qfixed -qfixed  s/-qfixed //
#  -qfixed -qfree   s/-qfixed //
#  \.vf             s/=(\s\S+)(\s\S+\.vf)$/=$2$1/
#
#  The following are more complex Default Edits:
#
#  Pattern:  ^#NAME
#  Edit:     s#/aix811/##:s#/lnx81/##:s#/mac81/##:s#/tst\d+/##:s#/tstdev/##
#
#  Pattern:  [^\(]\$\(tcasedir\)[^\.]+\.vf
#  Edit:     s/([^\(])(\$\(tcasedir\)[^\.]+)\.(vf)/$1\$\($2\$\(product\)$3\)/
#
#  Pattern:  $\($\(tcasedir\)[^$]+$\(product\)vf\)
#  Edit:     s/=(\s\S+)(\s$\($\(tcasedir\)[^$]+$\(product\)vf\))$/=$2$1/
#

use File::Basename;
use File::Path;
use Getopt::Long;

sub CleanLine($$);
sub SplitLine($);
sub MassageLine($$$);

#
#  CleanLine() - To Cleanup Whitespace usage on the given Line; leading
#  and trailing whitespace are removed; "#TOKEN =x" is translated to
#  "#TOKEN\t\t= x"; instanaces of "\t[SPACE]" are replaced with "\t";
#  Indentation is added (as required); and instances of "[SPACE][SPACE]"
#  are replaced with "[SPACE]".
#
#  NOTE:  Assumes a 4 "[SPACE]" Tab.
#
#  Returns:  The Clean variation of the given Line.
#
sub
CleanLine($$)
{
	my( $line ) = $_[ 0 ];
	my( $indent ) = $_[ 1 ];

	$line =~ s/\s*=\s*/\t\t= /;

	$line =~ s/^\s+$//;
	$line =~ s/\s+$//;

	$indent = "\t" x $indent;
	$line = "$indent$line";

	$line =~ s/\t +/\t/g;
	$line =~ s/  +/ /g;

	return( $line );
}

#
#  SplitLine() - To Split the given Line into multiple lines that are less
#  than 80 characters is width.  Where a continuation line is required, the
#  current line is terminated with a "\".  Embedded "\t" characters are
#  handled by expansion (assuming a 4 "[SPACE]" Tab) to fill to the
#  appropriate column.  Instances of 4 contiguous "[SPACE]" characters are
#  replaced with "\t" characters afterwards.
#
#  Returns:  The adjusted Line.
#
sub
SplitLine($)
{
	my( $line ) = $_[ 0 ];

	my( $ptr ) = 0;
	my( $newPtr ) = 0;
	my( $spaceLine ) = $line;

	while ($ptr < length( $spaceLine ))
	{
		$newPtr = index($spaceLine, '\t', $ptr);
		if ($newPtr == -1)
		{
			$ptr = length( $spaceLine ) + 1;
		}

		else
		{
			$blanks = 4 - ($newPtr % 4);
			$blanks = ' ' x $blanks;

			substr($spaceLine, $newPtr, 1, $blanks);

			$ptr = $newPtr + 1;
		}
	}

	if ((length( $spaceLine ) > 78)  &&
		($line !~ /\n/))
	{
		my( $lineStart ) = 0;
		my( $lineEnd ) = 72;

		my( $indent ) = index($spaceLine, '=');
		$indent = ' ' x ($indent + 6);
		$indent = "\t\\\n$indent";

		my( $useLength ) = $lineEnd - length( $indent ) + 3;

		my( $newLine ) = '';
		while (($lineStart < length( $spaceLine ))  &&
				($lineEnd < length( $spaceLine )))
		{
			$lineEnd  = rindex($spaceLine, ' ', $lineEnd);
			if ($lineEnd == -1)
			{
				$lineEnd = length( $spaceLine ) + 1;
			}

			else
			{
				$lineEnd -= 1;

				if ($newLine ne '')
				{
					$newLine .= $indent;
				}

				$newLine .= substr($spaceLine, $lineStart,
									($lineEnd - $lineStart + 1));

				$lineStart = $lineEnd + 2;
				$lineEnd  = $lineStart + $useLength;
			}
		}

		$newLine .= $indent;
		$newLine .= substr($spaceLine, $lineStart);

		$newLine =~ s/    /\t/g;

		$line = $newLine;
	}

	return( $line );
}

#
#  MassageLine() - To perform the appropriate edits on the given Line.
#
#  Returns:  The edited Line.
#
sub
MassageLine($$$)
{
	my( $line ) = $_[ 0 ];
	my( $indent ) = $_[ 1 ];
	my( $lineEditsRef ) = $_[ 2 ];

	$line = &CleanLine($line, $indent);

	my( $pattern );
	foreach $pattern ( keys( %$lineEditsRef ) )
	{
		if ($line =~ m#$pattern#)
		{
			my( @editList );
			@editList = split(/:/, ${$lineEditsRef}{ $pattern }); 

			my( $edit );
			foreach $edit ( @editList )
			{
				my( $ed ) = "\$line =~ $edit;";

				my( $ioStatus ) = eval( $ed );
				if ( !defined( $ioStatus ) )
				{
					print STDERR "Line Edit: $@\n";
					print STDERR "    edit = \"$edit\"\n";

					exit( 1 );
				}
			}
		}
	}

	my( $newLine ) = &SplitLine( $line );
	if ($line ne $newLine)
	{
		$line = $newLine;
	}

	return( $line );
}


my( %lineEdits ) =
	(
		'^#NAME' => 
		's#/aix811/##:s#/lnx81/##:s#/mac81/##:s#/tst\d+/##:s#/tstdev/##',

		'\$TR_SRC' => 's/TR_SRC/(tcasedir)/g',

		'^[A-Z_]{8,}\t' =>  's/\t//',
		'^#[A-Z_]{7,}\t' => 's/\t//',

		'^\t' => 's/\t= /= /',

		'-qfixed -qfixed' => 's/-qfixed //',
		'-qfixed -qfree' => 's/-qfixed //',

		'\.vf' => 's/=(\s\S+)(\s\S+\.vf)$/=$2$1/',
		'[^\(]\$\(tcasedir\)[^\.]+\.vf' =>
		's/([^\(])(\$\(tcasedir\)[^\.]+)\.(vf)/$1\$\($2\$\(product\)$3\)/',
		'$\($\(tcasedir\)[^$]+$\(product\)vf\)' =>
		's/=(\s\S+)(\s$\($\(tcasedir\)[^$]+$\(product\)vf\))$/=$2$1/',
	);

my( @edits ) = ();

my( $ioStatus ) = &GetOptions( 'l=s' => \@edits );
if ($ioStatus == 0)
{
	print STDERR "getopt: $!\n";
	exit( 1 );
}


#
#  Add the User Defined Edits to the list of Default Edits.
#
if (@edits > 0)
{
	my( $edit );
	foreach $edit ( @edits )
	{
		my( @items ) = ();
		@items = split(/:/, $edit);

		my( $pattern ) = shift( @items );
		my( $editList ) = join(':', @items);
		$lineEdits{ $pattern } = $editList;
	}
}


#
#  Attempt to Open the file.
#
if ( !open(TSTDEV, "< $ARGV[ 0 ]") )
{
	print STDERR "open: $!\n    File = \"$ARGV[ 0 ]\"\n";
	exit( 1 );
}


#
#  Initialize Context Flags:
#
my( $endEnv ) = 0;
my( $indent ) = 0;
my( $inLink ) = 0;
my( $inCompile ) = 0;
my( $trunEnvUsed ) = 0;
my( $newTestCase ) = '';
my( $trunEnvDefined ) = 0;


#
#  Cycle through the .scenario file.
#
while ($line = <TSTDEV>)
{
	chomp( $line );

	$line = &MassageLine($line, $indent, \%lineEdits);
	if (($line =~ /^TR_SRC/)	||
		($line =~ /^OPTIONS/)	||
		($line =~ /^COMPILER/))
	{
		$trunEnvDefined = 1;
	}

	elsif ($line =~ /SCRIPT_RUN/)
	{
		$trunEnvUsed = 1;
	}

	elsif (($line =~ /^\t*#CMD\t.*= F_COMPILE$/)  ||
			($line =~ /^\t*#CMD\t.*= XLF2003_COMPILE$/))
	{
		$inCompile = 1;
	}

	elsif ($line =~ /^\t*#CMD\t.*= [A-Z_]*_LINK$/)
	{
		$inLink = 1;
	}

	elsif ($line =~ /^\t*#ENDCMD$/)
	{
		$inLink = 0;
		$inCompile = 0;
	}

	elsif ($line =~ / -qfree=f90/)
	{
		if (($inLink == 1)  ||
			($inCompile == 1))
		{
			$line =~ s/ -qfree=f90//g;
			if ($line =~ /\t=$/)
			{
				next;
			}
		}
	}

	elsif ($line =~ /^\t*#IF/)
	{
		$indent++;
	}

	elsif ($line =~ /^\t*#ENDIF/)
	{
		$indent--;
	}

	elsif ($line =~ /^#ENDENV/)
	{
		$endEnv = 1;
	}

	elsif ($line =~ /^$/)
	{
		$endEnv = 0;
	}

	elsif ($line =~ /^#BEGINCONTROL/)
	{
		if ($endEnv == 1)
		{
			$line = "\n$line";
		}

		$endEnv = 0;
	}

	$newTestCase .= "$line\n";
}

close( TSTDEV );

#
#  Weed out obsolete TRUN Environment Variable definitions if they're
#  not referenced.
#
if (($trunEnvDefined == 1)  &&
	($trunEnvUsed == 0))
{
	$newTestCase =~ s/\nCOMPILER\s*=\s*\S+\n/\n/;
	$newTestCase =~ s/\nTR_SRC\s*=\s*[^\n]+\n/\n/;
	$newTestCase =~ s/\nOPTIONS\s*=\s*[^\n]+\n/\n/;
}


#
#  newline handling:
#  o  Eliminate multiple blank lines
#  o  Eliminate leading/trailing blank lines.
#
$newTestCase =~ s/\n\n+/\n\n/g;
$newTestCase =~ s/^\n+//;
$newTestCase =~ s/\n\n+$/\n/;

#
#  Dump adjusted .scenario file to STDOUT and termiante.
#
print STDOUT "$newTestCase";
exit( 0 );
