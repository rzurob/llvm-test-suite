# In 'BEGIN', we record the line number of the start and end of the block containing the VOLATILE statement.
# After 'BEGIN', we look for three patterns:
# 1. LOC statements, which give the line number for subsequent statements
# 2. STO/LOD/STR/IND statements with "volatile" but referencing the wrong variables, and
# 3. STO/LOD/STR/IND statements referencing the "right" variables
# Occurances of (2) are not allowed - they mean a non-volatile variable has been marked volatile.
# Occurances of (3) have to be checked that they contain "volatile" if they are in the block containing the VOLATILE statement.
# Before exiting on discovering an error, we assign a value to the variable "illegal".
# If "illegal" is still 0 at END, we verify that each of the volatile variables
# we expected to see also appeared.

BEGIN    {
    slineStart=59;
    slineEnd=82;
    illegal=0;
}

# find source line number
/^ +LOC .*line_no=/ {
    sline=$0;
    sub(".*line_no=","",sline);
    sub("[^0-9].*","",sline);
}

# look for illegal uses of instructions marked with volatile
/^ +(STO|LOD|STR|IND) .*[dis]0obvious.*volatile/ {
    varName = substr($0,match($0,"[dis]0obvious"),9);
    print "variable", varName, "should be non-volatile on source line:", sline, ", listing line:", FNR;
    illegal=1;
    exit;
}

# look for expected uses of volatile -- MUST be volatile in block, may NOT be volatile outside the block
/^ +(STO|LOD|STR|IND) .*[dis][12]obvious.*/ {
    varName = substr($0,match($0,"[dis][12]obvious"),9);
    if (sline >= slineStart && sline <= slineEnd) {
	if (index($0,"volatile") == 0) {
	    print "variable", varName, "should be volatile in the block on source line:", sline, ", listing line:", FNR;
	    illegal=2;
	    exit;
	}
    } else {
	if (index($0,"volatile")) {
	    print "variable", varName, "should not be volatile outside the block on source line:", sline, ", listing line:", FNR;
	    illegal=3;
	    exit;
	}
    }
    counts[varName] ++;
}

# look for variables marked with volatile - d2obvious appears only in
# the block, introduced by "volatile" and is thereby implicitly
# declared in the including scope.  Since it is volatile for its
# entire extent, the SYM is marked is_volatile, and the full attribute
# listing will list it.  It should be the *only* such variable.
/^[a-z0-9]+ .* Volatile/ {
    varName = substr($0,1,match($0," "));
    if (varName != "d2obvious") {
	print "Listing line:\n", $0, "\nindicates", varName, "is incorrectly marked volatile in the whole program.\n";
	illegal=4;
	exit;
    }
}

END {
    if (illegal != 0) { exit illegal; }
    if (!counts["i1obvious"]) { exit 5; }
    if (!counts["s1obvious"]) { exit 6; }
    if (!counts["s2obvious"]) { exit 7; }
}
