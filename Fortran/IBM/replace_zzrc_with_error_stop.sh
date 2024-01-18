find . -name "*.[fF]*" -exec \
	sed --in-place \
	    -e '/CALL\>/ { s/CALL\s*zzrc\s*(\([0-9][0-9_]*\)\s*)/ERROR STOP \1/gi }' \
	    -e 's/CALL\s*zzrc\s*(\([0-9][0-9_]*\)\s*)/error stop \1/gi' \
	    {} \;
