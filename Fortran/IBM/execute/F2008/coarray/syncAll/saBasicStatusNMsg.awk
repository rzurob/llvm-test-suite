BEGIN       { cb=ca=0 }   # counts of "Before" and "After" lines
/Before/    {++cb}        # count one more "Before" line
/After/     {++ca}        # count one more "After" line
/^ *[0-9]+/ {images=$1}   # this is the number of images
END         {exit ((cb==ca) && (cb==images) && (NR==(cb+ca+1)))? 0 : 1} # expect one line each of "Before" and "After", per image
