for i in `cat lit.txt`
do
  filename=`basename $i`
  filename_noext=`basename $filename .scenario`
  pathname=${i%/$filename}

  mkdir -p compile/$pathname
  mv execute/$pathname/${filename_noext}.* compile/$pathname/
done
