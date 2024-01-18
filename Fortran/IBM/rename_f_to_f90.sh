
tcasedir="$(dirname $(realpath $1))"
scenario="${tcasedir}/$(basename $1)"
MY_TMP="$HOME/tmp/split"

rm -rf $MY_TMP
mkdir -p $MY_TMP
cd $MY_TMP

# Split scenario file into files containing only one F_COMP section each.
sed  -e '/#CMD.*_COMP.*$/,/#ENDCMD/!d' "$scenario" | csplit -f splitfile_ --suppress-matched --elide-empty-files - '/#ENDCMD/' '{*}'


for i in `ls splitfile_*`
do
  grep -q '(OPT_FIXED_SOURCE_FORM' "$i"
  has_qfixed=$?
  if [ $has_qfixed -ne 0 ]; then
    # Rename *.[fF] source files to have the .f90 extension
    inputs="$(sed -n -e '/#INPUT\s*=/ { s/#INPUT\s*=\(.*\)/\1/ ;  /[^ ]*\.[fF]\>/ !d ; s/\$(tcasedir)// ; p }' "$i")"
    if [ ! -z "$inputs" ]; then
      for f in $inputs
      do
        case "$f" in
          *.[Ff])
            mv "${tcasedir}/${f}" "${tcasedir}/${f}90"
            ;;

          *)
            # skip
            ;;
        esac
      done
    fi
  fi
done
