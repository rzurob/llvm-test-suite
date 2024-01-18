vf="$(realpath $1)"
dir="$(dirname $vf)"
base="$(basename $vf vf)"
mv "$vf" "${dir}/${base}reference_output"
