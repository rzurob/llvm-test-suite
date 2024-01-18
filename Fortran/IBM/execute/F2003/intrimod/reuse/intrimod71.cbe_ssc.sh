# fxnomp_lib001.presh
# Description:
#  Grep the output from -qlist to check that the omp_lib module is functional.
#
#  Purpose: Compile with default -qintsize and verify that the omp_lib module is working properly.
NAME=$1

echo $OPTIONS | grep "\-q64"
if test "$?" -eq 0
then

   grep -c __xlf_omp_init_lock          a.lst >  $NAME.out
   grep -c __xlf_omp_init_nest_lock     a.lst >> $NAME.out
   grep -c __xlf_omp_set_dynamic_i4     a.lst >> $NAME.out
   grep -c __xlf_omp_get_dynamic_i8     a.lst >> $NAME.out
   grep -c __xlf_omp_get_max_threads_i8 a.lst >> $NAME.out
   grep -c __xlf_omp_set_nested_i4      a.lst >> $NAME.out
   grep -c __xlf_omp_get_nested_i8      a.lst >> $NAME.out
   grep -c __xlf_omp_get_num_procs_i8   a.lst >> $NAME.out
   grep -c __xlf_omp_in_parallel_i8     a.lst >> $NAME.out
   grep -c __xlf_omp_set_num_threads_i4 a.lst >> $NAME.out
   grep -c __xlf_omp_get_wtick_r8       a.lst >> $NAME.out
   grep -c __xlf_omp_get_wtime_r8       a.lst >> $NAME.out
   grep -c __xlf_omp_set_lock           a.lst >> $NAME.out
   grep -c __xlf_omp_set_nest_lock      a.lst >> $NAME.out
   grep -c __xlf_omp_test_lock_i8       a.lst >> $NAME.out
   grep -c __xlf_omp_test_nest_lock_i8  a.lst >> $NAME.out
   grep -c __xlf_omp_unset_lock         a.lst >> $NAME.out
   grep -c __xlf_omp_unset_nest_lock    a.lst >> $NAME.out
   grep -c __xlf_omp_get_thread_num_i8  a.lst >> $NAME.out
   grep -c __xlf_omp_get_num_threads_i8 a.lst >> $NAME.out
   grep -c __xlf_omp_destroy_lock       a.lst >> $NAME.out
   grep -c __xlf_omp_destroy_nest_lock  a.lst >> $NAME.out
else
   grep -c __xlf_omp_init_lock          a.lst >  $NAME.out
   grep -c __xlf_omp_init_nest_lock     a.lst >> $NAME.out
   grep -c __xlf_omp_set_dynamic_i4     a.lst >> $NAME.out
   grep -c __xlf_omp_get_dynamic_i4     a.lst >> $NAME.out
   grep -c __xlf_omp_get_max_threads_i4 a.lst >> $NAME.out
   grep -c __xlf_omp_set_nested_i4      a.lst >> $NAME.out
   grep -c __xlf_omp_get_nested_i4      a.lst >> $NAME.out
   grep -c __xlf_omp_get_num_procs_i4   a.lst >> $NAME.out
   grep -c __xlf_omp_in_parallel_i4     a.lst >> $NAME.out
   grep -c __xlf_omp_set_num_threads_i4 a.lst >> $NAME.out
   grep -c __xlf_omp_get_wtick_r8       a.lst >> $NAME.out
   grep -c __xlf_omp_get_wtime_r8       a.lst >> $NAME.out
   grep -c __xlf_omp_set_lock           a.lst >> $NAME.out
   grep -c __xlf_omp_set_nest_lock      a.lst >> $NAME.out
   grep -c __xlf_omp_test_lock_i4       a.lst >> $NAME.out
   grep -c __xlf_omp_test_nest_lock_i4  a.lst >> $NAME.out
   grep -c __xlf_omp_unset_lock         a.lst >> $NAME.out
   grep -c __xlf_omp_unset_nest_lock    a.lst >> $NAME.out
   grep -c __xlf_omp_get_thread_num_i4  a.lst >> $NAME.out
   grep -c __xlf_omp_get_num_threads_i4 a.lst >> $NAME.out
   grep -c __xlf_omp_destroy_lock       a.lst >> $NAME.out
   grep -c __xlf_omp_destroy_nest_lock  a.lst >> $NAME.out
fi

diff $NAME.out $TR_SRC/$NAME.vf


if test "$?" -eq 0
then
   echo "Compilation Successful."
else
   echo "ERROR: The omp_lib module is not functional."
   exit 7
fi
if [ "$TRUN_SAVE" != "yes" ]
then
        rm -f ./a.lst
        rm -f ./$NAME.err
        rm -f ./$NAME.o
        rm -f ./$NAME.out
        rm -f ./fake_omp_lib_module.lst
        rm -f ./omp_lib.mod
fi

 
 
