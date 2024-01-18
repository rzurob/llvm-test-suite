! ************************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 -qlist
! %GROUP: ../fake_omp_lib_module.f intrimod71.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/intrimod71.sh intrimod71
! %END
!************************************************************************
!************************************************************************
!*
!*  LRP FORTRAN TEST CASE            IBM INTERNAL USE ONLY
!*
!*  Test Case Title  : OMP_LIB Module Unit test 
!*
!*  Test Case Name   : intrimod71.f
!*
!*  Release Version  : 1.0
!*
!*  Created By       : Bahram Chehrazy
!*
!*  Create Date      : 01/13/04
!*
!*  Modified By      :
!*
!*  Description      : Verify the omp_lib module is working properly  
!*                   : with INTRINSIC module nature
!*                   : Reused from omp_run/unit/fxnomp_lib001.f
!*
!*************************************************************************
program  omp_interface
     use, intrinsic :: omp_lib

     integer (kind = omp_lock_kind) slck
     integer (kind = omp_nest_lock_kind) nlck
     integer, parameter :: num = 5
#ifdef bg4thd
     integer, parameter :: NTHR = 4
#else
     integer, parameter :: NTHR = 10
#endif
     call omp_init_lock (slck)
     call omp_init_nest_lock(nlck)
     call omp_set_dynamic(.TRUE.)
     print *, omp_get_dynamic() 
     print *, omp_get_max_threads()
     call omp_set_nested(.TRUE.)
     print *, omp_get_nested()
     print *, omp_get_num_procs()
     print *, omp_in_parallel()
     call omp_set_num_threads(NTHR) 
     print *, omp_get_wtick()
     print *, omp_get_wtime()
     call omp_set_lock (slck)
     call omp_set_nest_lock(nlck)
     print *, omp_test_lock(slck)
     print *, omp_test_nest_lock(nlck)
     call omp_unset_lock (slck)
     call omp_unset_nest_lock(nlck)
     print *, omp_get_thread_num()
     print *, omp_get_num_threads()
     call omp_destroy_lock(slck)
     call omp_destroy_nest_lock(nlck)
end program omp_interface
      
