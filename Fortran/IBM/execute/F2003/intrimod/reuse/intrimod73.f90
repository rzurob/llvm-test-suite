!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90 -qnoswapomp 
! %GROUP: ../fake_omp_lib_module.f intrimod73.f
! %VERIFY: intrimod73.out:intrimod73.vf
! %STDIN:
! %STDOUT: intrimod73.out
! %EXECARGS:
! %POSTCMD: 
! %END
!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  TEST CASE TITLE            : intrimod73.f
!*
!*  PROGRAMMER                 : Bahram Chehrazy
!*  DATE                       : January 14, 2004
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : INTRINSIC/NON_INTRINSIC module nature.
!*
!*  REQUIRED COMPILER OPTIONS  : -qsmp -qfree=f90
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Test OpenMP run-time routines & Lock routines 
!*                             : with -qnoswapomp and INTRINSIC module nature.
!*
!* ===================================================================
!*  
!*  REVISION HISTORY
!*  
!*  MM/DD/YY:  Init:  Comments:
!*  02/25/02   BC     Initial Version
!*  11/18/02   BC     Modified to run successfully with both 32-bit and 64-bit
!*                    processes (defect 232225)
!*  14/01/04   BC     Modified and reused with INTRINSIC module nature
!*                    Original testcase : omp_run/misc/fxompmisc24.f
!*      
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
!


program intrimod73
   use, intrinsic :: omp_lib
   implicit none
   integer(kind=OMP_LOCK_KIND) lock
   integer*8 nest_lock

! Testing OpenMP run-time routines

   call OMP_SET_NUM_THREADS(10)
   call OMP_SET_DYNAMIC(.true.)
   call OMP_SET_NESTED(.true.)

   if ( OMP_GET_NUM_THREADS() .ne. 911) then
       error stop 21
   endif

   if ( OMP_GET_MAX_THREADS() .ne. 747) then
       error stop 22
   endif

   if ( OMP_GET_THREAD_NUM() .ne. 777) then
       error stop 23
   endif

   if ( OMP_GET_NUM_PROCS() .ne. 888) then
       error stop 24
   endif

   if ( .not. OMP_IN_PARALLEL() ) then
       error stop 25
   endif

   if ( OMP_GET_DYNAMIC() ) then
       error stop 26
   endif

   if ( .not. OMP_GET_NESTED() ) then
       error stop 27
   endif

! Testing OpenMP simple luck routines
   lock = 511

   call OMP_INIT_LOCK(lock)
   call OMP_SET_LOCK(lock)
   call OMP_UNSET_LOCK(lock)
   call OMP_DESTROY_LOCK(lock)

   if(  OMP_TEST_LOCK(lock) ) then
       error stop 28
   endif

! Testing OpenMP nestable luck routines
   nest_lock = 611

   call OMP_INIT_NEST_LOCK(nest_lock)
   call OMP_SET_NEST_LOCK(nest_lock)
   call OMP_UNSET_NEST_LOCK(nest_lock)
   call OMP_DESTROY_NEST_LOCK(nest_lock)

   if( OMP_TEST_NEST_LOCK(nest_lock) .ne. 811 ) then
       error stop 29
   endif


end program 


subroutine OMP_SET_NUM_THREADS(nthread)
   integer nthread
   print *,"This is OMP_SET_NUM_THREADS called with num_thread = ", nthread   
end subroutine OMP_SET_NUM_THREADS

subroutine OMP_SET_DYNAMIC(lvar)
   logical lvar
   print *,"This is OMP_SET_DYNAMIC called with value = ", lvar
end subroutine OMP_SET_DYNAMIC

subroutine OMP_SET_NESTED(lvar)
   logical lvar
   print *,"This is OMP_SET_NESTED called with value = ", lvar
end subroutine OMP_SET_NESTED

function OMP_GET_NUM_THREADS()
   integer omp_get_num_threads
   print *,"This is OMP_GET_NUM_THREADS"
   omp_get_num_threads = 911 
end function OMP_GET_NUM_THREADS

function OMP_GET_MAX_THREADS()
   integer omp_get_max_threads
   print *,"This is OMP_GET_MAX_THREADS"
   omp_get_max_threads = 747 
end function OMP_GET_MAX_THREADS

function OMP_GET_THREAD_NUM()
   integer omp_get_thread_num
   print *,"This is OMP_GET_THREAD_NUM"
   omp_get_thread_num = 777 
end function OMP_GET_THREAD_NUM

function OMP_GET_NUM_PROCS()
   integer omp_get_num_procs
   print *,"This is OMP_GET_NUM_PROCS"
   omp_get_num_procs = 888 
end function OMP_GET_NUM_PROCS

function OMP_IN_PARALLEL()
   logical omp_in_parallel
   print *,"This is OMP_IN_PARALLEL"
   omp_in_parallel = .true. 
end function OMP_IN_PARALLEL

function OMP_GET_DYNAMIC()
   logical omp_get_dynamic
   print *,"This is OMP_GET_DYNAMIC"
   omp_get_dynamic = .false. 
end function OMP_GET_DYNAMIC

function OMP_GET_NESTED()
   logical omp_get_nested
   print *,"This is OMP_GET_NESTED"
   omp_get_nested = .true. 
end function OMP_GET_NESTED

subroutine OMP_INIT_LOCK(svar)
   use, intrinsic :: omp_lib,only: OMP_LOCK_KIND 
   integer(kind=OMP_LOCK_KIND) svar
   print *, "This is OMP_INIT_LOCK called with value = ", svar
end subroutine OMP_INIT_LOCK

subroutine OMP_SET_LOCK(svar)
   use, intrinsic :: omp_lib,only: OMP_LOCK_KIND 
   integer(kind=OMP_LOCK_KIND) svar
   print *, "This is OMP_SET_LOCK called with value = ", svar
end subroutine OMP_SET_LOCK

subroutine OMP_UNSET_LOCK(svar)
   use, intrinsic :: omp_lib,only: OMP_LOCK_KIND 
   integer(kind=OMP_LOCK_KIND) svar
   print *, "This is OMP_UNSET_LOCK called with value = ", svar
end subroutine OMP_UNSET_LOCK

subroutine OMP_DESTROY_LOCK(svar)
   use, intrinsic :: omp_lib,only: OMP_LOCK_KIND 
   integer(kind=OMP_LOCK_KIND) svar
   print *, "This is OMP_DESTROY_LOCK called with value = ", svar
end subroutine OMP_DESTROY_LOCK

function OMP_TEST_LOCK(svar)
   use, intrinsic :: omp_lib,only: OMP_LOCK_KIND 
   integer(kind=OMP_LOCK_KIND) svar
   logical omp_test_lock 
   print *, "This is OMP_TEST_LOCK called with value = ", svar
   omp_test_lock = .false.
end function OMP_TEST_LOCK

subroutine OMP_INIT_NEST_LOCK(nvar)
   integer*8 nvar
   print *, "This is OMP_INIT_NEST_LOCK called with value = ", nvar
end subroutine OMP_INIT_NEST_LOCK

subroutine OMP_SET_NEST_LOCK(nvar)
   integer*8 nvar
   print *, "This is OMP_SET_NEST_LOCK called with value = ", nvar
end subroutine OMP_SET_NEST_LOCK

subroutine OMP_UNSET_NEST_LOCK(nvar)
   integer*8 nvar
   print *, "This is OMP_UNSET_NEST_LOCK called with value = ", nvar
end subroutine OMP_UNSET_NEST_LOCK

subroutine OMP_DESTROY_NEST_LOCK(nvar)
   integer*8 nvar
   print *, "This is OMP_DESTROY_NEST_LOCK called with value = ", nvar
end subroutine OMP_DESTROY_NEST_LOCK

function OMP_TEST_NEST_LOCK(nvar)
   integer*8 nvar
   integer OMP_TEST_NEST_LOCK
   print *, "This is OMP_TEST_NEST_LOCK called with value = ", nvar
   OMP_TEST_NEST_LOCK = 811
end function OMP_TEST_NEST_LOCK
