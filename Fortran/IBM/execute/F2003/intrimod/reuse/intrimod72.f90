!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90 -qsmp -F:xlf95_r
! %GROUP: intrimod72.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: rm -f *.mod
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : January 13, 2004
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Various OMP Lock routines mixed with
!*                               user threads.
!*
!*  REQUIRED COMPILER OPTIONS  : -qsmp -qfree=f90
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Verify that user defined interface take
!*                               precedence over the pre-defined interface
!*                               (OMP_LIB). This test case should fail with
!*                               the pre-defined OMP_LIB module interface.
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  04/05/99   AF     Various OMP Lock routines mixed withuser threads.
!*                    /a41v71/omp_run/mixed/fxomprm02aa.f
!*  25/02/02   BC     Altered to verify that user defined interface take
!*                    precedence over the pre-defined interface (OMP_LIB)
!*  13/01/04   BC     Modified and reused with INTRINSIC module nature
!*                    Original testcase : omp_run/misc/fxompmisc02.f
!*
!234567890123456789012345678901234567890123456789012345678901234567890
!

module omp_lib

   interface
      subroutine omp_init_lock(lock)
         integer*8 lock
      end subroutine omp_init_lock
      subroutine omp_destroy_lock(lock)
         integer*8 lock
      end subroutine omp_destroy_lock
     subroutine omp_set_lock(lock)
         integer*8 lock
      end subroutine omp_set_lock
      subroutine omp_unset_lock(lock)
         integer*8 lock
      end subroutine omp_unset_lock
   end interface

end module omp_lib


program intrimod72
   use, intrinsic :: f_pthread
   use, non_intrinsic :: omp_lib
   implicit none

   type(f_pthread_t)       thr
   type(f_pthread_attr_t)  at
   integer*8               lock_var
   integer                 count, ret
   common /count_block/    count

   interface
      subroutine test_sub(l)
         integer*8 l
      end subroutine test_sub
   end interface

   count = 0
   call omp_init_lock(lock_var)
   ret = f_pthread_attr_init(at)
   ret = f_pthread_attr_setdetachstate(at,PTHREAD_CREATE_UNDETACHED)

   ret = f_pthread_create( thr, at, FLAG_DEFAULT, test_sub, lock_var)
   if ( ret .ne. 0 ) then
      stop 9
   end if

!$omp parallel sections, shared(count)
   !$omp section
      call omp_set_lock(lock_var)
      !$OMP FLUSH(count)
      count = count + 1
      !$OMP FLUSH(count)
      call omp_unset_lock(lock_var)
   !$omp section
      call omp_set_lock(lock_var)
      !$OMP FLUSH(count)
      count = count + 7
      !$OMP FLUSH(count)
      call omp_unset_lock(lock_var)
   !$omp section
      call omp_set_lock(lock_var)
      !$OMP FLUSH(count)
      count = count + 4
      !$OMP FLUSH(count)
      call omp_unset_lock(lock_var)
!$omp end parallel sections

   ret = f_pthread_join(thr)
   call omp_destroy_lock(lock_var)
   if ( count .ne. 22 ) then
      stop 1
   end if

end program

subroutine test_sub(lock)
   use, non_intrinsic :: omp_lib

   integer*8               lock

   integer                 count
   common /count_block/    count

   call omp_set_lock(lock)
   !$OMP FLUSH(count)
   count = count + 2
   !$OMP FLUSH(count)
   call omp_unset_lock(lock)

   !$omp parallel sections, shared(count)
      !$omp section
         call omp_set_lock(lock)
         !$OMP FLUSH(count)
         count = count + 3
         !$OMP FLUSH(count)
         call omp_unset_lock(lock)
      !$omp section
         call omp_set_lock(lock)
         !$OMP FLUSH(count)
         count = count + 5
         !$OMP FLUSH(count)
         call omp_unset_lock(lock)
   !$omp end parallel sections

end subroutine test_sub
