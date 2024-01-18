!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May 23, 2012
!*
!*  PRIMARY FUNCTIONS TESTED   : C-interop OPTIONAL argument
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Calling a BIND(C) procedure from Fortran
!*                               where the procedure is defined in C.
!*                               - The actual arg is a disassociated POINTER
!*                                 or an unallocated POINTER and the dummy
!*                                 argument is not allocatable or pointer
!*                                 and -qxlf2008=checkpresence is enabled.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

@process xlf2008(checkpresence)
      use, intrinsic :: iso_c_binding
      implicit none

      interface
         subroutine c_func(arg1, arg2) bind(C)
           import
           integer(c_int), optional :: arg1
           real(c_double), optional :: arg2
         end
      end interface

      integer(c_int), pointer :: ii_ptr
      integer(c_int), target :: ii_targ
      real(c_double), pointer :: rr_ptr
      real(c_double), target :: rr_targ
      integer(c_int), allocatable :: ii_al
      real(c_double), allocatable :: rr_al

      nullify(ii_ptr)
      nullify(rr_ptr)

      call c_func(ii_ptr, rr_ptr)

      ii_ptr => ii_targ
      ii_targ = -20
      call c_func(ii_ptr)

      rr_ptr => rr_targ
      rr_targ = -3.14
      call c_func(ii_ptr, rr_ptr)

      nullify(rr_ptr)
      call c_func(ii_ptr, rr_ptr)

      rr_ptr => rr_targ
      rr_targ = -rr_targ
      call c_func(arg2=rr_ptr)

      nullify(ii_ptr)
      call c_func(ii_ptr, rr_ptr)

      call c_func(ii_ptr)

      allocate(ii_al)
      allocate(rr_al)
      ii_al = 5
      rr_al = 3.14
      call c_func(ii_al, rr_al)

      deallocate(ii_al)
      call c_func(ii_al, rr_al)

      deallocate(rr_al)
      call c_func(ii_al, rr_al)

      end
