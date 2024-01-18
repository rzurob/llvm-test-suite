!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May 23, 2012
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : C-interop OPTIONAL argument
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Calling a BIND(C) procedure from Fortran
!*                               where the procedure is defined in C and
!*                               contiguity checking for copy-in/out is
!*                               required.
!*                               - -qxlf2008=checkpresence is enabled and
!*                                 The actual arg is a pointer/allocatable
!*
!234567890123456789012345678901234567890123456789012345678901234567890

@process xlf2008(checkpresence)

      use, intrinsic :: iso_c_binding
      implicit none

      interface
      subroutine sub(arg, b) bind(c)
        import c_int,  c_float
        integer(c_int), optional :: arg(10)
        real(c_float),    optional :: b(10)
      end
      end interface

      integer, pointer :: arr(:), i
      real, pointer :: bb(:)
      allocate(arr(10), bb(10))
      arr = [(i,i=1,10,1)]
      bb = [(1.1*i, i=1, 10, 1)]

      call sub(arr)
      call sub()
      call sub(arr, bb)

      deallocate(bb)
      call sub(arr, bb)

      allocate(bb(10))
      bb = [(1.1*i, i=1, 10, 1)]
      deallocate(arr)
      call sub(arr, bb)

      deallocate(bb)
      call sub(arr, bb)

      end


