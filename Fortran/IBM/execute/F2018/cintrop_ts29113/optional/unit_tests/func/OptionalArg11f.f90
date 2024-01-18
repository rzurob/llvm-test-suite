!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : OptionalArg11f
!*
!*  PROGRAMMER                 : Bardia Mahjour
!*  DATE                       : May 23, 2012
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : C-interop OPTIONAL argument
!*                                                   
!*  SECONDARY FUNCTIONS TESTED : None 
!*
!*  DRIVER STANZA              : xlf2008
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*
!*  DESCRIPTION                : Calling a BIND(C) procedure from Fortran
!*                               where the procedure is defined in C and 
!*                               contiguity checking for copy-in/out is
!*                               required.
!*                               - The actual arg is a pointer and the
!*                                 interface contains a by-value argument.
!*                                 (VALUE attribute prevents ASTI from
!*                                  creating stubs).
!*
!234567890123456789012345678901234567890123456789012345678901234567890


      use, intrinsic :: iso_c_binding
      implicit none
      integer, parameter :: ARR_SIZE = 10

      interface
      subroutine sub(arg, b, n) bind(c)
        import c_int, c_float
        integer(c_int), optional :: arg(ARR_SIZE)
        real(c_float),    optional :: b(ARR_SIZE)
        integer(c_int), value :: n
      end
      end interface

      integer(c_int), pointer :: arr(:), i
      real(c_float), pointer :: bb(:)
      allocate(arr(ARR_SIZE), bb(ARR_SIZE))
      arr = [(i,i=1,ARR_SIZE,1)]
      bb = [(1.1*i, i=1, ARR_SIZE, 1)]

      call sub(arr, n=ARR_SIZE)
      call sub(n=ARR_SIZE)
      call sub(arr, bb, ARR_SIZE)
      call sub(b=bb, n=ARR_SIZE)

      end

