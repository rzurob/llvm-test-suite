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
!*                               where the procedure is defined in C and
!*                               contiguity checking for copy-in/out is
!*                               required.
!*                               - The actual arg is an optional dummy
!*                                 argument itself.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      use, intrinsic :: iso_c_binding
      implicit none

      interface
      subroutine sub(arg) bind(c)
        import
        integer(c_int), optional :: arg(*)
      end
      end interface

      integer(c_int), pointer :: arr_ptr(:)
      integer(c_int) :: arr(10)
      integer :: i
      arr = [(i,i=1,10,1)]
      allocate(arr_ptr(10))
      arr_ptr = -arr

      call foo(arr_ptr)
      call foo()

      call bar(arr)
      call bar()

      contains

      subroutine foo(arg)
        integer(c_int), optional, pointer :: arg(:)
        if (present(arg)) then
           ! see section 12.5.2.12.
           call sub(arg)
        end if
      end

      subroutine bar(arg)
        integer(c_int), optional :: arg(:)
        call sub(arg)
      end


      end
