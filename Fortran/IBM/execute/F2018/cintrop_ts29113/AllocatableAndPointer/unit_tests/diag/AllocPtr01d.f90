!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March, 2013
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : 26305: C-interop Allocatable/Pointer
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : langlvl testing
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      use, intrinsic :: iso_c_binding
      implicit none

      interface
         subroutine s1(a1, a2, a3) bind(c)
            import
            real(c_float) :: a1
            integer(c_int), allocatable :: a2
            real(c_float), pointer :: a3(:)
         end subroutine
      end interface

      real(c_float) :: r1
      real(c_float), pointer :: r2(:)
      integer(c_int), allocatable :: i1

      call s1(r1, i1, r2)

      end

      subroutine s1(a1, a2, a3) bind(c)
        use, intrinsic :: iso_c_binding
        implicit none
        real(c_float) :: a1
        integer(c_int), allocatable :: a2
        real(c_float), pointer :: a3(:)
        allocate(a2)
        a2 = int(a1, c_int)
        allocate(a3(10))
      end subroutine

      function f1(a, b) bind(c)
        use, intrinsic :: iso_c_binding
        implicit none
        integer(c_int) :: f1
        real(c_float), allocatable :: a
        real(c_float), pointer :: b
        f1 = 0
        if (associated(b)) f1 = a
      end function
