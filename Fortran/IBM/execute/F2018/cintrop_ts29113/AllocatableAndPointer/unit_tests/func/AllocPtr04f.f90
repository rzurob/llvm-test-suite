!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb, 2013
!*
!*  PRIMARY FUNCTIONS TESTED   : C-interop Allocatable/Pointer
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Try passing a non-contiguous pointer
!*                               array to C for printing.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      use, intrinsic :: iso_c_binding
      implicit none

      interface
        subroutine print2dfarr(arg, test) bind(c)
          import
          real(c_double), pointer :: arg(:,:)
          integer(c_int), value :: test
        end
      end interface

      integer, parameter :: ROWS = 10, COLS = 20

      real(c_double), target :: t(ROWS, COLS)
      real(c_double), pointer :: p(:,:)

      integer :: i, j

      t = reshape([(i, i = 1, ROWS*COLS, 1)], [ROWS,COLS])

      ! Test 1
      p => t
      call print2dfarr(p, 1)

      ! Test 2
      p => t(::2,:)
      call print2dfarr(p, 2)

      ! Test 3
      p => NULL()
      call print2dfarr(p, 3)

      ! Test 4
      i = 2
      p => t(:, i:COLS:i)
      call print2dfarr(p, 4)

      ! Test 5
      p => t(lbound(t,1)+1:ROWS-1, :)
      call print2dfarr(p, 5)

      ! Test 6
      p => t(::2, ::2)
      call print2dfarr(p, 6)

      ! Test 7
      p => t(:,:)
      call print2dfarr(p, 7)


      end



