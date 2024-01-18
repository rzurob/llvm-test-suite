! TS29113 says that the defintion of TKR compatible is changed so that
! two items are TKR compatible if TK match and either one has assumed rank.
! This test case checks for ambiguous generic interfaces involving
! assumed rank dummy arguments.
program main
  implicit none
  interface gen1
    subroutine sub11(a)
      integer :: a
    end subroutine

    subroutine sub12(a)
      integer :: a(..)
    end subroutine
  end interface

  interface gen2
    subroutine sub21(a)
      integer :: a(:)
    end subroutine

    subroutine sub22(a)
      integer :: a(..)
    end subroutine
  end interface

  interface gen3
    elemental subroutine sub31(a)
      integer, intent(in) :: a
    end subroutine

    subroutine sub32(a)
      integer :: a(..)
    end subroutine
  end interface

  interface gen4
    subroutine sub41(a) bind(c)
      integer :: a
    end subroutine

    subroutine sub42(a) bind(c)
      integer :: a(..)
    end subroutine
  end interface

  interface gen5
    subroutine sub51(a) bind(c)
      integer :: a
    end subroutine

    subroutine sub52(a) bind(c)
      integer :: a(..)
    end subroutine
  end interface

  interface gen6
    subroutine sub61(a)
      integer, allocatable :: a(:)
    end subroutine

    subroutine sub62(a)
      integer, pointer :: a(:)
    end subroutine

    subroutine sub63(a)
      integer, allocatable :: a(..)
    end subroutine
  end interface

  integer, pointer :: p(:)
  integer :: x
  x = 1

  call gen1(x)
  call gen2(x)
  call gen3(x)
  call gen4(x)
  call gen5(x)
  call gen6(p)
end program
