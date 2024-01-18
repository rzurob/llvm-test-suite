!* =================================================================== &
!*
!* DATE                       : February 22, 2011
!*
!* PRIMARY FUNCTIONS TESTED   : Implied-shape arrays
!*
!* DESCRIPTION                : Testing proper diagnostics of
!*                              implied-shape arrays with RHS
!*                              not compile-time known
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      module impliedshape02d_mod
      type base
        integer :: a    = 1
        integer :: b    = 2
      end type

      integer :: i (10,10,10,10)
      real :: r (10,10,10,10)
      double precision :: dp (10,10,10,10)
      complex :: cp (10,10,10,10)
      character(4) :: ch (10,10,10,10)
      logical :: l (10,10,10,10)
      byte :: b (10,10,10,10)
      type(base), save :: tb (10,10,10,10)

      integer, parameter :: i4d4 (*,*,*,*) = i
      real, parameter :: r4d4 (*,*,*,*) = r
      double precision, parameter :: dp4d4 (*,*,*,*) = dp
      complex, parameter :: cp4d4 (*,*,*,*) = cp
      character(*), parameter :: ch4d4 (*,*,*,*) = ch
      logical, parameter :: l4d4 (*,*,*,*) = l
      byte, parameter :: b4d4 (*,*,*,*) = b
      type(base), parameter :: tb4d4 (*,*,*,*) = tb

      end

      program impliedshape02d
      type base
        integer :: a    = 1
        integer :: b    = 2
      end type

      integer :: i (10,10,10,10)
      real :: r (10,10,10,10)
      double precision :: dp (10,10,10,10)
      complex :: cp (10,10,10,10)
      character(4) :: ch (10,10,10,10)
      logical :: l (10,10,10,10)
      byte :: b (10,10,10,10)
      type(base) :: tb (10,10,10,10)

      integer, parameter :: i4d4 (*,*,*,*) = i
      real, parameter :: r4d4 (*,*,*,*) = r
      double precision, parameter :: dp4d4 (*,*,*,*) = dp
      complex, parameter :: cp4d4 (*,*,*,*) = cp
      character(*), parameter :: ch4d4 (*,*,*,*) = ch
      logical, parameter :: l4d4 (*,*,*,*) = l
      byte, parameter :: b4d4 (*,*,*,*) = b
      type(base), parameter :: tb4d4 (*,*,*,*) = tb

      contains
      subroutine foo()
      type base
        integer :: a    = 1
        integer :: b    = 2
      end type

      integer :: i (10,10,10,10)
      real :: r (10,10,10,10)
      double precision :: dp (10,10,10,10)
      complex :: cp (10,10,10,10)
      character(4) :: ch (10,10,10,10)
      logical :: l (10,10,10,10)
      byte :: b (10,10,10,10)
      type(base) :: tb (10,10,10,10)

      integer, parameter :: i4d4 (*,*,*,*) = i
      real, parameter :: r4d4 (*,*,*,*) = r
      double precision, parameter :: dp4d4 (*,*,*,*) = dp
      complex, parameter :: cp4d4 (*,*,*,*) = cp
      character(*), parameter :: ch4d4 (*,*,*,*) = ch
      logical, parameter :: l4d4 (*,*,*,*) = l
      byte, parameter :: b4d4 (*,*,*,*) = b
      type(base), parameter :: tb4d4 (*,*,*,*) = tb

      end subroutine
      end

      subroutine bar(i,r,dp,cp,ch,l,b,tb)
      type base
        sequence
        integer :: a    = 1
        integer :: b    = 2
      end type

      integer :: i (10,10,10,10)
      real :: r (10,10,10,10)
      double precision :: dp (10,10,10,10)
      complex :: cp (10,10,10,10)
      character(4) :: ch (10,10,10,10)
      logical :: l (10,10,10,10)
      byte :: b (10,10,10,10)
      type(base) :: tb (10,10,10,10)

      integer, parameter :: i4d4 (*,*,*,*) = i
      real, parameter :: r4d4 (*,*,*,*) = r
      double precision, parameter :: dp4d4 (*,*,*,*) = dp
      complex, parameter :: cp4d4 (*,*,*,*) = cp
      character(*), parameter :: ch4d4 (*,*,*,*) = ch
      logical, parameter :: l4d4 (*,*,*,*) = l
      byte, parameter :: b4d4 (*,*,*,*) = b
      type(base), parameter :: tb4d4 (*,*,*,*) = tb

      end
