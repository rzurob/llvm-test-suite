!* =
!*
!* DATE                       : February 22, 2011
!*
!* PRIMARY FUNCTIONS TESTED   : Implied-shape arrays
!*
!* DESCRIPTION                : Testing proper diagnostics of
!*                              implied-shape arrays. RHS is
!*                              scalar
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      program impliedshape01d

      type base
        integer :: a    = 1
        integer :: b    = 2
      end type

      integer, parameter          :: i = 10
      real, parameter             :: r = 10.0
      double precision, parameter :: dp = 10.0
      complex, parameter          :: c = (10.0,11.0)
      character, parameter        :: ch = 'z'
      logical, parameter          :: l = .TRUE.
      byte, parameter             :: b = 10
      type(base), parameter       :: tb = base(10,11)

      ! 5a
      integer, parameter :: i4d1 (*) = 1
      integer, parameter :: i4d2 (*,*) = 2
      integer, parameter :: i4d3 (*,*,*) = 3
      integer, parameter :: i4d4 (*,*,*,*) = i

      real, parameter :: r4d1 (*) = 1.0
      real, parameter :: r4d2 (*,*) = 2.0
      real, parameter :: r4d3 (*,*,*) = 3.0
      real, parameter :: r4d4 (*,*,*,*) = r

      double precision, parameter :: dp4d1 (*) = 1.0
      double precision, parameter :: dp4d2 (*,*) = 2.0
      double precision, parameter :: dp4d3 (*,*,*) = 3.0
      double precision, parameter :: dp4d4 (*,*,*,*) = dp

      complex, parameter :: cp4d1 (*) = (1.0,2.0)
      complex, parameter :: cp4d2 (*,*) = (3.0,4.0)
      complex, parameter :: cp4d3 (*,*,*) = (5.0,6.0)
      complex, parameter :: cp4d4 (*,*,*,*) = c

      character, parameter :: ch4d1 (*) = 'a'
      character, parameter :: ch4d2 (*,*) = 'b'
      character, parameter :: ch4d3 (*,*,*) = 'c'
      character, parameter :: ch4d4 (*,*,*,*) = ch

      logical, parameter :: l4d1 (*) = .TRUE.
      logical, parameter :: l4d2 (*,*) = .FALSE.
      logical, parameter :: l4d3 (*,*,*) = .TRUE.
      logical, parameter :: l4d4 (*,*,*,*) = l

      byte, parameter :: b4d1 (*) = 1
      byte, parameter :: b4d2 (*,*) = 2
      byte, parameter :: b4d3 (*,*,*) = 3
      byte, parameter :: b4d4 (*,*,*,*) = b

      type(base), parameter :: tb4d1 (*) = base(1,2)
      type(base), parameter :: tb4d2 (*,*) = base(3,4)
      type(base), parameter :: tb4d3 (*,*,*) = base(5,6)
      type(base), parameter :: tb4d4 (*,*,*,*) = tb

      end
