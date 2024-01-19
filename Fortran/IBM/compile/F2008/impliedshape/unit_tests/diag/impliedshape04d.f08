!* =================================================================== &
!*
!* DATE                       : February 22, 2011
!*
!* PRIMARY FUNCTIONS TESTED   : Implied-shape arrays
!*
!* DESCRIPTION                : Testing proper diagnostics of
!*                              implied-shape arrays in
!*                              equivalence and common
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      module impliedshape04d_mod
      type base
        integer :: a    = 1
        integer :: b    = 2
      end type

      integer, parameter :: i1 (*) = [ 1, (i,i=2,6)    ]
      integer, parameter :: i2 (*) = [    (i,i=1,5), 6 ]
      real, parameter :: r1 (*) = [ 1.0, (real(i),i=2,6) ]
      real, parameter :: r2 (*) = [ (real(i),i=1,5), 6.0 ]
      double precision, parameter :: dp1 (*) = [ dble(1.0), (dble(i),i=2,6) ]
      double precision, parameter :: dp2 (*) = [ (dble(i),i=1,5), dble(6.0) ]
      complex, parameter :: cp1 (*) = [ 1, (i,i=2,6)    ]
      complex, parameter :: cp2 (*) = [    (i,i=1,5), 6 ]
      character, parameter :: ch1 (*) = [ 'a', ('b',i=2,6)    ]
      character, parameter :: ch2 (*) = [    ('a',i=1,5), 'b' ]
      logical, parameter :: l1 (*) = [ .TRUE., (.FALSE.,i=2,6)    ]
      logical, parameter :: l2 (*) = [    (.TRUE.,i=1,5), .FALSE. ]
      byte, parameter :: b1 (*) = [ 1, (i,i=2,6)    ]
      byte, parameter :: b2 (*) = [    (i,i=1,5), 6 ]
      type(base), parameter :: tb1 (*) = &
           & [ base(1,1), (base(i,i),i=2,6) ]
      type(base), parameter :: tb2 (*) = &
           & [ (base(i,i),i=1,5), base(6,6) ]

      equivalence( i1, i2 )
      equivalence( r1, i2 )
      equivalence( dp1, dp2 )
      equivalence( cp1, cp2 )
      equivalence( ch1, ch2 )
      equivalence( l1, l2 )
      equivalence( b1, b2 )
      equivalence( tb1, tb2 )

      common i1, r1, dp1, ch1, cp1, l1, b1, tb1

      end

      program impliedshape04d
      type base
        integer :: a    = 1
        integer :: b    = 2
      end type

      integer, parameter :: i1 (*) = [ 1, (i,i=2,6)    ]
      integer, parameter :: i2 (*) = [    (i,i=1,5), 6 ]
      real, parameter :: r1 (*) = [ 1.0, (real(i),i=2,6)    ]
      real, parameter :: r2 (*) = [ (real(i),i=1,5), 6.0 ]
      double precision, parameter :: dp1 (*) = [ dble(1.0), (dble(i),i=2,6) ]
      double precision, parameter :: dp2 (*) = [ (dble(i),i=1,5), dble(6.0) ]
      complex, parameter :: cp1 (*) = [ 1, (i,i=2,6)    ]
      complex, parameter :: cp2 (*) = [    (i,i=1,5), 6 ]
      character, parameter :: ch1 (*) = [ 'a', ('b',i=2,6)    ]
      character, parameter :: ch2 (*) = [    ('a',i=1,5), 'b' ]
      logical, parameter :: l1 (*) = [ .TRUE., (.FALSE.,i=2,6)    ]
      logical, parameter :: l2 (*) = [    (.TRUE.,i=1,5), .FALSE. ]
      byte, parameter :: b1 (*) = [ 1, (i,i=2,6)    ]
      byte, parameter :: b2 (*) = [    (i,i=1,5), 6 ]
      type(base), parameter :: tb1 (*) = &
           & [ base(1,1), (base(i,i),i=2,6) ]
      type(base), parameter :: tb2 (*) = &
           & [ (base(i,i),i=1,5), base(6,6) ]

      equivalence( i1, i2 )
      equivalence( r1, i2 )
      equivalence( dp1, dp2 )
      equivalence( cp1, cp2 )
      equivalence( ch1, ch2 )
      equivalence( l1, l2 )
      equivalence( b1, b2 )
      equivalence( tb1, tb2 )

      common i1, r1, dp1, ch1, cp1, l1, b1, tb1

      contains
      subroutine foo()
      type base
        integer :: a    = 1
        integer :: b    = 2
      end type

      integer, parameter :: i1 (*) = [ 1, (i,i=2,6)    ]
      integer, parameter :: i2 (*) = [    (i,i=1,5), 6 ]
      real, parameter :: r1 (*) = [ 1.0, (real(i),i=2,6)    ]
      real, parameter :: r2 (*) = [ (1.0 * i,i=1,5), 6.0 ]
      double precision, parameter :: dp1 (*) = [ 1.0, (1.0 * i,i=2,6) ]
      double precision, parameter :: dp2 (*) = [ (1.0 * i,i=1,5), 6.0 ]
      complex, parameter :: cp1 (*) = [ 1, (i,i=2,6)    ]
      complex, parameter :: cp2 (*) = [    (i,i=1,5), 6 ]
      character, parameter :: ch1 (*) = [ 'a', ('b',i=2,6)    ]
      character, parameter :: ch2 (*) = [    ('a',i=1,5), 'b' ]
      logical, parameter :: l1 (*) = [ .TRUE., (.FALSE.,i=2,6)    ]
      logical, parameter :: l2 (*) = [    (.TRUE.,i=1,5), .FALSE. ]
      byte, parameter :: b1 (*) = [ 1, (i,i=2,6)    ]
      byte, parameter :: b2 (*) = [    (i,i=1,5), 6 ]
      type(base), parameter :: tb1 (*) = &
           & [ base(1,1), (base(i,i),i=2,6) ]
      type(base), parameter :: tb2 (*) = &
           & [ (base(i,i),i=1,5), base(6,6) ]

      equivalence( i1, i2 )
      equivalence( r1, i2 )
      equivalence( dp1, dp2 )
      equivalence( cp1, cp2 )
      equivalence( ch1, ch2 )
      equivalence( l1, l2 )
      equivalence( b1, b2 )
      equivalence( tb1, tb2 )

      common i1, r1, dp1, ch1, cp1, l1, b1, tb1

      end subroutine
      end


      subroutine bar()
      type base
        integer :: a    = 1
        integer :: b    = 2
      end type

      integer, parameter :: i1 (*) = [ 1, (i,i=2,6)    ]
      integer, parameter :: i2 (*) = [    (i,i=1,5), 6 ]
      real, parameter :: r1 (*) = [ 1.0, (0.0 + i,i=2,6) ]
      real, parameter :: r2 (*) = [ (0.0 + i,i=1,5), 6.0 ]
      double precision, parameter :: dp1 (*) = [ 1.0, (0.0 + i,i=2,6) ]
      double precision, parameter :: dp2 (*) = [ (0.0 + i,i=1,5), 6.0 ]
      complex, parameter :: cp1 (*) = [ 1, (i,i=2,6)    ]
      complex, parameter :: cp2 (*) = [    (i,i=1,5), 6 ]
      character, parameter :: ch1 (*) = [ 'a', ('b',i=2,6)    ]
      character, parameter :: ch2 (*) = [    ('a',i=1,5), 'b' ]
      logical, parameter :: l1 (*) = [ .TRUE., (.FALSE.,i=2,6)    ]
      logical, parameter :: l2 (*) = [    (.TRUE.,i=1,5), .FALSE. ]
      byte, parameter :: b1 (*) = [ 1, (i,i=2,6)    ]
      byte, parameter :: b2 (*) = [    (i,i=1,5), 6 ]
      type(base), parameter :: tb1 (*) = &
           & [ base(1,1), (base(i,i),i=2,6) ]
      type(base), parameter :: tb2 (*) = &
           & [ (base(i,i),i=1,5), base(6,6) ]

      equivalence( i1, i2 )
      equivalence( r1, i2 )
      equivalence( dp1, dp2 )
      equivalence( cp1, cp2 )
      equivalence( ch1, ch2 )
      equivalence( l1, l2 )
      equivalence( b1, b2 )
      equivalence( tb1, tb2 )

      common i1, r1, dp1, ch1, cp1, l1, b1, tb1

      end subroutine
