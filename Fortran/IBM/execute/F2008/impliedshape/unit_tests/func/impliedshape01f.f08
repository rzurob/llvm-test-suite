!* =================================================================== &
!*
!* DATE                       : February 14, 2011
!*
!* PRIMARY FUNCTIONS TESTED   : Implied-shape arrays
!*
!* DESCRIPTION                : Testing proper functionality of
!*                              implied-shape arrays
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      program impliedshape01f

      type base
        integer :: a    = 1
        integer :: b    = 2
      end type

      ! Integer
      integer, parameter :: i4c1 (*) = [ 1, (i,i=2,6)    ]
      integer, parameter :: i4c2 (*) = [    (i,i=1,5), 6 ]
      integer, parameter :: i4c3 (*) = [ 1, (i,i=2,5), 6 ]
      integer, parameter :: i4c4 (*) = [ (i,i=1,3), (i,i=4,6) ]
      integer, parameter :: i4d1 (*) = reshape([1,2],[1])
      integer, parameter :: i4d2 (*,*) = reshape([1,2,3,4],[2,2])
      integer, parameter :: i4d3 (*,*,*) = &
           & reshape([1,2,3,4,5,6,7,8],[2,2,2])
      integer, parameter :: i4d4 (*,*,*,*) = &
           & reshape([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16],[2,2,2,2])

      integer, parameter :: i4c1a (6) = [ 1, (i,i=2,6)    ]
      integer, parameter :: i4c2a (6) = [    (i,i=1,5), 6 ]
      integer, parameter :: i4c3a (6) = [ 1, (i,i=2,5), 6 ]
      integer, parameter :: i4c4a (6) = [ (i,i=1,3), (i,i=4,6) ]
      integer, parameter :: i4d1a (1) = reshape([1,2],[1])
      integer, parameter :: i4d2a (2,2) = reshape([1,2,3,4],[2,2])
      integer, parameter :: i4d3a (2,2,2) = &
           & reshape([1,2,3,4,5,6,7,8],[2,2,2])
      integer, parameter :: i4d4a (2,2,2,2) = &
           & reshape([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16],[2,2,2,2])


      ! Real
      real, parameter :: r4c1 (*) = [ 1.0, (i,i=2,6)    ]
      real, parameter :: r4c2 (*) = [    (i,i=1,5), 6.0 ]
      real, parameter :: r4c3 (*) = [ 1.0, (i,i=2,5), 6.0 ]
      real, parameter :: r4c4 (*) = [ (i,i=1,3), (i,i=4,6) ]
      real, parameter :: r4d1 (*) = reshape([1.0,2.0],[1])
      real, parameter :: r4d2 (*,*) = reshape([1.0,2.0,3.0,4.0],[2,2])
      real, parameter :: r4d3 (*,*,*) = &
           & reshape([1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0],[2,2,2])
      real, parameter :: r4d4 (*,*,*,*) = &
           & reshape([1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0, &
                    & 11.0,12.0,13.0,14.0,15.0,16.0],[2,2,2,2])

      real, parameter :: r4c1a (6) = [ 1.0, (i,i=2,6)    ]
      real, parameter :: r4c2a (6) = [    (i,i=1,5), 6.0 ]
      real, parameter :: r4c3a (6) = [ 1.0, (i,i=2,5), 6.0 ]
      real, parameter :: r4c4a (6) = [ (i,i=1,3), (i,i=4,6) ]
      real, parameter :: r4d1a (1) = reshape([1.0,2.0],[1])
      real, parameter :: r4d2a (2,2) = reshape([1.0,2.0,3.0,4.0],[2,2])
      real, parameter :: r4d3a (2,2,2) = &
           & reshape([1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0],[2,2,2])
      real, parameter :: r4d4a (2,2,2,2) = &
           & reshape([1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0, &
                    & 11.0,12.0,13.0,14.0,15.0,16.0],[2,2,2,2])


      ! Double Precision
      double precision, parameter :: dp4c1 (*) = [ 1.0, (i,i=2,6)    ]
      double precision, parameter :: dp4c2 (*) = [    (i,i=1,5), 6.0 ]
      double precision, parameter :: dp4c3 (*) = [ 1.0, (i,i=2,5), 6.0 ]
      double precision, parameter :: dp4c4 (*) = [ (i,i=1,3), (i,i=4,6) ]
      double precision, parameter :: dp4d1 (*) = reshape([1.0,2.0],[1])
      double precision, parameter :: dp4d2 (*,*) = reshape([1.0,2.0,3.0,4.0],[2,2])
      double precision, parameter :: dp4d3 (*,*,*) = &
           & reshape([1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0],[2,2,2])
      double precision, parameter :: dp4d4 (*,*,*,*) = &
           & reshape([1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0, &
                    & 11.0,12.0,13.0,14.0,15.0,16.0],[2,2,2,2])

      double precision, parameter :: dp4c1a (6) = [ 1.0, (i,i=2,6)    ]
      double precision, parameter :: dp4c2a (6) = [    (i,i=1,5), 6.0 ]
      double precision, parameter :: dp4c3a (6) = [ 1.0, (i,i=2,5), 6.0 ]
      double precision, parameter :: dp4c4a (6) = [ (i,i=1,3), (i,i=4,6) ]
      double precision, parameter :: dp4d1a (1) = reshape([1.0,2.0],[1])
      double precision, parameter :: dp4d2a (2,2) = reshape([1.0,2.0,3.0,4.0],[2,2])
      double precision, parameter :: dp4d3a (2,2,2) = &
           & reshape([1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0],[2,2,2])
      double precision, parameter :: dp4d4a (2,2,2,2) = &
           & reshape([1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0, &
                    & 11.0,12.0,13.0,14.0,15.0,16.0],[2,2,2,2])

      ! Complex
      complex, parameter :: cp4c1 (*) = [ 1, (i,i=2,6)    ]
      complex, parameter :: cp4c2 (*) = [    (i,i=1,5), 6 ]
      complex, parameter :: cp4c3 (*) = [ 1, (i,i=2,5), 6 ]
      complex, parameter :: cp4c4 (*) = [ (i,i=1,3), (i,i=4,6) ]
      complex, parameter :: cp4d1 (*) = reshape([1,2],[1])
      complex, parameter :: cp4d2 (*,*) = reshape([1,2,3,4],[2,2])
      complex, parameter :: cp4d3 (*,*,*) = &
           & reshape([1,2,3,4,5,6,7,8],[2,2,2])
      complex, parameter :: cp4d4 (*,*,*,*) = &
           & reshape([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16],[2,2,2,2])

      complex, parameter :: cp4c1a (6) = [ 1, (i,i=2,6)    ]
      complex, parameter :: cp4c2a (6) = [    (i,i=1,5), 6 ]
      complex, parameter :: cp4c3a (6) = [ 1, (i,i=2,5), 6 ]
      complex, parameter :: cp4c4a (6) = [ (i,i=1,3), (i,i=4,6) ]
      complex, parameter :: cp4d1a (1) = reshape([1,2],[1])
      complex, parameter :: cp4d2a (2,2) = reshape([1,2,3,4],[2,2])
      complex, parameter :: cp4d3a (2,2,2) = &
           & reshape([1,2,3,4,5,6,7,8],[2,2,2])
      complex, parameter :: cp4d4a (2,2,2,2) = &
           & reshape([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16],[2,2,2,2])


      ! Character
      character, parameter :: ch4c1 (*) = [ 'a', ('b',i=2,6)    ]
      character, parameter :: ch4c2 (*) = [    ('a',i=1,5), 'b' ]
      character, parameter :: ch4c3 (*) = [ 'a', ('b',i=2,5), 'c' ]
      character, parameter :: ch4c4 (*) = [ ('a',i=1,3), ('b',i=4,6) ]
      character, parameter :: ch4d1 (*) = reshape(['a','b'],[1])
      character, parameter :: ch4d2 (*,*) = reshape(['a','b','c','d'],[2,2])
      character, parameter :: ch4d3 (*,*,*) = &
           & reshape(['a','b','c','d','e','f','g','h'],[2,2,2])
      character, parameter :: ch4d4 (*,*,*,*) = &
           & reshape(['a','b','c','d','e','f','g','h', &
           &          'i','j','k','l','m','n','o','p'],[2,2,2,2])

      character, parameter :: ch4c1a (6) = [ 'a', ('b',i=2,6)    ]
      character, parameter :: ch4c2a (6) = [    ('a',i=1,5), 'b' ]
      character, parameter :: ch4c3a (6) = [ 'a', ('b',i=2,5), 'c' ]
      character, parameter :: ch4c4a (6) = [ ('a',i=1,3), ('b',i=4,6) ]
      character, parameter :: ch4d1a (1) = reshape(['a','b'],[1])
      character, parameter :: ch4d2a (2,2) = reshape(['a','b','c','d'],[2,2])
      character, parameter :: ch4d3a (2,2,2) = &
           & reshape(['a','b','c','d','e','f','g','h'],[2,2,2])
      character, parameter :: ch4d4a (2,2,2,2) = &
           & reshape(['a','b','c','d','e','f','g','h', &
           &          'i','j','k','l','m','n','o','p'],[2,2,2,2])


      ! Logical
      logical, parameter :: l4c1 (*) = [ .TRUE., (.FALSE.,i=2,6)    ]
      logical, parameter :: l4c2 (*) = [    (.TRUE.,i=1,5), .FALSE. ]
      logical, parameter :: l4c3 (*) = [ .TRUE., (.FALSE.,i=2,5), .TRUE. ]
      logical, parameter :: l4c4 (*) = [ (.TRUE.,i=1,3), (.FALSE.,i=4,6) ]
      logical, parameter :: l4d1 (*) = reshape([.TRUE.,.FALSE.],[1])
      logical, parameter :: l4d2 (*,*) = &
           & reshape([.TRUE.,.FALSE.,.TRUE.,.FALSE.],[2,2])
      logical, parameter :: l4d3 (*,*,*) = &
           & reshape([.FALSE.,.TRUE.,.FALSE.,.TRUE., &
           &          .FALSE.,.TRUE.,.FALSE.,.TRUE.],[2,2,2])
      logical, parameter :: l4d4 (*,*,*,*) = &
           & reshape([.TRUE.,.FALSE.,.TRUE.,.FALSE., &
           &          .TRUE.,.FALSE.,.TRUE.,.FALSE., &
           &          .TRUE.,.FALSE.,.TRUE.,.FALSE., &
           &          .TRUE.,.FALSE.,.TRUE.,.FALSE.],[2,2,2,2])

      logical, parameter :: l4c1a (6) = [ .TRUE., (.FALSE.,i=2,6)    ]
      logical, parameter :: l4c2a (6) = [    (.TRUE.,i=1,5), .FALSE. ]
      logical, parameter :: l4c3a (6) = [ .TRUE., (.FALSE.,i=2,5), .TRUE. ]
      logical, parameter :: l4c4a (6) = [ (.TRUE.,i=1,3), (.FALSE.,i=4,6) ]
      logical, parameter :: l4d1a (1) = reshape([.TRUE.,.FALSE.],[1])
      logical, parameter :: l4d2a (2,2) = &
           & reshape([.TRUE.,.FALSE.,.TRUE.,.FALSE.],[2,2])
      logical, parameter :: l4d3a (2,2,2) = &
           & reshape([.FALSE.,.TRUE.,.FALSE.,.TRUE., &
           &          .FALSE.,.TRUE.,.FALSE.,.TRUE.],[2,2,2])
      logical, parameter :: l4d4a (2,2,2,2) = &
           & reshape([.TRUE.,.FALSE.,.TRUE.,.FALSE., &
           &          .TRUE.,.FALSE.,.TRUE.,.FALSE., &
           &          .TRUE.,.FALSE.,.TRUE.,.FALSE., &
           &          .TRUE.,.FALSE.,.TRUE.,.FALSE.],[2,2,2,2])


      ! Byte
      byte, parameter :: b4c1 (*) = [ 1, (i,i=2,6)    ]
      byte, parameter :: b4c2 (*) = [    (i,i=1,5), 6 ]
      byte, parameter :: b4c3 (*) = [ 1, (i,i=2,5), 6 ]
      byte, parameter :: b4c4 (*) = [ (i,i=1,3), (i,i=4,6) ]
      byte, parameter :: b4d1 (*) = reshape([1,2],[1])
      byte, parameter :: b4d2 (*,*) = reshape([1,2,3,4],[2,2])
      byte, parameter :: b4d3 (*,*,*) = &
           & reshape([1,2,3,4,5,6,7,8],[2,2,2])
      byte, parameter :: b4d4 (*,*,*,*) = &
           & reshape([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16],[2,2,2,2])

      byte, parameter :: b4c1a (6) = [ 1, (i,i=2,6)    ]
      byte, parameter :: b4c2a (6) = [    (i,i=1,5), 6 ]
      byte, parameter :: b4c3a (6) = [ 1, (i,i=2,5), 6 ]
      byte, parameter :: b4c4a (6) = [ (i,i=1,3), (i,i=4,6) ]
      byte, parameter :: b4d1a (1) = reshape([1,2],[1])
      byte, parameter :: b4d2a (2,2) = reshape([1,2,3,4],[2,2])
      byte, parameter :: b4d3a (2,2,2) = &
           & reshape([1,2,3,4,5,6,7,8],[2,2,2])
      byte, parameter :: b4d4a (2,2,2,2) = &
           & reshape([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16],[2,2,2,2])

      ! Derived type
      type(base), parameter :: tb4c1 (*) = &
           & [ base(1,1), (base(i,i),i=2,6) ]
      type(base), parameter :: tb4c2 (*) = &
           & [ (base(i,i),i=1,5), base(6,6) ]
      type(base), parameter :: tb4c3 (*) = &
           & [ base(1,1), (base(i,i),i=2,5), base(6,6) ]
      type(base), parameter :: tb4c4 (*) = &
           & [ (base(i,i),i=1,3), (base(i,i),i=4,6) ]
      type(base), parameter :: tb4d1 (*) = reshape([base(), base(10,11)],[2])
      type(base), parameter :: tb4d2 (*,*) = &
           & reshape([base(), base(3,3), base(4,4), base(10,11)],[2,2])
      type(base), parameter :: tb4d3 (*,*,*) = &
           & reshape([(base(i,i),i=1,8)],[2,2,2])
      type(base), parameter :: tb4d4 (*,*,*,*) = &
           & reshape([(base(i,i),i=1,16)],[2,2,2,2])

      type(base), parameter :: tb4c1a (6) = &
           & [ base(1,1), (base(i,i),i=2,6) ]
      type(base), parameter :: tb4c2a (6) = &
           & [ (base(i,i),i=1,5), base(6,6) ]
      type(base), parameter :: tb4c3a (6) = &
           & [ base(1,1), (base(i,i),i=2,5), base(6,6) ]
      type(base), parameter :: tb4c4a (6) = &
           & [ (base(i,i),i=1,3), (base(i,i),i=4,6) ]
      type(base), parameter :: tb4d1a (2) = reshape([base(), base(10,11)],[2])
      type(base), parameter :: tb4d2a (2,2) = &
           & reshape([base(), base(3,3), base(4,4), base(10,11)],[2,2])
      type(base), parameter :: tb4d3a (2,2,2) = &
           & reshape([(base(i,i),i=1,8)],[2,2,2])
      type(base), parameter :: tb4d4a (2,2,2,2) = &
           & reshape([(base(i,i),i=1,16)],[2,2,2,2])

      if (ANY(i4c1 .NE. i4c1a)) ERROR STOP 1
      if (ANY(i4c2 .NE. i4c2a)) ERROR STOP 2
      if (ANY(i4c3 .NE. i4c3a)) ERROR STOP 3
      if (ANY(i4c4 .NE. i4c4a)) ERROR STOP 4
      if (ANY(i4d1 .NE. i4d1a)) ERROR STOP 5
      if (ANY(i4d2 .NE. i4d2a)) ERROR STOP 6
      if (ANY(i4d3 .NE. i4d3a)) ERROR STOP 7
      if (ANY(i4d4 .NE. i4d4a)) ERROR STOP 8

      if (ANY(r4c1 .NE. r4c1a)) ERROR STOP 11
      if (ANY(r4c2 .NE. r4c2a)) ERROR STOP 12
      if (ANY(r4c3 .NE. r4c3a)) ERROR STOP 13
      if (ANY(r4c4 .NE. r4c4a)) ERROR STOP 14
      if (ANY(r4d1 .NE. r4d1a)) ERROR STOP 15
      if (ANY(r4d2 .NE. r4d2a)) ERROR STOP 16
      if (ANY(r4d3 .NE. r4d3a)) ERROR STOP 17
      if (ANY(r4d4 .NE. r4d4a)) ERROR STOP 18

      if (ANY(dp4c1 .NE. dp4c1a)) ERROR STOP 21
      if (ANY(dp4c2 .NE. dp4c2a)) ERROR STOP 22
      if (ANY(dp4c3 .NE. dp4c3a)) ERROR STOP 23
      if (ANY(dp4c4 .NE. dp4c4a)) ERROR STOP 24
      if (ANY(dp4d1 .NE. dp4d1a)) ERROR STOP 25
      if (ANY(dp4d2 .NE. dp4d2a)) ERROR STOP 26
      if (ANY(dp4d3 .NE. dp4d3a)) ERROR STOP 27
      if (ANY(dp4d4 .NE. dp4d4a)) ERROR STOP 28

      if (ANY(cp4c1 .NE. cp4c1a)) ERROR STOP 41
      if (ANY(cp4c2 .NE. cp4c2a)) ERROR STOP 42
      if (ANY(cp4c3 .NE. cp4c3a)) ERROR STOP 43
      if (ANY(cp4c4 .NE. cp4c4a)) ERROR STOP 44
      if (ANY(cp4d1 .NE. cp4d1a)) ERROR STOP 45
      if (ANY(cp4d2 .NE. cp4d2a)) ERROR STOP 46
      if (ANY(cp4d3 .NE. cp4d3a)) ERROR STOP 47
      if (ANY(cp4d4 .NE. cp4d4a)) ERROR STOP 48

      if (ANY(ch4c1 .NE. ch4c1a)) ERROR STOP 51
      if (ANY(ch4c2 .NE. ch4c2a)) ERROR STOP 52
      if (ANY(ch4c3 .NE. ch4c3a)) ERROR STOP 53
      if (ANY(ch4c4 .NE. ch4c4a)) ERROR STOP 54
      if (ANY(ch4d1 .NE. ch4d1a)) ERROR STOP 55
      if (ANY(ch4d2 .NE. ch4d2a)) ERROR STOP 56
      if (ANY(ch4d3 .NE. ch4d3a)) ERROR STOP 57
      if (ANY(ch4d4 .NE. ch4d4a)) ERROR STOP 58

      if (ANY(l4c1 .NEQV. l4c1a)) ERROR STOP 61
      if (ANY(l4c2 .NEQV. l4c2a)) ERROR STOP 62
      if (ANY(l4c3 .NEQV. l4c3a)) ERROR STOP 63
      if (ANY(l4c4 .NEQV. l4c4a)) ERROR STOP 64
      if (ANY(l4d1 .NEQV. l4d1a)) ERROR STOP 65
      if (ANY(l4d2 .NEQV. l4d2a)) ERROR STOP 66
      if (ANY(l4d3 .NEQV. l4d3a)) ERROR STOP 67
      if (ANY(l4d4 .NEQV. l4d4a)) ERROR STOP 68

      if (ANY(b4c1 .NE. b4c1a)) ERROR STOP 71
      if (ANY(b4c2 .NE. b4c2a)) ERROR STOP 72
      if (ANY(b4c3 .NE. b4c3a)) ERROR STOP 73
      if (ANY(b4c4 .NE. b4c4a)) ERROR STOP 74
      if (ANY(b4d1 .NE. b4d1a)) ERROR STOP 75
      if (ANY(b4d2 .NE. b4d2a)) ERROR STOP 76
      if (ANY(b4d3 .NE. b4d3a)) ERROR STOP 77
      if (ANY(b4d4 .NE. b4d4a)) ERROR STOP 78

      if (ANY(tb4c1%a .NE. tb4c1a%a) .OR. ANY(tb4c1%b .NE. tb4c1a%b)) ERROR STOP 81
      if (ANY(tb4c1%a .NE. tb4c1a%a) .OR. ANY(tb4c1%b .NE. tb4c1a%b)) ERROR STOP 82
      if (ANY(tb4c1%a .NE. tb4c1a%a) .OR. ANY(tb4c1%b .NE. tb4c1a%b)) ERROR STOP 83
      if (ANY(tb4c1%a .NE. tb4c1a%a) .OR. ANY(tb4c1%b .NE. tb4c1a%b)) ERROR STOP 84
      if (ANY(tb4c1%a .NE. tb4c1a%a) .OR. ANY(tb4c1%b .NE. tb4c1a%b)) ERROR STOP 85
      if (ANY(tb4c1%a .NE. tb4c1a%a) .OR. ANY(tb4c1%b .NE. tb4c1a%b)) ERROR STOP 86
      if (ANY(tb4c1%a .NE. tb4c1a%a) .OR. ANY(tb4c1%b .NE. tb4c1a%b)) ERROR STOP 87
      if (ANY(tb4c1%a .NE. tb4c1a%a) .OR. ANY(tb4c1%b .NE. tb4c1a%b)) ERROR STOP 88

      end
