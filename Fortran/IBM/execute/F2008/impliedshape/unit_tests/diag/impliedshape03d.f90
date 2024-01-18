!* =================================================================== &
!*
!* DATE                       : February 22, 2011
!* ORIGIN                     : AIX Compiler Development,
!*
!* PRIMARY FUNCTIONS TESTED   : Implied-shape arrays
!*
!* DESCRIPTION                : Testing proper diagnostics of
!*                              implied-shape arrays without
!*                              the parameter attribute
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      module impliedshape03d_mod
      type base
        integer :: a    = 1
        integer :: b    = 2
      end type

      ! Integer
      integer :: i4c1 (*) = [ 1, (i,i=2,6)    ]
      integer :: i4c2 (*) = [    (i,i=1,5), 6 ]
      integer :: i4c3 (*) = [ 1, (i,i=2,5), 6 ]
      integer :: i4c4 (*) = [ (i,i=1,3), (i,i=4,6) ]
      integer :: i4d1 (*) = reshape([1,2],[1])
      integer :: i4d2 (*,*) = reshape([1,2,3,4],[2,2])
      integer :: i4d3 (*,*,*) = &
           & reshape([1,2,3,4,5,6,7,8],[2,2,2])
      integer :: i4d4 (*,*,*,*) = &
           & reshape([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16],[2,2,2,2])

      ! Real
      real :: r4c1 (*) = [ 1.0, (real(i),i=2,6) ]
      real :: r4c2 (*) = [ (real(i),i=1,5), 6.0 ]
      real :: r4c3 (*) = [ 1.0, (real(i),i=2,5), 6.0 ]
      real :: r4c4 (*) = [ (real(i),i=1,3), (real(i),i=4,6) ]
      real :: r4d1 (*) = reshape([1.0,2.0],[1])
      real :: r4d2 (*,*) = reshape([1.0,2.0,3.0,4.0],[2,2])
      real :: r4d3 (*,*,*) = &
           & reshape([1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0],[2,2,2])
      real :: r4d4 (*,*,*,*) = &
           & reshape([1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0, &
                    & 11.0,12.0,13.0,14.0,15.0,16.0],[2,2,2,2])

      ! Double Precision
      double precision :: dp4c1 (*) = [ 1.0, (real(i),i=2,6) ]
      double precision :: dp4c2 (*) = [ (real(i),i=1,5), 6.0 ]
      double precision :: dp4c3 (*) = [ 1.0, (real(i),i=2,5), 6.0 ]
      double precision :: dp4c4 (*) = [ (i,i=1,3), (i,i=4,6) ]
      double precision :: dp4d1 (*) = reshape([1.0,2.0],[1])
      double precision :: dp4d2 (*,*) = reshape([1.0,2.0,3.0,4.0],[2,2])
      double precision :: dp4d3 (*,*,*) = &
           & reshape([1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0],[2,2,2])
      double precision :: dp4d4 (*,*,*,*) = &
           & reshape([1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0, &
                    & 11.0,12.0,13.0,14.0,15.0,16.0],[2,2,2,2])

      ! Complex
      complex :: cp4c1 (*) = [ 1, (int(i),i=2,6) ]
      complex :: cp4c2 (*) = [ (int(i),i=1,5), 6 ]
      complex :: cp4c3 (*) = [ 1, (int(i),i=2,5), 6 ]
      complex :: cp4c4 (*) = [ (i,i=1,3), (i,i=4,6) ]
      complex :: cp4d1 (*) = reshape([1,2],[1])
      complex :: cp4d2 (*,*) = reshape([1,2,3,4],[2,2])
      complex :: cp4d3 (*,*,*) = &
           & reshape([1,2,3,4,5,6,7,8],[2,2,2])
      complex :: cp4d4 (*,*,*,*) = &
           & reshape([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16],[2,2,2,2])



      ! Character
      character :: ch4c1 (*) = [ 'a', ('b',i=2,6)    ]
      character :: ch4c2 (*) = [    ('a',i=1,5), 'b' ]
      character :: ch4c3 (*) = [ 'a', ('b',i=2,5), 'c' ]
      character :: ch4c4 (*) = [ ('a',i=1,3), ('b',i=4,6) ]
      character :: ch4d1 (*) = reshape(['a','b'],[1])
      character :: ch4d2 (*,*) = reshape(['a','b','c','d'],[2,2])
      character :: ch4d3 (*,*,*) = &
           & reshape(['a','b','c','d','e','f','g','h'],[2,2,2])
      character :: ch4d4 (*,*,*,*) = &
           & reshape(['a','b','c','d','e','f','g','h', &
           &          'i','j','k','l','m','n','o','p'],[2,2,2,2])


      ! Logical
      logical :: l4c1 (*) = [ .TRUE., (.FALSE.,i=2,6)    ]
      logical :: l4c2 (*) = [    (.TRUE.,i=1,5), .FALSE. ]
      logical :: l4c3 (*) = [ .TRUE., (.FALSE.,i=2,5), .TRUE. ]
      logical :: l4c4 (*) = [ (.TRUE.,i=1,3), (.FALSE.,i=4,6) ]
      logical :: l4d1 (*) = reshape([.TRUE.,.FALSE.],[1])
      logical :: l4d2 (*,*) = &
           & reshape([.TRUE.,.FALSE.,.TRUE.,.FALSE.],[2,2])
      logical :: l4d3 (*,*,*) = &
           & reshape([.FALSE.,.TRUE.,.FALSE.,.TRUE., &
           &          .FALSE.,.TRUE.,.FALSE.,.TRUE.],[2,2,2])
      logical :: l4d4 (*,*,*,*) = &
           & reshape([.TRUE.,.FALSE.,.TRUE.,.FALSE., &
           &          .TRUE.,.FALSE.,.TRUE.,.FALSE., &
           &          .TRUE.,.FALSE.,.TRUE.,.FALSE., &
           &          .TRUE.,.FALSE.,.TRUE.,.FALSE.],[2,2,2,2])

      ! Byte
      byte :: b4c1 (*) = [ 1, (i,i=2,6)    ]
      byte :: b4c2 (*) = [    (i,i=1,5), 6 ]
      byte :: b4c3 (*) = [ 1, (i,i=2,5), 6 ]
      byte :: b4c4 (*) = [ (i,i=1,3), (i,i=4,6) ]
      byte :: b4d1 (*) = reshape([1,2],[1])
      byte :: b4d2 (*,*) = reshape([1,2,3,4],[2,2])
      byte :: b4d3 (*,*,*) = &
           & reshape([1,2,3,4,5,6,7,8],[2,2,2])
      byte :: b4d4 (*,*,*,*) = &
           & reshape([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16],[2,2,2,2])


      ! Derived type
      type(base) :: tb4c1 (*) = &
           & [ base(1,1), (base(i,i),i=2,6) ]
      type(base) :: tb4c2 (*) = &
           & [ (base(i,i),i=1,5), base(6,6) ]
      type(base) :: tb4c3 (*) = &
           & [ base(1,1), (base(i,i),i=2,5), base(6,6) ]
      type(base) :: tb4c4 (*) = &
           & [ (base(i,i),i=1,3), (base(i,i),i=4,6) ]
      type(base) :: tb4d1 (*) = reshape([base(), base(10,11)],[2])
      type(base) :: tb4d2 (*,*) = &
           & reshape([base(), base(3,3), base(4,4), base(10,11)],[2,2])
      type(base) :: tb4d3 (*,*,*) = &
           & reshape([(base(i,i),i=1,8)],[2,2,2])
      type(base) :: tb4d4 (*,*,*,*) = &
           & reshape([(base(i,i),i=1,16)],[2,2,2,2])
      end

      program impliedshape03d

      type base
        integer :: a    = 1
        integer :: b    = 2
      end type

      ! Integer
      integer :: i4c1 (*) = [ 1, (i,i=2,6)    ]
      integer :: i4c2 (*) = [    (i,i=1,5), 6 ]
      integer :: i4c3 (*) = [ 1, (i,i=2,5), 6 ]
      integer :: i4c4 (*) = [ (i,i=1,3), (i,i=4,6) ]
      integer :: i4d1 (*) = reshape([1,2],[1])
      integer :: i4d2 (*,*) = reshape([1,2,3,4],[2,2])
      integer :: i4d3 (*,*,*) = &
           & reshape([1,2,3,4,5,6,7,8],[2,2,2])
      integer :: i4d4 (*,*,*,*) = &
           & reshape([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16],[2,2,2,2])

      ! Real
      real :: r4c1 (*) = [ 1.0, (real(i),i=2,6) ]
      real :: r4c2 (*) = [ (real(i),i=1,5), 6.0 ]
      real :: r4c3 (*) = [ 1.0, (real(i),i=2,5), 6.0 ]
      real :: r4c4 (*) = [ (i,i=1,3), (i,i=4,6) ]
      real :: r4d1 (*) = reshape([1.0,2.0],[1])
      real :: r4d2 (*,*) = reshape([1.0,2.0,3.0,4.0],[2,2])
      real :: r4d3 (*,*,*) = &
           & reshape([1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0],[2,2,2])
      real :: r4d4 (*,*,*,*) = &
           & reshape([1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0, &
                    & 11.0,12.0,13.0,14.0,15.0,16.0],[2,2,2,2])

      ! Double Precision
      double precision :: dp4c1 (*) = [ 1.0, (real(i),i=2,6) ]
      double precision :: dp4c2 (*) = [ (real(i),i=1,5), 6.0 ]
      double precision :: dp4c3 (*) = [ 1.0, (real(i),i=2,5), 6.0 ]
      double precision :: dp4c4 (*) = [ (i,i=1,3), (i,i=4,6) ]
      double precision :: dp4d1 (*) = reshape([1.0,2.0],[1])
      double precision :: dp4d2 (*,*) = reshape([1.0,2.0,3.0,4.0],[2,2])
      double precision :: dp4d3 (*,*,*) = &
           & reshape([1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0],[2,2,2])
      double precision :: dp4d4 (*,*,*,*) = &
           & reshape([1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0, &
                    & 11.0,12.0,13.0,14.0,15.0,16.0],[2,2,2,2])

      ! Complex
      complex :: cp4c1 (*) = [ 1, (i,i=2,6)    ]
      complex :: cp4c2 (*) = [    (i,i=1,5), 6 ]
      complex :: cp4c3 (*) = [ 1, (i,i=2,5), 6 ]
      complex :: cp4c4 (*) = [ (i,i=1,3), (i,i=4,6) ]
      complex :: cp4d1 (*) = reshape([1,2],[1])
      complex :: cp4d2 (*,*) = reshape([1,2,3,4],[2,2])
      complex :: cp4d3 (*,*,*) = &
           & reshape([1,2,3,4,5,6,7,8],[2,2,2])
      complex :: cp4d4 (*,*,*,*) = &
           & reshape([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16],[2,2,2,2])



      ! Character
      character :: ch4c1 (*) = [ 'a', ('b',i=2,6)    ]
      character :: ch4c2 (*) = [    ('a',i=1,5), 'b' ]
      character :: ch4c3 (*) = [ 'a', ('b',i=2,5), 'c' ]
      character :: ch4c4 (*) = [ ('a',i=1,3), ('b',i=4,6) ]
      character :: ch4d1 (*) = reshape(['a','b'],[1])
      character :: ch4d2 (*,*) = reshape(['a','b','c','d'],[2,2])
      character :: ch4d3 (*,*,*) = &
           & reshape(['a','b','c','d','e','f','g','h'],[2,2,2])
      character :: ch4d4 (*,*,*,*) = &
           & reshape(['a','b','c','d','e','f','g','h', &
           &          'i','j','k','l','m','n','o','p'],[2,2,2,2])


      ! Logical
      logical :: l4c1 (*) = [ .TRUE., (.FALSE.,i=2,6)    ]
      logical :: l4c2 (*) = [    (.TRUE.,i=1,5), .FALSE. ]
      logical :: l4c3 (*) = [ .TRUE., (.FALSE.,i=2,5), .TRUE. ]
      logical :: l4c4 (*) = [ (.TRUE.,i=1,3), (.FALSE.,i=4,6) ]
      logical :: l4d1 (*) = reshape([.TRUE.,.FALSE.],[1])
      logical :: l4d2 (*,*) = &
           & reshape([.TRUE.,.FALSE.,.TRUE.,.FALSE.],[2,2])
      logical :: l4d3 (*,*,*) = &
           & reshape([.FALSE.,.TRUE.,.FALSE.,.TRUE., &
           &          .FALSE.,.TRUE.,.FALSE.,.TRUE.],[2,2,2])
      logical :: l4d4 (*,*,*,*) = &
           & reshape([.TRUE.,.FALSE.,.TRUE.,.FALSE., &
           &          .TRUE.,.FALSE.,.TRUE.,.FALSE., &
           &          .TRUE.,.FALSE.,.TRUE.,.FALSE., &
           &          .TRUE.,.FALSE.,.TRUE.,.FALSE.],[2,2,2,2])

      ! Byte
      byte :: b4c1 (*) = [ 1, (i,i=2,6)    ]
      byte :: b4c2 (*) = [    (i,i=1,5), 6 ]
      byte :: b4c3 (*) = [ 1, (i,i=2,5), 6 ]
      byte :: b4c4 (*) = [ (i,i=1,3), (i,i=4,6) ]
      byte :: b4d1 (*) = reshape([1,2],[1])
      byte :: b4d2 (*,*) = reshape([1,2,3,4],[2,2])
      byte :: b4d3 (*,*,*) = &
           & reshape([1,2,3,4,5,6,7,8],[2,2,2])
      byte :: b4d4 (*,*,*,*) = &
           & reshape([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16],[2,2,2,2])


      ! Derived type
      type(base) :: tb4c1 (*) = &
           & [ base(1,1), (base(i,i),i=2,6) ]
      type(base) :: tb4c2 (*) = &
           & [ (base(i,i),i=1,5), base(6,6) ]
      type(base) :: tb4c3 (*) = &
           & [ base(1,1), (base(i,i),i=2,5), base(6,6) ]
      type(base) :: tb4c4 (*) = &
           & [ (base(i,i),i=1,3), (base(i,i),i=4,6) ]
      type(base) :: tb4d1 (*) = reshape([base(), base(10,11)],[2])
      type(base) :: tb4d2 (*,*) = &
           & reshape([base(), base(3,3), base(4,4), base(10,11)],[2,2])
      type(base) :: tb4d3 (*,*,*) = &
           & reshape([(base(i,i),i=1,8)],[2,2,2])
      type(base) :: tb4d4 (*,*,*,*) = &
           & reshape([(base(i,i),i=1,16)],[2,2,2,2])

      contains
      subroutine foo()
      type base
        integer :: a    = 1
        integer :: b    = 2
      end type

      ! Integer
      integer :: i4c1 (*) = [ 1, (i,i=2,6)    ]
      integer :: i4c2 (*) = [    (i,i=1,5), 6 ]
      integer :: i4c3 (*) = [ 1, (i,i=2,5), 6 ]
      integer :: i4c4 (*) = [ (i,i=1,3), (i,i=4,6) ]
      integer :: i4d1 (*) = reshape([1,2],[1])
      integer :: i4d2 (*,*) = reshape([1,2,3,4],[2,2])
      integer :: i4d3 (*,*,*) = &
           & reshape([1,2,3,4,5,6,7,8],[2,2,2])
      integer :: i4d4 (*,*,*,*) = &
           & reshape([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16],[2,2,2,2])

      ! Real
      real :: r4c1 (*) = [ 1.0, (real(i),i=2,6) ]
      real :: r4c2 (*) = [ (real(i),i=1,5), 6.0 ]
      real :: r4c3 (*) = [ 1.0, (real(i),i=2,5), 6.0 ]
      real :: r4c4 (*) = [ (i,i=1,3), (i,i=4,6) ]
      real :: r4d1 (*) = reshape([1.0,2.0],[1])
      real :: r4d2 (*,*) = reshape([1.0,2.0,3.0,4.0],[2,2])
      real :: r4d3 (*,*,*) = &
           & reshape([1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0],[2,2,2])
      real :: r4d4 (*,*,*,*) = &
           & reshape([1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0, &
                    & 11.0,12.0,13.0,14.0,15.0,16.0],[2,2,2,2])

      ! Double Precision
      double precision :: dp4c1 (*) = [ 1.0, (real(i),i=2,6) ]
      double precision :: dp4c2 (*) = [ (real(i),i=1,5), 6.0 ]
      double precision :: dp4c3 (*) = [ 1.0, (real(i),i=2,5), 6.0 ]
      double precision :: dp4c4 (*) = [ (i,i=1,3), (i,i=4,6) ]
      double precision :: dp4d1 (*) = reshape([1.0,2.0],[1])
      double precision :: dp4d2 (*,*) = reshape([1.0,2.0,3.0,4.0],[2,2])
      double precision :: dp4d3 (*,*,*) = &
           & reshape([1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0],[2,2,2])
      double precision :: dp4d4 (*,*,*,*) = &
           & reshape([1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0, &
                    & 11.0,12.0,13.0,14.0,15.0,16.0],[2,2,2,2])

      ! Complex
      complex :: cp4c1 (*) = [ 1, (i,i=2,6)    ]
      complex :: cp4c2 (*) = [    (i,i=1,5), 6 ]
      complex :: cp4c3 (*) = [ 1, (i,i=2,5), 6 ]
      complex :: cp4c4 (*) = [ (i,i=1,3), (i,i=4,6) ]
      complex :: cp4d1 (*) = reshape([1,2],[1])
      complex :: cp4d2 (*,*) = reshape([1,2,3,4],[2,2])
      complex :: cp4d3 (*,*,*) = &
           & reshape([1,2,3,4,5,6,7,8],[2,2,2])
      complex :: cp4d4 (*,*,*,*) = &
           & reshape([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16],[2,2,2,2])



      ! Character
      character :: ch4c1 (*) = [ 'a', ('b',i=2,6)    ]
      character :: ch4c2 (*) = [    ('a',i=1,5), 'b' ]
      character :: ch4c3 (*) = [ 'a', ('b',i=2,5), 'c' ]
      character :: ch4c4 (*) = [ ('a',i=1,3), ('b',i=4,6) ]
      character :: ch4d1 (*) = reshape(['a','b'],[1])
      character :: ch4d2 (*,*) = reshape(['a','b','c','d'],[2,2])
      character :: ch4d3 (*,*,*) = &
           & reshape(['a','b','c','d','e','f','g','h'],[2,2,2])
      character :: ch4d4 (*,*,*,*) = &
           & reshape(['a','b','c','d','e','f','g','h', &
           &          'i','j','k','l','m','n','o','p'],[2,2,2,2])


      ! Logical
      logical :: l4c1 (*) = [ .TRUE., (.FALSE.,i=2,6)    ]
      logical :: l4c2 (*) = [    (.TRUE.,i=1,5), .FALSE. ]
      logical :: l4c3 (*) = [ .TRUE., (.FALSE.,i=2,5), .TRUE. ]
      logical :: l4c4 (*) = [ (.TRUE.,i=1,3), (.FALSE.,i=4,6) ]
      logical :: l4d1 (*) = reshape([.TRUE.,.FALSE.],[1])
      logical :: l4d2 (*,*) = &
           & reshape([.TRUE.,.FALSE.,.TRUE.,.FALSE.],[2,2])
      logical :: l4d3 (*,*,*) = &
           & reshape([.FALSE.,.TRUE.,.FALSE.,.TRUE., &
           &          .FALSE.,.TRUE.,.FALSE.,.TRUE.],[2,2,2])
      logical :: l4d4 (*,*,*,*) = &
           & reshape([.TRUE.,.FALSE.,.TRUE.,.FALSE., &
           &          .TRUE.,.FALSE.,.TRUE.,.FALSE., &
           &          .TRUE.,.FALSE.,.TRUE.,.FALSE., &
           &          .TRUE.,.FALSE.,.TRUE.,.FALSE.],[2,2,2,2])

      ! Byte
      byte :: b4c1 (*) = [ 1, (i,i=2,6)    ]
      byte :: b4c2 (*) = [    (i,i=1,5), 6 ]
      byte :: b4c3 (*) = [ 1, (i,i=2,5), 6 ]
      byte :: b4c4 (*) = [ (i,i=1,3), (i,i=4,6) ]
      byte :: b4d1 (*) = reshape([1,2],[1])
      byte :: b4d2 (*,*) = reshape([1,2,3,4],[2,2])
      byte :: b4d3 (*,*,*) = &
           & reshape([1,2,3,4,5,6,7,8],[2,2,2])
      byte :: b4d4 (*,*,*,*) = &
           & reshape([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16],[2,2,2,2])


      ! Derived type
      type(base) :: tb4c1 (*) = &
           & [ base(1,1), (base(i,i),i=2,6) ]
      type(base) :: tb4c2 (*) = &
           & [ (base(i,i),i=1,5), base(6,6) ]
      type(base) :: tb4c3 (*) = &
           & [ base(1,1), (base(i,i),i=2,5), base(6,6) ]
      type(base) :: tb4c4 (*) = &
           & [ (base(i,i),i=1,3), (base(i,i),i=4,6) ]
      type(base) :: tb4d1 (*) = reshape([base(), base(10,11)],[2])
      type(base) :: tb4d2 (*,*) = &
           & reshape([base(), base(3,3), base(4,4), base(10,11)],[2,2])
      type(base) :: tb4d3 (*,*,*) = &
           & reshape([(base(i,i),i=1,8)],[2,2,2])
      type(base) :: tb4d4 (*,*,*,*) = &
           & reshape([(base(i,i),i=1,16)],[2,2,2,2])
      end subroutine
      end
      subroutine bar()
      type base
        integer :: a    = 1
        integer :: b    = 2
      end type

      ! Integer
      integer :: i4c1 (*) = [ 1, (i,i=2,6)    ]
      integer :: i4c2 (*) = [    (i,i=1,5), 6 ]
      integer :: i4c3 (*) = [ 1, (i,i=2,5), 6 ]
      integer :: i4c4 (*) = [ (i,i=1,3), (i,i=4,6) ]
      integer :: i4d1 (*) = reshape([1,2],[1])
      integer :: i4d2 (*,*) = reshape([1,2,3,4],[2,2])
      integer :: i4d3 (*,*,*) = &
           & reshape([1,2,3,4,5,6,7,8],[2,2,2])
      integer :: i4d4 (*,*,*,*) = &
           & reshape([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16],[2,2,2,2])

      ! Real
      real :: r4c1 (*) = [ 1.0, (real(i),i=2,6) ]
      real :: r4c2 (*) = [ (real(i),i=1,5), 6.0 ]
      real :: r4c3 (*) = [ 1.0, (real(i),i=2,5), 6.0 ]
      real :: r4c4 (*) = [ (i,i=1,3), (i,i=4,6) ]
      real :: r4d1 (*) = reshape([1.0,2.0],[1])
      real :: r4d2 (*,*) = reshape([1.0,2.0,3.0,4.0],[2,2])
      real :: r4d3 (*,*,*) = &
           & reshape([1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0],[2,2,2])
      real :: r4d4 (*,*,*,*) = &
           & reshape([1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0, &
                    & 11.0,12.0,13.0,14.0,15.0,16.0],[2,2,2,2])

      ! Double Precision
      double precision :: dp4c1 (*) = [ dble(1.0), (dble(i),i=2,6) ]
      double precision :: dp4c2 (*) = [ (dble(i),i=1,5), dble(6.0) ]
      double precision :: dp4c3 (*) = [ dble(1.0), (dble(i),i=2,5), dble(6.0) ]
      double precision :: dp4c4 (*) = [ (dble(i),i=1,3), (dble(i),i=4,6) ]
      double precision :: dp4d1 (*) = reshape([1.0,2.0],[1])
      double precision :: dp4d2 (*,*) = reshape([1.0,2.0,3.0,4.0],[2,2])
      double precision :: dp4d3 (*,*,*) = &
           & reshape([1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0],[2,2,2])
      double precision :: dp4d4 (*,*,*,*) = &
           & reshape([1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0, &
                    & 11.0,12.0,13.0,14.0,15.0,16.0],[2,2,2,2])

      ! Complex
      complex :: cp4c1 (*) = [ 1, (i,i=2,6)    ]
      complex :: cp4c2 (*) = [    (i,i=1,5), 6 ]
      complex :: cp4c3 (*) = [ 1, (i,i=2,5), 6 ]
      complex :: cp4c4 (*) = [ (i,i=1,3), (i,i=4,6) ]
      complex :: cp4d1 (*) = reshape([1,2],[1])
      complex :: cp4d2 (*,*) = reshape([1,2,3,4],[2,2])
      complex :: cp4d3 (*,*,*) = &
           & reshape([1,2,3,4,5,6,7,8],[2,2,2])
      complex :: cp4d4 (*,*,*,*) = &
           & reshape([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16],[2,2,2,2])



      ! Character
      character :: ch4c1 (*) = [ 'a', ('b',i=2,6)    ]
      character :: ch4c2 (*) = [    ('a',i=1,5), 'b' ]
      character :: ch4c3 (*) = [ 'a', ('b',i=2,5), 'c' ]
      character :: ch4c4 (*) = [ ('a',i=1,3), ('b',i=4,6) ]
      character :: ch4d1 (*) = reshape(['a','b'],[1])
      character :: ch4d2 (*,*) = reshape(['a','b','c','d'],[2,2])
      character :: ch4d3 (*,*,*) = &
           & reshape(['a','b','c','d','e','f','g','h'],[2,2,2])
      character :: ch4d4 (*,*,*,*) = &
           & reshape(['a','b','c','d','e','f','g','h', &
           &          'i','j','k','l','m','n','o','p'],[2,2,2,2])


      ! Logical
      logical :: l4c1 (*) = [ .TRUE., (.FALSE.,i=2,6)    ]
      logical :: l4c2 (*) = [    (.TRUE.,i=1,5), .FALSE. ]
      logical :: l4c3 (*) = [ .TRUE., (.FALSE.,i=2,5), .TRUE. ]
      logical :: l4c4 (*) = [ (.TRUE.,i=1,3), (.FALSE.,i=4,6) ]
      logical :: l4d1 (*) = reshape([.TRUE.,.FALSE.],[1])
      logical :: l4d2 (*,*) = &
           & reshape([.TRUE.,.FALSE.,.TRUE.,.FALSE.],[2,2])
      logical :: l4d3 (*,*,*) = &
           & reshape([.FALSE.,.TRUE.,.FALSE.,.TRUE., &
           &          .FALSE.,.TRUE.,.FALSE.,.TRUE.],[2,2,2])
      logical :: l4d4 (*,*,*,*) = &
           & reshape([.TRUE.,.FALSE.,.TRUE.,.FALSE., &
           &          .TRUE.,.FALSE.,.TRUE.,.FALSE., &
           &          .TRUE.,.FALSE.,.TRUE.,.FALSE., &
           &          .TRUE.,.FALSE.,.TRUE.,.FALSE.],[2,2,2,2])

      ! Byte
      byte :: b4c1 (*) = [ 1, (i,i=2,6)    ]
      byte :: b4c2 (*) = [    (i,i=1,5), 6 ]
      byte :: b4c3 (*) = [ 1, (i,i=2,5), 6 ]
      byte :: b4c4 (*) = [ (i,i=1,3), (i,i=4,6) ]
      byte :: b4d1 (*) = reshape([1,2],[1])
      byte :: b4d2 (*,*) = reshape([1,2,3,4],[2,2])
      byte :: b4d3 (*,*,*) = &
           & reshape([1,2,3,4,5,6,7,8],[2,2,2])
      byte :: b4d4 (*,*,*,*) = &
           & reshape([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16],[2,2,2,2])


      ! Derived type
      type(base) :: tb4c1 (*) = &
           & [ base(1,1), (base(i,i),i=2,6) ]
      type(base) :: tb4c2 (*) = &
           & [ (base(i,i),i=1,5), base(6,6) ]
      type(base) :: tb4c3 (*) = &
           & [ base(1,1), (base(i,i),i=2,5), base(6,6) ]
      type(base) :: tb4c4 (*) = &
           & [ (base(i,i),i=1,3), (base(i,i),i=4,6) ]
      type(base) :: tb4d1 (*) = reshape([base(), base(10,11)],[2])
      type(base) :: tb4d2 (*,*) = &
           & reshape([base(), base(3,3), base(4,4), base(10,11)],[2,2])
      type(base) :: tb4d3 (*,*,*) = &
           & reshape([(base(i,i),i=1,8)],[2,2,2])
      type(base) :: tb4d4 (*,*,*,*) = &
           & reshape([(base(i,i),i=1,16)],[2,2,2,2])
      end subroutine
