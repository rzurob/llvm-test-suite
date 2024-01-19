!* =================================================================== &
!*
!* DATE                       : February 14, 2011
!*
!* PRIMARY FUNCTIONS TESTED   : Implied-shape arrays
!*
!* DESCRIPTION                : Testing proper diagnostics of
!*                              implied-shape arrays with langlvls
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      program impliedshape06d

      type base
        integer :: a    = 1
        integer :: b    = 2
      end type

      ! Integer
      integer, parameter :: i4d1 (*) = reshape([1,2],[1])
      integer, parameter :: i4d2 (*,*) = reshape([1,2,3,4],[2,2])
      integer, parameter :: i4d3 (*,*,*) = &
           & reshape([1,2,3,4,5,6,7,8],[2,2,2])
      integer, parameter :: i4d4 (*,*,*,*) = &
           & reshape([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16],[2,2,2,2])

      real, parameter :: r4d1 (*) = reshape([1.0,2.0],[1])
      real, parameter :: r4d2 (*,*) = reshape([1.0,2.0,3.0,4.0],[2,2])
      real, parameter :: r4d3 (*,*,*) = &
           & reshape([1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0],[2,2,2])
      real, parameter :: r4d4 (*,*,*,*) = &
           & reshape([1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0, &
                    & 11.0,12.0,13.0,14.0,15.0,16.0],[2,2,2,2])

      double precision, parameter :: dp4d1 (*) = reshape([1.0,2.0],[1])
      double precision, parameter :: dp4d2 (*,*) = reshape([1.0,2.0,3.0,4.0],[2,2])
      double precision, parameter :: dp4d3 (*,*,*) = &
           & reshape([1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0],[2,2,2])
      double precision, parameter :: dp4d4 (*,*,*,*) = &
           & reshape([1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0, &
                    & 11.0,12.0,13.0,14.0,15.0,16.0],[2,2,2,2])

      complex, parameter :: cp4d1 (*) = reshape([1,2],[1])
      complex, parameter :: cp4d2 (*,*) = reshape([1,2,3,4],[2,2])
      complex, parameter :: cp4d3 (*,*,*) = &
           & reshape([1,2,3,4,5,6,7,8],[2,2,2])
      complex, parameter :: cp4d4 (*,*,*,*) = &
           & reshape([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16],[2,2,2,2])

      character, parameter :: ch4d1 (*) = reshape(['a','b'],[1])
      character, parameter :: ch4d2 (*,*) = reshape(['a','b','c','d'],[2,2])
      character, parameter :: ch4d3 (*,*,*) = &
           & reshape(['a','b','c','d','e','f','g','h'],[2,2,2])
      character, parameter :: ch4d4 (*,*,*,*) = &
           & reshape(['a','b','c','d','e','f','g','h', &
           &          'i','j','k','l','m','n','o','p'],[2,2,2,2])

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

      type(base), parameter :: tb4d1 (*) = reshape([base(), base(10,11)],[2])
      type(base), parameter :: tb4d2 (*,*) = &
           & reshape([base(), base(3,3), base(4,4), base(10,11)],[2,2])
      type(base), parameter :: tb4d3 (*,*,*) = &
           & reshape([(base(i,i),i=1,8)],[2,2,2])
      type(base), parameter :: tb4d4 (*,*,*,*) = &
           & reshape([(base(i,i),i=1,16)],[2,2,2,2])

      end
