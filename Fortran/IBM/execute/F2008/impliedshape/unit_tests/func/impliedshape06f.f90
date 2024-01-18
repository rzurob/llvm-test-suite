!* =================================================================== &
!*
!* DATE                       : February 14, 2011
!* ORIGIN                     : AIX Compiler Development,
!*
!* PRIMARY FUNCTIONS TESTED   : Implied-shape arrays
!*
!* DESCRIPTION                : Testing proper functionality of
!*                              implied-shape arrays differing
!*                              declaration statements
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      program impliedshape06f

      type base
        integer :: a    = 1
        integer :: b    = 2
      end type

      ! The PARAMETER statement must appear last as stated in the F2008
      ! standard, section 5.3.13.2:
      ! A named constant shall not be referenced unless it has been
      ! defined previously in the same statement, defined in
      ! a prior statement, or made accessible by use or host
      ! association.

      integer :: i4c4, r4c4
      dimension :: i4c4(*), r4c4(*)
      PARAMETER ( i4c4 = [ (i,i=1,3), (i,i=4,6) ], &
                & r4c4 = [ (i,i=1,3), (i,i=4,6) ] )

      dimension :: i4c4a(*), r4c4a(*)
      integer, parameter :: i4c4a = [ (i,i=1,3), (i,i=4,6) ]
      real,    parameter :: r4c4a = [ (i,i=1,3), (i,i=4,6) ]

      dimension :: i4d4(*,*,*,*), r4d4(*,*,*,*), dp4c4 (*), dp4d4 (*,*,*,*)
      integer :: i4d4
      real :: r4d4
      double precision :: dp4c4, dp4d4
      parameter ( i4d4 =                                               &
        & reshape([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16],[2,2,2,2]), &
        & r4d4 = reshape([1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,    &
        &                 11.0,12.0,13.0,14.0,15.0,16.0],[2,2,2,2]),   &
        & dp4c4 = [ (i,i=1,3), (i,i=4,6) ],                            &
        & dp4d4 = reshape([1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,   &
        &                  11.0,12.0,13.0,14.0,15.0,16.0],[2,2,2,2]) )

      complex :: cp4c4, cp4d4
      dimension :: cp4c4 (*), cp4d4 (*,*,*,*)
      parameter ( cp4c4 = [ (i,i=1,3), (i,i=4,6) ], cp4d4 = &
         & reshape([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16],[2,2,2,2]) )

      dimension :: ch4c4 (*), ch4d4 (*,*,*,*), ch4c44 (*)
      dimension :: ch4d44 (*,*,*,*), ch4c4s (*), ch4d4s (*,*,*,*)

      character    :: ch4c4 , ch4d4
      character(4) :: ch4c44, ch4d44
      character(*) :: ch4c4s, ch4d4s

      parameter ( ch4c4 = [ ('a',i=1,3), ('b',i=4,6) ], ch4d4 =  &
        & reshape(['a','b','c','d','e','f','g','h',              &
        &          'i','j','k','l','m','n','o','p'],[2,2,2,2]),  &
        & ch4c44 = ch4c4, ch4d44 = ch4d4, ch4c4s = ch4c44,       &
        & ch4d4s = reshape(['a','b','c','d','e','f','g','h',     &
        &          'i','j','k','l','m','n','o','p'],[2,2,2,2]) )

      logical :: l4c4, l4d4
      dimension :: l4c4 (*), l4d4 (*,*,*,*)
      parameter ( l4c4 = [ (.TRUE.,i=1,3), (.FALSE.,i=4,6) ], l4d4 =  &
       & reshape([.TRUE.,.FALSE.,.TRUE.,.FALSE., &
       &          .TRUE.,.FALSE.,.TRUE.,.FALSE., &
       &          .TRUE.,.FALSE.,.TRUE.,.FALSE., &
       &          .TRUE.,.FALSE.,.TRUE.,.FALSE.],[2,2,2,2]) )

      dimension :: b4c4 (*), b4d4 (*,*,*,*)
      byte :: b4c4, b4d4
      parameter ( b4c4 = [ (i,i=1,3), (i,i=4,6) ], b4d4 = &
       & reshape([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16],[2,2,2,2]) )

      dimension :: tb4c4 (*), tb4d4 (*,*,*,*)
      type(base) :: tb4c4, tb4d4
      parameter ( &
      & tb4c4 = [ (base(i,i),i=1,3), (base(i,i),i=4,6) ], &
      & tb4d4 = reshape([(base(i,i),i=1,16)],[2,2,2,2]) )

      type(base) :: tb4c4a, tb4d4a
      dimension :: tb4c4a (*), tb4d4a (*,*,*,*)
      parameter ( &
      & tb4c4a = [ (base(i,i),i=1,3), (base(i,i),i=4,6) ], &
      & tb4d4a = reshape([(base(i,i),i=1,16)],[2,2,2,2]) )

      dimension :: tb4c4b (*), tb4d4b (*,*,*,*)
      type(base), parameter :: tb4c4b = [ (base(i,i),i=1,3), (base(i,i),i=4,6) ], &
      &                        tb4d4b = reshape([(base(i,i),i=1,16)],[2,2,2,2])



      print *, i4c4
      print *, i4d4
      print *, i4c4a
      print *, r4c4a
      print *, r4c4
      print *, r4d4
      print *, dp4c4
      print *, dp4d4
      print *, cp4c4
      print *, cp4d4
      print *, ch4c4
      print *, ch4d4
      print *, ch4c44
      print *, ch4d44
      print *, ch4c4s
      print *, ch4d4s
      print *, l4c4
      print *, l4d4
      print *, b4c4
      print *, b4d4
      print *, tb4c4
      print *, tb4d4
      print *, tb4c4a
      print *, tb4d4a
      print *, tb4c4b
      print *, tb4d4b

      end
