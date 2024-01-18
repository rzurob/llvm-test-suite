!* =================================================================== &
!*
!* DATE                       : February 14, 2011
!* ORIGIN                     : AIX Compiler Development,
!*
!* PRIMARY FUNCTIONS TESTED   : Implied-shape arrays
!*
!* DESCRIPTION                : Testing proper functionality of
!*                              implied-shape arrays with array
!*                              intrinsics
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      program impliedshape03f

      ! Integer:
      integer, parameter :: i4c1 (*) = [ 1, (i,i=2,6)    ]
      integer, parameter :: i4c2 (*) = [    (i,i=1,5), 6 ]
      integer, parameter :: i4c3 (*) = [ 1, (i,i=2,5), 6 ]
      integer, parameter :: i4c4 (*) = [ (i,i=1,3), (i,i=4,6) ]

      integer, parameter :: i4d1 (*)       = &
      & reshape([1,2],[1])
      integer, parameter :: i4d2 (*,*)     = &
      & reshape([1,2,3,4],[2,2])
      integer, parameter :: i4d3 (*,*,*)   = &
      & reshape([1,2,3,4,5,6,7,8],[2,2,2])
      integer, parameter :: i4d4 (*,*,*,*) = &
      & reshape([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16],[2,2,2,2])

      integer, parameter :: i4e1 (ubound(i4d4,1):*, &
      &                           ubound(i4d4,2):*, &
      &                           ubound(i4d4,3):*, &
      &                           ubound(i4d4,4):*) = i4d4

      integer, parameter :: i4e2 (ubound(i4e1,1):*, &
      &                           ubound(i4e1,2):*, &
      &                           ubound(i4e1,3):*, &
      &                           ubound(i4e1,4):*) = &
      & reshape([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16],shape(i4e1))

      integer, parameter :: i4e3 (lbound(i4e2,1):*, &
      &                           lbound(i4e2,2):*, &
      &                           ubound(i4e2,3):*, &
      &                           lbound(i4e2,4):*) = &
      & reshape([(i,i=1,size(i4e2))],shape(i4e1))

      integer, parameter :: i4e4 (2,2,2,2) = i4e3

      integer, parameter :: i4e5 (size(i4e3)) = reshape (i4e4,[size(i4e4)])

      integer, parameter :: i4e6 (40:*,50:*,60:*,70:*) = &
      & reshape([(i,i=1,size(i4e3))],shape(i4e3))

      integer, parameter :: i4e7 (140:*,150:*,160:*,170:*) = i4e6

      integer(size(i4d3)), parameter :: i    = size(i4e1)
      integer(size(i4d3)), parameter :: j(*) = shape(i4e2)
      integer(size(i4d2)), parameter :: k(*) = lbound(i4e3)
      integer(size(i4d2)), parameter :: l(*) = ubound(i4e4)

      if (size(i4e6)      .NE. size(i4e7)   .OR. &
        & ANY(shape(i4e6) .NE. shape(i4e7)) .OR. &
        & ANY(i4e6        .NE. i4e7)) then
        ERROR STOP 1
      endif

      print *, i4e1
      print *, i4e2
      print *, i4e3
      print *, i4e4
      print *, i4e5
      print *, i4e6
      print *, i4e7
      print *, i,j,k,l
      end
