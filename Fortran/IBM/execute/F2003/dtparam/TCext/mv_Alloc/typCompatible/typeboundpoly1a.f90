! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/F2003/mv_Alloc/typCompatible/typeboundpoly1a.f
! opt variations: -qnok -ql

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM is polymorphic type, TO is class(*)
!*                               move_alloc is called in type-bound procedure
!*                               defined by module proc
!*
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


      module m
          integer ::  numC = 0, numB = 0
          type base(k1)    ! (4)
              integer, kind :: k1
              contains
                  procedure, nopass ::  move => mv_alloc1
          end type

          type, extends(base) :: child    ! (4)
              contains
                  procedure, nopass ::  move => mv_alloc2
          end type

          contains
              subroutine mv_alloc1( x,y)
                 class(base(4)) , allocatable :: y(:,:,:,:,:)
                 class(*) , allocatable :: x(:,:,:,:,:)
                 numB = numB + 1
                 call move_alloc(y,x)
              end subroutine
              subroutine mv_alloc2( x,y)
                 class(base(4)) , allocatable :: y(:,:,:,:,:)
                 class(*) , allocatable :: x(:,:,:,:,:)
                 numC = numC + 1
                 call move_alloc(y,x)
              end subroutine
      end module


      program main

      use m

      allocatable b1, b2, b
      class(base(4)) b2(:,:,:,:,:), b
      class(*) b1(:,:,:,:,:)
      type(child(4)) c

      allocate(child(4) :: b)
      allocate(base(4) :: b1(2:1,3:2,4:-2,5:0,6:5))
      allocate(child(4):: b2(1,2,3,4,5))

      ! TO of unlimit poly, FROM of type child
      call b%move(b1, b2)

      if ( .not. same_type_as(b1,c)) stop 30

      if ( .not. allocated(b1) ) stop 31
      if ( allocated(b2) ) stop 33
      print *, shape(b1)

      if ( numC /= 1 ) stop 43
      if ( numB /= 0 ) stop 45
      end

