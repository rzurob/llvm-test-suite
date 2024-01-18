! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodeferredlp /tstdev/F2003/mv_Alloc/typCompatible/typeboundpoly1.f
! opt variations: -qnok -qnol -qdeferredlp

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM/TO are polymorphic type
!*                               move_alloc is called in type-bound procedure
!*                               defined by external proc
!*
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


      module m
          integer :: numB = 0, numC = 0
          type base(k1,n1)    ! (4,20)
              integer, kind :: k1
              integer, len  :: n1
              contains
                  procedure, nopass ::  move => move_alloc1
          end type

          type, extends(base) :: child    ! (4,20)
              contains
                  procedure, nopass ::  move2 => move_alloc2
          end type

          interface
               subroutine move_alloc1( x, y)
                   import base
                   class(base(4,*)) , allocatable :: x(:,:,:,:,:,:),y(:,:,:,:,:,:)
               end subroutine
               subroutine move_alloc2( x,y)
                   import child
                   class(child(4,*)) , allocatable :: x(:,:,:,:,:,:),y(:,:,:,:,:,:)
               end subroutine
          end interface
      end module


      program main

      use m

      allocatable b1, b2, b
      class(base(4,20)) b1(:,:,:,:,:,:), b2(:,:,:,:,:,:), b
      type(child(4,20)) c

      allocate(b)
      allocate(b1(2,1,1,1,1,1))
      allocate(b2(3,3,3,3,3,3))

      ! TO of type base, FROM of type base
      call b%move(b1, b2)

      if ( allocated(b1) ) stop 21
      if ( .not. allocated(b2) ) stop 23
      print *, shape(b2)

      deallocate(b)
      deallocate(b2)

      allocate(child(4,20) :: b)
      allocate(b1(2,1,1,1,1,1))
      allocate(child(4,20):: b2(2:1,1:0,0,0,0,0))

      ! TO of type base, FROM of type child
      call b%move(b2, b1)

      if ( .not. same_type_as(b1,c)) stop 30

      if ( .not. allocated(b1) ) stop 31
      if ( allocated(b2) ) stop 33
      print *, shape(b1)

      if ( numB /= 2 ) stop 41
      if ( numC /= 0 ) stop 43
      end

      subroutine move_alloc1( x, y)
          use m
          class(base(4,*)) , allocatable :: x(:,:,:,:,:,:),y(:,:,:,:,:,:)
          numB = numB + 1
          call move_alloc(x,y)
      end subroutine

      subroutine move_alloc2( x,y)
          use m
          class(child(4,*)) , allocatable :: x(:,:,:,:,:,:),y(:,:,:,:,:,:)
          numC = numC + 1
          call move_alloc(y,x)
      end subroutine
