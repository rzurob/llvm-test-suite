! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : typeboundpoly1.f 
!*
!*  PROGRAMMER                 : Michelle Zhang 
!*  DATE                       : 06/01/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*                              
!*
!*  DRIVER STANZA              : xlf2003
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
          type base
              contains
                  procedure, nopass ::  move => move_alloc1
          end type

          type, extends(base) :: child
              contains
                  procedure, nopass ::  move2 => move_alloc2
          end type

          interface
               subroutine move_alloc1( x, y)
                   import base 
                   class(base) , allocatable :: x(:,:,:,:,:,:),y(:,:,:,:,:,:)
               end subroutine
               subroutine move_alloc2( x,y)
                   import child 
                   class(child) , allocatable :: x(:,:,:,:,:,:),y(:,:,:,:,:,:)
               end subroutine
          end interface
      end module


      program main

      use m

      allocatable b1, b2, b 
      class(base) b1(:,:,:,:,:,:), b2(:,:,:,:,:,:), b
      type(child) c

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

      allocate(child :: b)
      allocate(b1(2,1,1,1,1,1))
      allocate(child:: b2(2:1,1:0,0,0,0,0))

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
          class(base) , allocatable :: x(:,:,:,:,:,:),y(:,:,:,:,:,:)
          numB = numB + 1 
          call move_alloc(x,y)
      end subroutine

      subroutine move_alloc2( x,y)
          use m
          class(child) , allocatable :: x(:,:,:,:,:,:),y(:,:,:,:,:,:)
          numC = numC + 1
          call move_alloc(y,x) 
      end subroutine
