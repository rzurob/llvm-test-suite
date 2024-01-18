! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :  typeboundpoly1a.f
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
          type base
              contains
                  procedure, nopass ::  move => mv_alloc1
          end type

          type, extends(base) :: child
              contains
                  procedure, nopass ::  move => mv_alloc2
          end type

          contains
              subroutine mv_alloc1( x,y)
                 class(base) , allocatable :: y(:,:,:,:,:)
                 class(*) , allocatable :: x(:,:,:,:,:)
                 numB = numB + 1
                 call move_alloc(y,x) 
              end subroutine
              subroutine mv_alloc2( x,y)
                 class(base) , allocatable :: y(:,:,:,:,:)
                 class(*) , allocatable :: x(:,:,:,:,:)
                 numC = numC + 1
                 call move_alloc(y,x) 
              end subroutine
      end module


      program main

      use m

      allocatable b1, b2, b 
      class(base) b2(:,:,:,:,:), b
      class(*) b1(:,:,:,:,:)
      type(child) c

      allocate(child :: b)
      allocate(base :: b1(2:1,3:2,4:-2,5:0,6:5))
      allocate(child:: b2(1,2,3,4,5))

      ! TO of unlimit poly, FROM of type child 
      call b%move(b1, b2)
      
      if ( .not. same_type_as(b1,c)) stop 30

      if ( .not. allocated(b1) ) stop 31 
      if ( allocated(b2) ) stop 33 
      print *, shape(b1) 
      
      if ( numC /= 1 ) stop 43
      if ( numB /= 0 ) stop 45
      end

