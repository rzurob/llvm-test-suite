! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : realkind8a.f 
!*
!*  PROGRAMMER                 : Michelle Zhang 
!*  DATE                       : 06/13/2006
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
!*  DESCRIPTION                : FROM is of type real*8
!*                               TO is  of unlimited poly 
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
      type base
          class(*), allocatable :: r
          real*8, allocatable :: var
      end type 

      interface 
           subroutine sub(arg)
               import base
               type(base) :: arg
           end subroutine
      end interface
end module

      use m

      logical precision_r8

      type(base), allocatable :: b

      allocate(b)

      call sub(b)

      call move_alloc(b%var, b%r)

      if ( allocated(b%var) ) stop 21
      if ( .not. allocated(b%r) ) stop 25

      select type( x => b%r )
            type is (real*8)
                write (*, '(f15.8)' ) b%var
      end select

      end 

           subroutine sub(arg)
               use m, only:base
               type(base) :: arg
               real*8, allocatable :: r1

               allocate(arg%r, source = dprod(1.1_4, 2.2_4) )
               allocate(r1, source=-3.3_8) 
               allocate(arg%var, source = r1 )

           end subroutine
