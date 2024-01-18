! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : mixunlmtpolyDT2.f 
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
!*  DESCRIPTION                : FROM is of an nonpoly DT 
!*                               TO is of unlmited poly
!*                               both are dummy args
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

      type  :: base
          integer*8 :: id
          character(2) :: name
      end type 

      contains
          subroutine sub(a, b)
              type(base), allocatable :: a(:,:)
              class(*), allocatable  :: b(:,:)

              call move_alloc(a, b)
          end subroutine

end module

use m
      integer i
      type(base), allocatable :: a(:,:)
      class(*), allocatable :: b(:,:)
      allocate(a(5,2), source=reshape((/ (base(i, char(i+70)), i=1,10) /) , &
           (/5, 2/) ))
      call sub(a, b)

      if ( .not. allocated(b)) stop 21
      if ( allocated (a)) stop 23
      select type(b)
          type is (base)
              print *, ( (/ ( b(i,1)%id, i = 1,5) /) )
              print *, ( (/ ( b(i,1)%name, i = 1,5) /) )
              print *, ( (/ ( b(i,2)%id, i = 1,5) /) )
              print *, ( (/ ( b(i,2)%name, i = 1,5) /) )
          class default
             stop 51
      end select

      end
