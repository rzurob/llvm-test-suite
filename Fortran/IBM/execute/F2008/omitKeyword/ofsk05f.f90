!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : ofsk05f.f
!*
!*  PROGRAMMER                 : Jin Li
!*  DATE                       : 2010-09-30
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : F2008 Omit FUNCTION and SUBROUTINE Keywords
!*  REFERENCE                  : Feature Number 376084
!*  REQUIRED COMPILER OPTIONS  : noopt
!*
!*  DESCRIPTION
!*
!*  A subroutine and a pure function within a module are terminated by "END"
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module m
   type base(t1)
      integer, kind :: t1
      integer(t1)   :: l
      character(30) :: x
      contains
         procedure, pass :: puregetl
         generic :: getl => puregetl
   end type

   contains
      integer pure function puregetl ( dtv )
         class(base(4)), intent(in) :: dtv
         puregetl = dtv%l
      end 

      subroutine sub ( dtv, c )
         class(base(4)), intent(inout) :: dtv
         character(len=dtv%getl()), intent(in) :: c      ! <--- complains here
         if (len(c) <> 12) error stop 1_4
         dtv%x(1:dtv%getl()) = c
      end
end module

use m
character(26) :: c = "abcdefghijklmnopqrstuvwxyz"
type(base(4)) :: b1
b1 = base(4)( 12, 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx' )
call sub ( b1, c )
if ( b1%x /= 'abcdefghijklxxxxxxxxxxxxxxxxxx' ) error stop 2_4
end
