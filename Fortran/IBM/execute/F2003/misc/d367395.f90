!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : GENERICS
!*
!*  PROGRAMMER                 : Gaby Baghdadi
!*  DATE                       : 13/07/2009
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*  DESCRIPTION                : Reduced test case from original TC given 
!*                               in defect 346625.
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

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
      end function

      subroutine sub ( dtv, c )
         class(base(4)), intent(inout) :: dtv
         character(len=dtv%getl()), intent(in) :: c      ! <--- complains here
         if (len(c) <> 12) error stop 1_4
         dtv%x(1:dtv%getl()) = c
      end subroutine
end module

use m
character(26) :: c = "abcdefghijklmnopqrstuvwxyz"
type(base(4)) :: b1
b1 = base(4)( 12, 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx' )
call sub ( b1, c )
if ( b1%x /= 'abcdefghijklxxxxxxxxxxxxxxxxxx' ) error stop 2_4
end
