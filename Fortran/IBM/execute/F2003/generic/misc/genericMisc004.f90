!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 11/01/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*                             :
!*  SECONDARY FUNCTIONS TESTED : misc.
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Elemental subroutine containing
!*                               intent(out) attribute and Finalization
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
   type base
      integer :: i
      contains
         procedure, pass :: bassgn
         final :: bfinal
   end type

contains

   elemental subroutine bassgn ( a, b )
      class(base), intent(out) :: a
      type(base), intent(in)  :: b

   end subroutine

   pure subroutine bfinal( a )
      type(base), intent(inout) :: a

   end subroutine

end module


program genericMisc004
   use m

   type(base) :: b1(4), b2
   call bassgn ( b1, b1 )
   
   call bassgn ( b2, base(10) )
   call bassgn ( b1, (/ b2, base(20), base(30), base(40) /) )

end
