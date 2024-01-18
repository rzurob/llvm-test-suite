! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/generic/misc/genericMisc004.f
! opt variations: -ql

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
   type base(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: i
      contains
         procedure, pass :: bassgn
         final :: bfinal
   end type

contains

   elemental subroutine bassgn ( a, b )
      class(base(4)), intent(out) :: a
      type(base(4)), intent(in)  :: b

   end subroutine

   pure subroutine bfinal( a )
      type(base(4)), intent(inout) :: a

   end subroutine

end module


program genericMisc004
   use m

   type(base(4)) :: b1(4), b2
   call bassgn ( b1, b1 )
   
   call bassgn ( b2, base(4)(10) )
   call bassgn ( b1, (/ b2, base(4)(20), base(4)(30), base(4)(40) /) )

end
