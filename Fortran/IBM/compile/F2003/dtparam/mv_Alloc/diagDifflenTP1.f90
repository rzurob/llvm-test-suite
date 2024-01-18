! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM is of nonpoly of type base
!*                               TO is of poly of type base
!*                               Type parameters for TO/FROM are different
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


module m
   type base(l)
       integer, len :: l
   end type

   contains
      subroutine sub(arg, brg)
           type(base(l=5)), allocatable :: arg
           class( base(2)), allocatable :: brg

           call move_alloc(arg, brg)

           func = 10
      end subroutine
end module

use m

   type(base(5)),  allocatable :: a1
   class(base(l=2)), allocatable :: a2

   call sub(a1, a2)

end
