! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : diagDifflenTP.f 
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
!*  DESCRIPTION                : FROM is of nonpoly of type child 
!*                               TO is of poly of type base
!*                               Type parameters for TO/FROM are different
!*				 defect 323068
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

   type, extends(base) :: child 
   end type

   contains 
      subroutine sub(arg, brg)
           type(child(l=3)), allocatable :: arg
           class( base(:)), allocatable :: brg

           call move_alloc(arg, brg)

           func = 10 
      end subroutine 
end module

use m
      
   type(child(l=3)),  allocatable :: a1
   class(base(:)), allocatable :: a2

   call sub(a1, a2)

end
