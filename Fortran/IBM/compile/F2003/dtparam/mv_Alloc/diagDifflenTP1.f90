! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : diagDifflenTP1.f 
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
