!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 06/07/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Testing: Procedure Pointer with BIND(C) feature
!*                                        Dummy Argument being Procedure pointer with bind(C) interface
!*                                        argument argument with different interface (same interface, but NOT bind(C))
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

   use ISO_C_BINDING

   type, bind(C) :: base
      integer(C_INT) :: i1, i2
   end type

   interface
      subroutine inf()
      end subroutine
   end interface

   interface
      subroutine print1() bind(c)
      end subroutine
   end interface

   interface
      subroutine print2() bind(c)
      end subroutine
   end interface

end module

   use m

   procedure(inf), pointer :: p1

   call setpp1(p1)
   call p1()

   call setpp2(p1)
   call p1()

   contains

      subroutine setpp1(a)
         procedure(print1), pointer :: a

         a => print1

      end subroutine

      subroutine setpp2(a)
         procedure(print2), pointer :: a

         a => print2

      end subroutine

end
