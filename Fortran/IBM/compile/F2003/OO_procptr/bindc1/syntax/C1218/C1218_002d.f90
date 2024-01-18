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
!*                                        C1218: BIND(C) specified, with interface,
!*                                               and interface with out BIND(C)
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

   use ISO_C_BINDING

   interface
      function foo1(i)
         import C_INT
         integer(C_INT) :: foo1, i
      end function
   end interface
   
   interface
      subroutine bar1(r)
         import C_FLOAT
         real(C_FLOAT) :: r
      end subroutine
   end interface

   procedure(foo1), pointer, BIND(C) :: b1
   procedure(bar1), pointer, BIND(C) :: b2
   procedure(foo1), BIND(C) :: b3
   procedure(bar1), BIND(C) :: b4

end
