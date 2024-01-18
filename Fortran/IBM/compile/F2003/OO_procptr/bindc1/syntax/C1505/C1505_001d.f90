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
!*                                        C1505: Define BIND(C) type with procedure pointers
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
      subroutine foo(i) BIND(C)
         import C_INT
         integer(C_INT) :: i
      end subroutine
   end interface
   
   interface
      function foo1() BIND(C)
         import C_INT
         integer(C_INT) :: foo1
      end function
   end interface

   type, BIND(C) :: base
      integer(C_INT) :: i
      procedure(foo), pointer, nopass :: p1
   end type
   
   type, BIND(C) :: base2
      integer(C_INT) :: j
      procedure(foo1), pointer, nopass :: p1
   end type

end
