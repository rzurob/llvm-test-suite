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
!*                                        multiple levels of calls on FORTRAN and C side with procptr and func ptr
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
      subroutine foo1(i) bind(c, name="C_FOO1")
         import C_INT
         integer(C_INT), intent(inout) :: i
      end subroutine
   end interface

   integer(C_INT) :: i = 1001_C_INT

   procedure(foo1), pointer :: pp1
   pp1 => foo1

   call pp1(i)

   print *, "calls completed:", i

end

subroutine foo2(i) bind(C)

   use ISO_C_BINDING, only: C_INT

   integer(C_INT), intent(inout) :: i

   interface
      subroutine foo3(i) bind(c)
         import C_INT
         integer(C_INT), intent(inout) :: i
      end subroutine
   end interface

   procedure(foo3), pointer :: pp2

   print *,"inside foo2:",i
   pp2 => foo3
   
   i = i + 1

   call pp2(i)

end subroutine

subroutine foo5(i) bind(C)

   use ISO_C_BINDING, only: C_INT

   integer(C_INT), intent(inout) :: i

   print *,"inside foo5:",i

   i = i + 1

end subroutine
