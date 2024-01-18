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
!*                                        BIND(C) Procedure Pointer pointing at FORTRAN functions with BIND(C) attribute
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

   interface
      integer(C_INT) function intinterface1(i) bind(c)
         import C_INT
         integer(C_INT), intent(in) :: i
      end function
   end interface

   interface
      subroutine intinterface2(i) bind(c)
         import C_INT
         integer(C_INT), intent(in) :: i
      end subroutine
   end interface

   procedure(intinterface1), pointer :: fptr1

   contains

   integer(C_INT) function times2(i) bind(c)
      integer(C_INT), intent(in) :: i
      times2 = i * 2
   end function

   subroutine print2(i) bind(c)
      integer(C_INT), intent(in) :: i
      print *, i, i
   end subroutine

end module

   use m

   integer(C_INT) :: sum1, sum2
   procedure(intinterface2), pointer :: fptr2

   fptr1 => times2
   fptr2 => print2

   sum1 = fptr1 ( 123_C_INT )
   if ( sum1 /= 246 ) error stop 1_4

   call fptr2( 123 )

end


