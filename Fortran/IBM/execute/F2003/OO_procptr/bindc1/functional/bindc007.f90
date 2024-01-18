!*  ===================================================================
!*
!*  DATE                       : 06/07/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Procedure Pointer with BIND(C) feature
!*                                        Procedure Pointer pointing at FORTRAN functions which returns a procedure pointer that
!*                                        is pointing to C functions with character dummy arguments (C_CHAR)
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

   use ISO_C_BINDING, only: C_CHAR

   interface
      character(kind=C_CHAR) function touppercase ( c ) BIND(C)
         import C_CHAR
         character(kind=C_CHAR), intent(in) :: c
      end function
   end interface

   interface
      character(kind=C_CHAR) function tolowercase ( c ) BIND(C)
         import C_CHAR
         character(kind=C_CHAR), intent(in) :: c
      end function
   end interface

end module

program bindc007
   use m

   interface
      function getpptr(i)
         import touppercase
         procedure(touppercase), pointer :: getpptr
         integer, intent(in) :: i
      end function
   end interface

   character(kind=C_CHAR, len=1) :: c1

   procedure(tolowercase), pointer, bind(c) :: mypp

   mypp => getpptr(1) ! set touppercase

   c1 = mypp("a")
   if ( c1 /= "A") error stop 1_4

   mypp => getpptr(0) ! set tolowercase

   c1 = mypp(c1)
   if ( c1 /= "a") error stop 2_4

end

function getpptr(i)
   use m, only: touppercase, tolowercase

   procedure(touppercase), pointer, bind(c) :: getpptr  !<- different interface name, but same interface characteristics
   integer, intent(in) :: i

   if ( i == 1 ) then
      getpptr => touppercase
   else
      getpptr => tolowercase
   end if

end function
