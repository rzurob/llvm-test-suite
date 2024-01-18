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
!*                                        Sequence Association, try C_CHAR characters
!*                                        Dummy argument being assume-size array
!*
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
      subroutine printchar ( c1 )  bind(c)
         import C_CHAR
         character(kind=C_CHAR) :: c1(*)
      end subroutine
   end interface

end module


module m1

   use ISO_C_BINDING, only: C_CHAR

   interface
      subroutine print ( c )  bind(c)
         import C_CHAR
         character(kind=C_CHAR) :: c(*)
      end subroutine
   end interface

   procedure(print), pointer :: pp1

end module

program bindc023
   use m
   use m1, only: pp1

   character(kind=C_CHAR, len=10) :: c1(10)

   pp1 => printchar

   c1 = (/ "Abcdefghij", "aBcdefghij", "abCdefghij", "abcDefghij", "abcdEfghij", &
           "abcdeFghij", "abcdefGhij", "abcdefgHij", "abcdefghIj", "abcdefghiJ" /)

   call pp1 ( c1 )           !<- print out first 2 element

   call pp1 ( c1(10:1:-2) )  !<- print out c1(10) c1(8) element

   call pp1 ( c1 ((/3,7/)) ) !<- print out c1(3) c1(7) element

   call pp1 ( c1(8)(6:10) )  !<- print out "fgHijabcdefghIjabcde" element

end program
