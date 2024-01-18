!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 05/01/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Characters with deferred length type parameter
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : scalar character with deferred length
!*                               with index
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

   character(36), target :: c1
   
   character(:), pointer :: c2

   type base
      character(:), pointer :: c(:)
   end type

   type(base) :: b1

end module

program deferLenPtrAssgn020
   use m

   c1 = "xxxxxxxxxxxxxxxxxxxxxxxibmxxxxxxibmx"
   print *, len(c1), index(c1, 'ibm'), index(substring="ibm", string=c1, back=.true. )
   
   c2 => c1
   print *, len(c2), index(c2, 'ibm'), index(substring="ibm", string=c2, back=.true. )

   allocate ( b1%c(3), source = (/ "a" // c1 // "b", "c" // c1 // "d", "e" // c1 // "f"/) )

   print *, len(b1%c), index(b1%c, 'ibm'), index(b1%c, c1), index(c1,b1%c)

   c2 => b1%c(1)

   print *, len(c2), index(c2, 'ibm'), index(substring=c2, string=b1%c), index(substring=b1%c, string=c2)

end program