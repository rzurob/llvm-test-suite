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
!*  DESCRIPTION                : scalar character with deferred length as structure component with
!*                               implicit pointer assignment
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

module n
   type base
      character(:), pointer :: c
   end type

end module

program deferLenAllocate007
   use n

   type(base) :: b1
   type(base) :: b2

   character, target :: a="A"
   character, target :: b="B"
   character, target :: c="C"
   character(2), target :: aa="AA"
   character(3), target :: bbb="BBB"
   character*4, target :: cccc="CCCC"

   b1 = base(a)
   print *, b1%c, len(b1%c), associated(b1%c, a)

   b2 = base(b)
   print *, b2%c, len(b2%c), associated(b2%c, b)

   b1 = base(c)
   print *, b1%c, len(b1%c), associated(b1%c, c)

   b2 = base(aa)
   print *, b2%c, len(b2%c), associated(b2%c, aa)

   b1 = b2
   print *, b1%c, len(b1%c), associated(b1%c, aa), associated ( b1%c, b2%c )

   b2 = base(bbb)
   print *, b2%c, len(b2%c), associated(b2%c, bbb)

   b1 = base(cccc)
   print *, b1%c, len(b1%c), associated(b1%c, cccc)

   associate ( ggg => cccc )
      b2 = base(ggg)
      print *, b2%c, len(b2%c), associated(b2%c, ggg), associated(b2%c, cccc)
   end associate

end program
