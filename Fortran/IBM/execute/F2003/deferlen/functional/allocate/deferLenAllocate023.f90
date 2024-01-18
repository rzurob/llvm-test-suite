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
!*  DESCRIPTION                : scalar character with deferred length scalar/array
!*                               with LLE, LLT, LGE, LGT
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

   character(:), pointer :: c1(:)
   character(:), allocatable :: c2

   type base
      character(:), pointer :: c(:)
   end type

   type(base) :: b1

end module

program deferLenAllocate023
   use m

   allocate ( c1(3), source = (/ "cat1", "cat2", "cat3" /) )

   allocate ( c2, source = "tiger" )

   allocate ( b1%c(3), source = (/ "dog1", 'dog2', 'dog3' /) )

   print *, lge(c1, b1%c), lgt(c1, b1%c), lge(b1%c, c1), lgt(b1%c, c1)
   print *, lle(c1, b1%c), llt(c1, b1%c), lle(b1%c, c1), llt(b1%c, c1)

   print *, lge(c2, b1%c), lgt(c2, b1%c), lge(b1%c, c2), lgt(b1%c, c2)
   print *, lle(c2, b1%c), llt(c2, b1%c), lle(b1%c, c2), llt(b1%c, c2)

   deallocate ( c1, b1%c )

   allocate ( c1(3), source = (/ "cat", "dog", "cat" /) )
   allocate ( b1%c(3), source = (/ "dog", "cat", "dog" /) )

   print *, lge(c1, b1%c), lgt(c1, b1%c), lge(b1%c, c1), lgt(b1%c, c1)
   print *, lle(c1, b1%c), llt(c1, b1%c), lle(b1%c, c1), llt(b1%c, c1)

end program
