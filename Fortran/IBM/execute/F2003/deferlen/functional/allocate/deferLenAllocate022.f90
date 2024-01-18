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
!*                               with LLE, LLT
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

   character(:), pointer :: c1

   type base
      character(:), pointer :: c
   end type

   type(base) :: b1

end module

program deferLenAllocate022
   use m

   ! same

   allocate ( c1, source = "abcdefghijklmnopqrstuvwxyz" )
   allocate ( b1%c, source = "abcdefghijklmnopqrstuvwxyz" )
   print *, lle(c1, b1%c), llt(c1, b1%c), lle(b1%c, c1), llt(b1%c, c1)

   ! same but different case
   
   deallocate ( c1, b1%c )

   allocate ( c1, source = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" )
   allocate ( b1%c, source = "abcdefghijklmnopqrstuvwxyz" )
   print *, lle(c1, b1%c), llt(c1, b1%c), lle(b1%c, c1), llt(b1%c, c1)
   
   ! different
   
   deallocate ( c1, b1%c )

   allocate ( c1, source = "dogs" )
   allocate ( b1%c, source = "cats" )
   print *, lle(c1, b1%c), llt(c1, b1%c), lle(b1%c, c1), llt(b1%c, c1)   

end program