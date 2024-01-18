!*  ===================================================================
!*
!*  DATE                       : 05/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Characters with deferred length type parameter
!*
!*  DESCRIPTION                : scalar character with deferred length
!*                               with LGE, LGT
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

program deferLenAllocate021
   use m

   allocate ( c1, source = "abcdefghijklmnopqrstuvwxyz" )
   allocate ( b1%c, source = "abcdefghijklmnopqrstuvwxyz" )

   ! same

   print *, lge(c1, b1%c), lgt(c1, b1%c), lge(b1%c, c1), lgt(b1%c, c1)

   deallocate ( c1, b1%c )
   allocate ( c1, source = "abcdefghijklmnopqrstuvwxyz       " )
   allocate ( b1%c, source = "abcdefghijklmnopqrstuvwxyz                   " )

   ! same, but with uneven trailing spaces

   print *, lge(c1, b1%c), lgt(c1, b1%c), lge(b1%c, c1), lgt(b1%c, c1)

   deallocate ( c1, b1%c )
   allocate ( c1, source = "" )
   allocate ( b1%c, source = "" )

   ! same, but both zero length

   print *, lge(c1, b1%c), lgt(c1, b1%c), lge(b1%c, c1), lgt(b1%c, c1)

   deallocate ( c1, b1%c )
   allocate ( c1, source = "       abcdefghijklmnopqrstuvwxyz" )
   allocate ( b1%c, source = " abcdefghijklmnopqrstuvwxyz" )

   ! same, but with uneven prevailing spaces

   print *, lge(c1, b1%c), lgt(c1, b1%c), lge(b1%c, c1), lgt(b1%c, c1)

end program
