!*  ===================================================================
!*
!*  DATE                       : 05/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Characters with deferred length type parameter
!*
!*  DESCRIPTION                : scalar character with deferred length
!*                               with len and len_trim
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
   character(:), pointer :: c1
end module

program deferLenPtrAssgn010
   use n

   character(len=:), pointer :: c2

   allocate ( c1, source = "mary has a little lamb, little lamb, little lamb, It's fleece was white as snow!&
                            &                                                                                " )
   print *, c1, len(c1), len_trim(c1)

   c2 => c1(1:80)
   print *, c2, len(c2), len_trim(c2)

   print *, len(c1//c2), len_trim(c1 // c2),len(c2//c1), len_trim(c2 // c1)

end program
