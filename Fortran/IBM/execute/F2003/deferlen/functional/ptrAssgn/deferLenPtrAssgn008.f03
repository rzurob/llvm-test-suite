!*  ===================================================================
!*
!*  DATE                       : 05/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Characters with deferred length type parameter
!*
!*  DESCRIPTION                : array character with deferred length with
!*                               pointer assignment statement
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
   character(:), pointer :: c1(:)
end module

program deferLenPtrAssgn008
   use n

   character(len=:), pointer :: c2(:)
   integer(1) :: i

   allocate ( c1(10), source = (/ 'aa', 'bb', 'cc', 'dd', 'ee', 'ff', 'gg', 'hh', 'ii', 'jj' /) )

   i = len( c1 )
   if ( i /= 2_1 ) error stop 1_4

   print *, c1, size(c1)

   c2 => c1(2:10)
   print *, c2, len(c2), size(c2), associated(c2, c1)

   nullify ( c2 )

   allocate ( c2(4), source = reshape ( source = c1(1:7:2), shape = (/4/) ) )
   print *, c2, len(c2), size(c2), associated(c2, c1)

   c1 => c2

   print *, c1, len(c1), size(c1), associated(c1,c2)

end program
