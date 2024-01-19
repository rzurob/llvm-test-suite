!*  ===================================================================
!*
!*  DATE                       : 05/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Characters with deferred length type parameter
!*
!*  DESCRIPTION                : scalar character with deferred length
!*                               pointer assignment statement and dynamically change character length
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

program deferLenPtrAssgn009
   use n

   character(len=:), pointer :: c2(:)

   character(3), target :: c3(3) = (/ 'abc', 'def', 'ghi' /)
   character(4), target :: c4(4) = (/ 'AAAA', 'BBBB', 'CCCC', 'DDDD' /)

   integer(1) :: i

   allocate ( c1(10), source = (/ 'aa', 'bb', 'cc', 'dd', 'ee', 'ff', 'gg', 'hh', 'ii', 'jj' /) )

   i = len( c1 )
   if ( i /= 2_1 ) error stop 1_4

   print *, c1, size(c1)

   c2 => c1(2:10)
   print *, c2, len(c2), size(c2), associated(c2, c1)

   c2 => c3(3:1:-1)
   print *, c2, len(c2), size(c2), associated(c2, c3)

   c1 => c3(3:1:-1)
   print *, c1, len(c1), size(c1), associated(c1, c2)

   c1 = (/ 'zzz', 'yyy', 'xxx'  /)
   print *, c1
   print *, c2
   print *, c3

   c1 => c4
   print *, c1, len(c1), size(c1), associated(c1, c2)

   c2 => c4(1:4)
   print *, c2, len(c2), size(c2), associated(c1, c2)

end program
