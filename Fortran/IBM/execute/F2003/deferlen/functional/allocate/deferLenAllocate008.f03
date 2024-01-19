!*  ===================================================================
!*
!*  DATE                       : 05/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Characters with deferred length type parameter
!*
!*  DESCRIPTION                : array character with deferred length with
!*                               allocate statement
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

program deferLenAllocate008
   use n

   character(len=:), allocatable :: c2(:,:)
   integer(1) :: i

   allocate ( c1(10), source = (/ 'aa', 'bb', 'cc', 'dd', 'ee', 'ff', 'gg', 'hh', 'ii', 'jj' /) )

   i = len( c1, kind=1 )
   if ( i /= 2_1 ) error stop 1_4

   print *, c1

   allocate ( c2(3,3), source = reshape ( source = c1(2:10), shape = (/3,3/) ) )
   print *, c2, len(c2)

   deallocate ( c2 )

   allocate ( c2(2,2), source = reshape ( source = c1(1:7:2), shape = (/2,2/) ) )
   print *, c2, len(c2)

   deallocate ( c2 )

   allocate ( c2(2,2), source = reshape ( source = c1((/10,1,9,2/)), shape = (/2,2/) ) )
   print *, c2, len(c2)

end program
