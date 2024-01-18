!*  ===================================================================
!*
!*  DATE                       : 05/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Characters with deferred length type parameter
!*
!*  DESCRIPTION                : array character with deferred length with
!*                               allocate statement with specifying different lengths in array elements
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

program deferLenAllocate005d
   use n

   character(kind=1,len=:), allocatable :: c2(:,:)
   character(150) :: abc

   character(:), pointer :: c3(:)
   character(0) cc1
   character(1) cc2

   abc = 'a'

   allocate ( c1(3), source = (/ 'a', 'bc', 'def' /) )
   allocate ( c2(2,2), errmsg=abc, source = reshape ( source=(/ abc, abc // 'b', abc // 'b' // 'c', 'z' //abc//'b'//'c' /), shape = (/2,2/)) )

   allocate ( c3, source = (/ cc1, cc2 /) )

end program
