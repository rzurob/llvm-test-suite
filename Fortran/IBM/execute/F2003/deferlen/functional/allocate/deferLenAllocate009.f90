!*  ===================================================================
!*
!*  DATE                       : 05/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Characters with deferred length type parameter
!*
!*  DESCRIPTION                : scalar character with deferred length with substring assignments
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
   character(:), pointer :: c1
end module

program deferLenAllocate009
   use n

   character(len=:), allocatable :: c2

   allocate ( c1, source = 'abcdefghijkl' )
   c1(1:3) = 'xxx'

   print *, c1
   c1(4:6) = ''

   ! should be padded with blanks
   if ( c1 /= 'xxx   ghijkl' ) error stop 1_4

   print *, c1
   c1(7:9) = 'yyy'
   print *, c1
   c1(10:12) = 'zzz'
   print *, c1

   allocate ( c2, source = c1(2:12) )
   print *, c2

end program
