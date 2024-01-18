!*  ===================================================================
!*
!*  DATE                       : 05/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Characters with deferred length type parameter
!*
!*  DESCRIPTION                : scalar character with deferred length
!*                               with formatted I/O (namelist)
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

program deferLenAllocate013
   use n

   character(len=:), allocatable :: c2
   character(5), target :: c3

   integer :: stat
   character(150) :: msg = ''

   namelist /abc/ c1, c2, c3

   open ( 1, form='formatted', access='sequential', file='deferLenAllocate013.1' )

   allocate ( c1, source = 'abcdefghijkl' )
   allocate ( c2, source = '  ABCDEFGHIJ' )
   c3 = 'happy'

   write ( 1, abc, iostat = stat, iomsg = msg )

   if ( ( stat /= 0 ) .or. ( msg /= '' ) ) error stop 1_4

end program
