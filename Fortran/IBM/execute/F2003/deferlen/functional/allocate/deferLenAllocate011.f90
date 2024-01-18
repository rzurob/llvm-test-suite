!*  ===================================================================
!*
!*  DATE                       : 05/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Characters with deferred length type parameter
!*
!*  DESCRIPTION                : scalar character with deferred length
!*                               with formatted I/O (list directed) with deferred character in iomsg
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

program deferLenAllocate011
   use n

   character(len=:), allocatable :: c2
   character(5), target :: c3

   integer :: stat

   allocate ( c1, source = '                                                  ' ) ! 50 spaces
   allocate ( c2, source = '' ) ! 0 length

   c2 = 'abcdefgh'

   open ( 1, form='formatted', access='sequential', file='deferLenAllocate011.1' )
   read ( 1, *, iostat=stat, iomsg = c2 ) c1 ! end of file read

   if (c2 /= '1525-001') error stop 3_4
   if (len(c2) /= 8) error stop 4_4

   if (len(c1) /= 50) error stop 1_4
   if (c1 /= '') error stop 2_4

   close ( 1, status='delete' )

end program
