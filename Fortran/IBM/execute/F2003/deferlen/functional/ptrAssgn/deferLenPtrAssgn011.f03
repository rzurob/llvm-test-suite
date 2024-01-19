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

program deferLenPtrAssgn011
   use n

   character(len=:), pointer :: c2
   character(5), target :: c3

   integer :: stat

   allocate ( c1, source = '                                                                                                                                                      ' ) ! 150 spaces
   c2 => c1

   open ( 1, form='formatted', access='sequential', file='deferLenPtrAssgn011.1' )
   read ( 1, *, iostat=stat, iomsg = c2 ) c1 ! end of file read

   print *, len(c1), ( c1 == c2 )
   print *, len(c2), associated(c1, c2)

   close ( 1, status='delete' )

end program
