!*  ===================================================================
!*
!*  DATE                       : 05/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Characters with deferred length type parameter
!*
!*  DESCRIPTION                : scalar character with deferred length
!*                               with formatted I/O (list directed)
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

program deferLenPtrAssgn012
   use n

   character(len=:), pointer :: c2
   character(26), target :: c3

   integer :: stat

   open ( 1, form='formatted', access='sequential', file='deferLenAllocate012.1' )

   do i = 65,90
      write ( 1, "(a1)" , advance="no" ) char(i)
   end do
   write ( 1, "(\)" )

   rewind 1

   allocate ( c1, source = "                          " ) ! 26 spaces
   c2 => c1

   read ( 1, * ) c2
   print *, c1
   print *, c2

   rewind 1

   c3 = 'abcdefghijklmnopqrstuvwxyz'

   c2 => c3
   c1 => c2

   write ( 1, * ) c2, c3

   backspace 1

   allocate ( c1, source = c3//c2)

   read ( 1, * ) c1

   print *, c1

   close ( 1, status='delete' )

end program