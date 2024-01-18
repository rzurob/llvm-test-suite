!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 05/01/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Characters with deferred length type parameter
!*
!*  DRIVER STANZA              : xlf2003
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

program deferLenAllocate012
   use n

   character(len=:), allocatable :: c2
   character(5), target :: c3

   integer :: stat

   open ( 1, form='formatted', access='sequential', file='deferLenAllocate012.1' )

   do i = 65,90
      write ( 1, "(a1)" , advance="no" ) char(i)
   end do
   write ( 1, "(\)" )

   rewind 1

   allocate ( c1, source = "                          " ) ! 26 spaces

   read ( 1, * ) c1
   print *, c1

   rewind 1

   allocate ( c2, source = 'abcdefghijklmnopqrstuvwxyz' )

   c3 = 'HAPPY'

   write ( 1, * ) c2, c3

   backspace 1

   deallocate ( c1 )

   allocate ( c1, source = c3//c2)

   read ( 1, * ) c1

   print *, c1

   close ( 1, status='delete' )

end program
