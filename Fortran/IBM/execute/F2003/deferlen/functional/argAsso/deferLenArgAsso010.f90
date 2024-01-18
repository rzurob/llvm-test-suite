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
!*  DESCRIPTION                : character with deferred length
!*                               in user-defined assignment
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

   interface assignment(=)
      subroutine mycharassignment(a,b)
         character(*), intent(out) :: a
         integer, intent(in) :: b(:)
      end subroutine
   end interface

end module

subroutine mycharassignment(a,b)
   character(*), intent(out) :: a
   integer, intent(in) :: b(:)

   if ( len(a) .ne. size(b) ) error stop 1_4

   do i =1,size(b)
      a(i:i) = achar(b(i))
   end do

end subroutine

program deferLenArgAsso010
   use n

   character(:), allocatable :: c1, c2(:)

   allocate ( character(10) :: c1 )

   allocate ( c2(10), source = (/ 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j' /) )

   c1 = iachar(c2)

   print *, c1
   print *, c2

end program
