!*  ===================================================================
!*
!*  DATE                       : 05/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Characters with deferred length type parameter
!*
!*  DESCRIPTION                : array of character with deferred length
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

   type base
      character(:), allocatable :: c(:)
   end type

   interface !assignment(=)
      subroutine mycharassignment(a,b)
         import base
         type(base), intent(out)  :: a
         character(:), allocatable, intent(in) :: b(:)
      end subroutine
   end interface

end module

subroutine mycharassignment(a,b)
   use n, only: base
   type(base), intent(out)  :: a
   character(:), allocatable, intent(in) :: b(:)

   if( allocated( a%c ) ) deallocate ( a%c )
   print *, "inside assignment: ", b, len(b), size(b)
   allocate ( a%c(size(b)), source = b )

end subroutine

program deferLenArgAsso014
   use n

   type(base), pointer :: b1
   character(:), allocatable :: c1(:)

   allocate ( b1, source = base(null()) )
   allocate ( c1(5), source = (/ 'abc', 'def', 'ghi', 'jkl', 'mno' /) )

   if (allocated(b1%c)) error stop 10_4

!   b1 = c1
   call mycharassignment(b1, c1)

   print *, b1%c, len(b1%c), size(b1%c)

end program
