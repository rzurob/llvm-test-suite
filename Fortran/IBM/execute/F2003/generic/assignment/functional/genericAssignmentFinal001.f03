!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : assignment: Finalization for generic assignment tb
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   type base
      integer, pointer :: i
      contains
         generic :: assignment(=) => bassgn
         procedure, pass :: bassgn
         final :: bfinal
   end type

   contains

   subroutine bassgn ( a, b )
      class(base), intent(out) :: a
      type(base), intent(in)   :: b

       a%i = b%i

       print *, 'bassign'

   end subroutine

   subroutine bfinal ( a )
      type(base), intent(inout) :: a

      write (*,'(1x,a)',advance='no') 'bfinal'

      if ( associated ( a%i ) ) then
         write (*,'(":",i6)',advance='no') a%i
       	 nullify ( a%i )
      end if

      allocate ( integer :: a%i )
      write (*,'(\)')

   end subroutine

end module

program genericAssignmentFinal001
   use m

   type(base) :: b1
   type(base), allocatable :: b2
   type(base), pointer :: b3

   integer, target :: i1 = 1227

   allocate ( b2, b3 )

   b1 = base(i1)
   print *, b1%i

   b2 = b1
   print *, b2%i

   b3 = b1
   print *, b3%i

end program
