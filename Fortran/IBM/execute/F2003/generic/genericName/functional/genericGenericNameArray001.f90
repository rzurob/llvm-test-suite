!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with generic-name
!*
!*  DESCRIPTION                : generic-name: generic tb containing array of intrinsic types dummy args of different ranks
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
      integer, allocatable :: i(:)
      contains
         procedure, pass(a) :: base_int1darray
         procedure, pass(a) :: base_int2darray
         generic :: assgn => base_int1darray, base_int2darray
   end type

   contains

   subroutine base_int1darray( a, b )
      class(base), intent(inout) :: a
      integer, intent(in) :: b(:)

      if ( allocated ( a%i ) ) deallocate ( a%i )

      allocate ( a%i(size(b)), source = b )

   end subroutine

   subroutine base_int2darray( a, b )
      class(base), intent(inout) :: a
      integer, intent(in) :: b(:,:)

      if ( allocated ( a%i ) ) deallocate ( a%i )

      allocate ( a%i( size(b)), source = reshape ( source = b, shape = (/ size(b) /) ) )

   end subroutine

end module

program genericGenericNameArray001
   use m

   class(base), allocatable :: b1
   class(base), pointer ::  b2
   integer :: i(2,2)

   allocate ( b1, b2 )

   call b1%assgn( (/0,1,2,3,4,5,6,7,8,9 /) )
   call b2%assgn( (/b1%i, 10, 11 /) )

   print *, b1%i
   print *, b2%i

   i = reshape ( source = (/ 11,12,13,14 /), shape = (/2,2/) )

   call b1%assgn(i)
   call b2%assgn(i)

   print *, b1%i
   print *, b2%i

end program
