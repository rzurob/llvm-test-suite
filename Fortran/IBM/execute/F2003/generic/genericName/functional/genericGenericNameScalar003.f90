!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with generic-name
!*
!*  DESCRIPTION                : generic-name: scalar derived type calling
!*                                             generic, specific bindings
!*                                             with pass and nopass specbnd pointing to same procedure (subroutine)
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
      integer :: i
      contains
         procedure, pass(a) :: assgnpass => assgn
         procedure, nopass  :: assgnnopass => assgn
         generic :: assgn => assgnpass, assgnnopass
   end type

   contains

      subroutine assgn ( a, j )
         class(base), intent(inout) :: a
         integer, intent(in) :: j

         a%i = j

         print *, 'assgn'

      end subroutine

end module

program genericGenericNameScalar003
   use m

   type(base) :: b1
   type(base), allocatable :: b2
   type(base), pointer :: b3

   allocate ( b2, b3 )

   ! calling generic name with pass attribute

   call b1%assgn(100)
   call b2%assgn(200)
   call b3%assgn(300)

   print *, b1%i, b2%i, b3%i

   ! calling generic name with nopass attribute

   call b1%assgn(b2,1000)
   call b2%assgn(b3,2000)
   call b3%assgn(b1,3000)

   print *, b1%i, b2%i, b3%i

   ! calling specific binding

   call b1%assgnpass(10000)
   call b2%assgnnopass(b3, 20000)

   print *, b1%i, b2%i, b3%i

end program
