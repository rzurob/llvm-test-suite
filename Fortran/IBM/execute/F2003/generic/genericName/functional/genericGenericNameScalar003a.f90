!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 11/01/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*                             :
!*  SECONDARY FUNCTIONS TESTED : with generic-name
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : generic-name: scalar derived type calling
!*                                             generic, specific bindings
!*                                             with pass and nopass specbnd pointing to same procedure (function)
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
         procedure, pass(a) :: addpass => add
         procedure, nopass  :: addnopass => add
         generic :: add => addpass, addnopass
   end type

   contains

      integer function add ( a, j )
         class(base), intent(in) :: a
         integer, intent(in) :: j

         add = a%i + j

         print *, 'add'

      end function

end module

program genericGenericNameScalar003a
   use m

   type(base) :: b1
   type(base), allocatable :: b2
   type(base), pointer :: b3

   integer :: i, j(4),k(2,2)

   allocate ( b2, b3 )

   b1 = base(100)
   b2 = base(200)
   b3 = base(300)

   ! calling generic name with pass attribute

   i = b1%add(100)
   j = b2%add(200)
   k = b3%add(300)

   print *, i
   print *, j
   print *, k

   ! calling generic name with nopass attribute

   i = b1%add(base(10), 100)
   j = b2%add(b1, 200)
   k = b3%add(base(30), 300)

   print *, i
   print *, j
   print *, k

end program
