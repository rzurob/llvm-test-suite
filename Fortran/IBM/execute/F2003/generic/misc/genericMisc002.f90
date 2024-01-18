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
!*  SECONDARY FUNCTIONS TESTED : Misc.
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Structures with Array component used in dummy argument and func return
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

module m

   type inner
      integer :: i
   end type

   type base
      type(inner) :: in(1)
   end type

   contains

   type(base) function add ( a, b )
      class(base), intent(in) :: a, b

      add%in = iadd ( a%in , b%in )
      print *, iadd ( a%in , b%in ), add%in, 'end'

   end function

   type(inner) function iadd ( a, b )
      class(inner), intent(in) :: a(1),b(1)
      dimension :: iadd(1)

      iadd%i = a%i + b%i

   end function

end module

program genericMisc002
   use m

   type(base) :: b1, b2

   b1 = base ((/inner(10)/))
   b2 = base ((/inner(11)/))

   b1 = add ( b1 , b2 )

end program
