!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 09/28/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Testing: If the rightmost part-name is of abstract type, data-ref shall be polymorphic. (C611)
!*                                        R614: structure-component is data-ref
!*                                        non-polymorphic abstract type data-ref assigned data, user defined assignment
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

   type, abstract :: base
      integer :: id
   end type

   type, extends(base) :: child
      real :: rid
   end type

end module

program userDefAssgn001
   use m

   interface assignment (=)
      subroutine aaa ( a, b )
         import base
         class(base), intent(out) :: a
         class(base), intent(in) :: b
      end subroutine
   end interface


   class(base), allocatable :: a, b
   type(child) :: c

   allocate ( a, source = child(1,2.3))
   allocate ( b, source = child(2,3.4))

   a = b             !<- legal
   select type ( b )
      class is ( child )
         a = b%base  !<- illegal
         c%base = b  !<- illegal
   end select

end program

subroutine aaa ( a, b )
   use m
   class(base), intent(out) :: a
   class(base), intent(in) :: b

   a%id = b%id
end subroutine
