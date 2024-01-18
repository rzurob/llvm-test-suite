!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with generic-name
!*
!*  DESCRIPTION                : generic-name: scalar derived type calling
!*                                             generic, specific bindings and the subroutine itself with polymorphic types
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
      integer, pointer :: i => null()
      contains
         procedure, pass(a) :: bassgn
         generic :: assgn => bassgn
   end type

   type, extends(base) :: child
      integer, allocatable :: j
      contains
         procedure, pass(a) :: bassgn => cassgn
         procedure, pass :: twoargassgn
         generic :: assgn => twoargassgn
   end type

   contains

      subroutine bassgn ( a, k )
         class(base), intent(out) :: a
         integer, intent(in) :: k

         if ( .not. associated( a%i ) ) allocate ( a%i )
         a%i = k

         print *, 'bassgn'

      end subroutine

      subroutine cassgn ( a, k )
         class(child), intent(out) :: a
         integer, intent(in) :: k

         call a%base%assgn(k)

         if ( .not. allocated( a%j ) ) allocate ( a%j )

         a%j = k
         print *, 'cassgn'

      end subroutine

      subroutine twoargassgn ( a, k, l )
         class(child), intent(out) :: a
         integer, intent(in) :: k, l

         call a%base%assgn(k)

         if ( .not. allocated( a%j ) ) allocate ( a%j )

         a%j = l
         print *, 'twoargassgn'

      end subroutine

end module

program genericGenericNameScalar008
   use m

   type(base) :: b1
   class(base), allocatable :: b2
   class(base), pointer :: b3
   class(child), pointer :: c1

   allocate ( b2, b3, c1 )

   call b1%assgn(100)
   print *, b1%i
   call b2%assgn(200)
   print *, b2%i
   call b3%assgn(300)
   print *, b3%i

   call c1%assgn(400)
   print *, c1%i, c1%j

   call c1%assgn(500,600)
   print *, c1%i, c1%j

   deallocate ( b2, b3 )

   allocate ( child :: b2, b3 )

   call b2%assgn(700)
   call b3%assgn(800)

   select type ( b2 )
      type is ( child )
         print *, b2%i, b2%j
         call b2%assgn(900,1000)
         print *, b2%i, b2%j
   end select

   select type ( g => b3 )
      class is ( child )
         print *, g%i, g%j
         call g%assgn(1100,1200)
         print *, g%i, g%j
   end select

end program
