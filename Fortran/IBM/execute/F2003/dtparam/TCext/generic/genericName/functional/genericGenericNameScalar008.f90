! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/F2003/generic/genericName/functional/genericGenericNameScalar008.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

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

   type base(n1,k1)    ! (20,4)
      integer, kind        :: k1
      integer, len         :: n1
      integer(k1), pointer :: i => null()
      contains
         procedure, pass(a) :: bassgn
         generic :: assgn => bassgn
   end type

   type, extends(base) :: child    ! (20,4)
      integer(k1), allocatable :: j
      contains
         procedure, pass(a) :: bassgn => cassgn
         procedure, pass :: twoargassgn
         generic :: assgn => twoargassgn
   end type

   contains

      subroutine bassgn ( a, k )
         class(base(*,4)), intent(out) :: a
         integer, intent(in) :: k

         if ( .not. associated( a%i ) ) allocate ( a%i )
         a%i = k

         print *, 'bassgn'

      end subroutine

      subroutine cassgn ( a, k )
         class(child(*,4)), intent(out) :: a
         integer, intent(in) :: k

         call a%base%assgn(k)

         if ( .not. allocated( a%j ) ) allocate ( a%j )

         a%j = k
         print *, 'cassgn'

      end subroutine

      subroutine twoargassgn ( a, k, l )
         class(child(*,4)), intent(out) :: a
         integer, intent(in) :: k, l

         call a%base%assgn(k)

         if ( .not. allocated( a%j ) ) allocate ( a%j )

         a%j = l
         print *, 'twoargassgn'

      end subroutine

end module

program genericGenericNameScalar008
   use m

   type(base(20,4)) :: b1
   class(base(:,4)), allocatable :: b2
   class(base(:,4)), pointer :: b3
   class(child(:,4)), pointer :: c1

   allocate ( base(20,4):: b2, b3 )
   allocate ( child(20,4):: c1 )

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

   allocate ( child(20,4) :: b2, b3 )

   call b2%assgn(700)
   call b3%assgn(800)

   select type ( b2 )
      type is ( child(*,4) )
         print *, b2%i, b2%j
         call b2%assgn(900,1000)
         print *, b2%i, b2%j
   end select

   select type ( g => b3 )
      class is ( child(*,4) )
         print *, g%i, g%j
         call g%assgn(1100,1200)
         print *, g%i, g%j
   end select

end program
