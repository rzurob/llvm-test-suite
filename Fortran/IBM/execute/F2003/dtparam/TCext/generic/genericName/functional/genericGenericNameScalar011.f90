! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/F2003/generic/genericName/functional/genericGenericNameScalar011.f
! opt variations: -ql -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with generic-name
!*
!*  DESCRIPTION                : generic-name: scalar derived type calling
!*                                             generic and the subroutine itself
!*                                             with private specific bindings
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

   type base(k1)    ! (4)
      integer, kind        :: k1
      integer(k1), pointer :: i => null()
      contains
         procedure, private, pass(b) :: myassgn
         generic :: a => myassgn
   end type

   type, extends(base) :: child    ! (4)
      integer(k1) :: j = -999
      contains
         procedure, private, pass(b) :: mycassgn
         generic :: a => mycassgn
   end type

   contains

      subroutine myassgn ( a, b )
         class(base(4)), intent(out) :: b
         integer, intent(in) :: a

         if ( .not. associated( b%i ) ) allocate ( b%i )
         b%i = a

         select type ( b )
            type is ( child(4) )
               b%j = a
         end select

         print *, 'myassgn'

      end subroutine

      subroutine mycassgn ( a, b, c )
         class(child(4)), intent(out) :: b
         integer, intent(in) :: a, c

         if ( .not. associated( b%i ) ) allocate ( b%i )
         b%i = a
         b%j = c
         print *, 'mycassgn'

      end subroutine

end module


program genericGenericNameScalar011
   use m

   type(base(4)) :: b1
   class(base(4)), pointer :: b2
   class(child(4)), allocatable :: c1

   allocate ( b2, c1 )

   call b1%a(100)
   call b2%a(200)
   call c1%a(300)

   print *, b1%i, b2%i, c1%i

   call c1%a(400, 500)

   print *, c1%i, c1%j

   deallocate ( b2 )
   allocate ( b2, source = child(4) () )

   call b2%a(600)

   select type ( b2 )
      type is ( child(4) )
         print *, b2%i, b2%j
         call b2%a(700,800)
         print *, b2%i, b2%j
   end select

end program
