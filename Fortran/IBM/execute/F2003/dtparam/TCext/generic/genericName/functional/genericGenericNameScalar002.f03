! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/generic/genericName/functional/genericGenericNameScalar002.f
! opt variations: -ql

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with generic-name
!*
!*  DESCRIPTION                : generic-name: scalar derived type calling
!*                                             generic, specific bindings and the subroutine itself
!*                                             with different types of dummy arguments
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
         procedure, pass(a) :: bassgnint
         procedure, pass    :: bassgnreal
         generic :: assgn => bassgnint, bassgnreal
   end type

   contains

      subroutine bassgnint ( a, j )
         class(base(4)), intent(inout) :: a
         integer, intent(in) :: j

         if ( .not. associated( a%i ) ) allocate ( a%i )
         a%i = j

         print *, 'bassgnint'

      end subroutine

      subroutine bassgnreal ( a, j )
         class(base(4)), intent(inout) :: a
         real, intent(in) :: j

         if ( .not. associated( a%i ) ) allocate ( a%i )
         a%i = int(j,kind(0))

         print *, 'bassgnreal'

      end subroutine

end module


program genericGenericNameScalar002
   use m

   type(base(4)) :: b1
   type(base(4)), allocatable :: b2
   type(base(4)), pointer :: b3

   allocate ( b2, b3 )

   ! call generic type bound

   call b1%assgn(10)
   call b2%assgn(20)
   call b3%assgn(30)

   print *, b1%i, b2%i, b3%i

   ! call specific type bound

   call b1%bassgnint(100)
   call b2%bassgnint(200)
   call b3%bassgnint(300)

   print *, b1%i, b2%i, b3%i

   ! call subroutine directly

   call bassgnint(b1,1000)
   call bassgnint(b2,2000)
   call bassgnint(b3,3000)

   print *, b1%i, b2%i, b3%i

   ! call generic type bound

   call b1%assgn(12.0)
   call b2%assgn(22.0)
   call b3%assgn(32.0)

   print *, b1%i, b2%i, b3%i

  ! call specific type bound

   call b1%bassgnreal(102.0)
   call b2%bassgnreal(202.0)
   call b3%bassgnreal(302.0)

   print *, b1%i, b2%i, b3%i

   ! call subroutine directly

   call bassgnreal(b1,1002.0)
   call bassgnreal(b2,2002.0)
   call bassgnreal(b3,3002.0)

   print *, b1%i, b2%i, b3%i

end program
