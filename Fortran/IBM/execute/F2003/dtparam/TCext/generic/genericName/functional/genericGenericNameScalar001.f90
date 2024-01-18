! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/generic/genericName/functional/genericGenericNameScalar001.f
! opt variations: -qnol -qnodeferredlp

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
!*                                             generic, specific bindings and the subroutine itself
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

   contains

      subroutine bassgn ( a, j )
         class(base(*,4)), intent(out) :: a
         integer, intent(in) :: j

         if ( .not. associated( a%i ) ) allocate ( a%i )
         a%i = j

         print *, 'bassgn'

      end subroutine

end module


program genericGenericNameScalar001
   use m

   type(base(20,4)) :: b1
   type(base(:,4)), allocatable :: b2
   type(base(:,4)), pointer :: b3

   allocate ( base(20,4):: b2, b3 )

   ! call generic type bound

   call b1%assgn(10)
   call b2%assgn(20)
   call b3%assgn(30)

   print *, b1%i, b2%i, b3%i

   ! call specific type bound

   call b1%bassgn(100)
   call b2%bassgn(200)
   call b3%bassgn(300)

   print *, b1%i, b2%i, b3%i

   ! call subroutine directly

   call bassgn(b1,1000)
   call bassgn(b2,2000)
   call bassgn(b3,3000)

   print *, b1%i, b2%i, b3%i

end program
