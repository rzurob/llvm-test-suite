! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/F2003/generic/genericName/functional/genericGenericNameScalar014.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with generic-name
!*
!*  DESCRIPTION                : generic-name: scalar derived type calling
!*                                             A private type-bound procedure is accessible
!*                                             only within the module containing the type definition.
!*
!*                                             private spec binding overridden by child type which is defined in another module
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
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)   :: i
      contains
         procedure, private, nopass :: noarg
         procedure, private, nopass :: onearg
         generic :: abc => noarg, onearg
   end type

   interface
      subroutine onearg(b)
         import base
         class(base(*,4)), intent(in) :: b
      end subroutine
   end interface

   contains

      subroutine noarg()
         print *,'noarg'
      end subroutine

end module

module n
   use m

   type,extends(base) :: child    ! (20,4)
      integer(k1) :: j
   end type

end module

program genericGenericNameScalar014
   use n

   class(base(:,4)), allocatable :: b1
   type(child(20,4)) :: c1

   call b1%abc()
   allocate ( b1, source = base(20,4)(100) )
   call b1%abc()
   call b1%abc(b1)

   call c1%abc()

   c1 = child(20,4)(200, 300)

   call b1%abc(c1)
   call c1%abc(b1)
   call c1%abc(c1)

end program

subroutine onearg(b)
   use m, only: base
   use n, only: child

   class(base(*,4)), intent(in) :: b

   select type ( b )
      type is ( base(*,4) )
         print *, 'onearg:', b%i
      type is ( child(*,4) )
         print *, 'onearg:', b%i, b%j
   end select

end subroutine
