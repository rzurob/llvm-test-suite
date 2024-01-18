! GB DTP extension using:
! ftcx_dtp -ql -qnodeferredlp /tstdev/F2003/generic/assignment/functional/genericAssignmentPass016.f
! opt variations: -qnol -qdeferredlp

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with assignment
!*
!*  DESCRIPTION                : assignment: pass-obj specified for second arg, however we still
!*                                           try to use the first arg for specbnd resolution
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
      integer(k1)   :: i = -999
      contains
         procedure, pass(b) :: base_base
         procedure, pass(b) :: child_base
         generic :: assignment(=) => base_base, child_base
   end type

   type, extends(base) :: child    ! (20,4)
      contains
         procedure, pass(b) :: child_base => child_child
   end type

   contains

   subroutine base_base ( a, b )
      type(base(*,4)), intent(out) :: a
      class(base(*,4)), intent(in)  :: b

      a%i = b%i

      print *,'base_base'

   end subroutine

   subroutine child_base ( a, b )
      type(child(*,4)), intent(out) :: a
      class(base(*,4)), intent(in)  :: b

      a%i = b%i

      print *,'child_base'

   end subroutine

   subroutine child_child ( a, b )
      type(child(*,4)), intent(out) :: a
      class(child(*,4)), intent(in)  :: b

      a%i = b%i

      print *,'child_child'

   end subroutine

end module

program genericAssignmentPass016
   use m

   class(base(20,4)), allocatable :: b1
   class(child(20,4)), pointer :: c1

   class(*), pointer :: u1

   allocate ( b1, c1 )

   b1 = base(20,4)(100)
   c1 = base(20,4)(200)

   print *,b1%i, c1%i

   b1 = c1
   c1 = child(20,4)(300)

   print *, b1%i, c1%i

   allocate ( base(20,4) :: u1 )

   select type ( u1 )
      class is ( base(*,4) )
         u1 = base(20,4)(100)
         print *, u1%i
         u1 = child(20,4)(200)
         print *, u1%i
   end select

   deallocate ( b1, u1 )
   allocate ( child(20,4) :: b1, u1 )

   select type ( b1 )
      type is ( child(*,4) )
         b1 = c1
         print *, b1%i
   end select

   select type ( u1 )
      class is ( child(*,4) )
         u1 = c1
         print *, u1%i
         u1 = base(20,4)(50)
         print *, u1%i
   end select

end program
