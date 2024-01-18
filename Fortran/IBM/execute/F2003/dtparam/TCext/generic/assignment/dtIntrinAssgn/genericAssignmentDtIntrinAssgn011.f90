! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp /tstdev/F2003/generic/assignment/dtIntrinAssgn/genericAssignmentDtIntrinAssgn011.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=self

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : Derived Type Intrinsic Assignment:
!*                                      - try a derived type containing an derived type scalar and array
!*                                        with elemental UD assignment in generic tb
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

   type base(n1,k1,k2)    ! (20,4,4)
      integer, kind :: k1,k2
      integer, len  :: n1
      integer(k1)   :: i
      integer(k2)   :: j(3)
      contains
         procedure :: bassgn
         generic :: assignment(=) => bassgn
   end type

   type container(k3,n2)    ! (4,20)
      integer, kind        :: k3
      integer, len         :: n2
      type(base(n2,k3,k3)) :: b1       ! scalar
      type(base(n2,k3,k3)) :: b2(2:6)  ! array
   end type

   contains

      elemental subroutine bassgn ( a, b )
         class(base(*,4,4)), intent(out) :: a
         class(base(*,4,4)), intent(in) :: b

         a%i = b%i + 1
         a%j = b%j + 1

      end subroutine

end module

program genericAssignmentDtIntrinAssgn011
   use m

   type(container(4,:)), pointer ::  c1
   type(container(4,:)) ::  c2
   type(container(4,20)) ::  c3

   allocatable :: c2

   allocate ( c1, source = container(4,20) ( base(20,4,4)(1,(/2,3,4/)), (/ ( base(20,4,4)(i,(/i+1,i+2,i+3/)), i = 5,21,4 ) /) ) )
   allocate ( c2, source = container(4,20) ( base(20,4,4)(2,(/3,4,5/)), (/ ( base(20,4,4)(i,(/i+1,i+2,i+3/)), i = 6,22,4 ) /) ) )
   print *, c1
   print *, c2

   c3 = c1
   print *, c3

   c1 = c2
   print *, c1

   c1%b2 = c2%b2(6:2:-1)
   print *, c1

end program
