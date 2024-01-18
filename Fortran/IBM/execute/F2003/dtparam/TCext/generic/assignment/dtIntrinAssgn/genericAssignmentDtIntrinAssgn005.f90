! GB DTP extension using:
! ftcx_dtp -qck -ql -qnodefaultpv -qnodeferredlp -qreuse=base /tstdev/F2003/generic/assignment/dtIntrinAssgn/genericAssignmentDtIntrinAssgn005.f
! opt variations: -qnock -qnol -qdefaultpv -qdeferredlp -qreuse=self -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : Derived Type Intrinsic Assignment:
!*                                  - Perform generic type bound assignment for the type
!*                                    component when it is declared for the type
!*                                    when there is type hierarchy on container type
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

   type com1(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)   :: i
      contains
         generic :: assignment(=) => c1assgn
         procedure, pass :: c1assgn
   end type

   type com2(k2,n2)    ! (1,3)
      integer, kind             :: k2
      integer, len              :: n2
      character(kind=k2,len=n2) :: c
      contains
         generic :: assignment(=) => c2assgn
         procedure, pass :: c2assgn
   end type

   contains

      subroutine c1assgn ( a, b )
         class(com1(*,4)), intent(out) :: a
         class(com1(*,4)), intent(in) :: b

         a%i = b%i
         print *, 'c1assgn'

      end subroutine

      subroutine c2assgn ( a, b )
         class(com2(1,*)), intent(out) :: a
         class(com2(1,*)), intent(in) :: b

        a%c = b%c
         print *, 'c2assgn'

      end subroutine

end module

module m1
   use m

   type base(n3,k3,k4,n4)    ! (20,4,1,3)
      integer, kind     :: k3,k4
      integer, len      :: n3,n4
      integer(k3)       :: x
      type(com1(n3,k3)) :: c11
      type(com2(k4,n4)) :: c21
   end type

   type, extends(base) :: child    ! (20,4,1,3)
      type(com1(n3,k3)) :: c12
      type(com2(k4,n4)) :: c22
   end type

end module

program genericAssignmentDtIntrinAssgn005
   use m1

   type(base(20,4,1,3)) :: b1
   type(base(20,4,1,3)), allocatable :: b2
   type(child(20,4,1,3)) :: c1
   type(child(20,4,1,3)), pointer :: c2

   allocate ( b2, c2 )

   b1 = base(20,4,1,3)(100, com1(20,4)(100), com2(1,3)('aaa'))
   print *, b1
   b2 = base(20,4,1,3)(200, com1(20,4)(200), com2(1,3)('bbb'))
   print *, b2

   c1 = child(20,4,1,3)(1000, com1(20,4)(1000), com2(1,3)('ccc'), com1(20,4)(2000), com2(1,3)('ddd') )
   print *, c1

   c2 = child(20,4,1,3)(3000, com1(20,4)(3000), com2(1,3)('eee'), com1(20,4)(4000), com2(1,3)('fff') )
   print *, c2

   b1 = c1%base
   print *, b1

   b2 = c2%base
   print *, b2

   c1 = c2
   print *, c1

end program
