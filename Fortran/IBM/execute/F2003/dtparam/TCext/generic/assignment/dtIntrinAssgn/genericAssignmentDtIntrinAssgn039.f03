! GB DTP extension using:
! ftcx_dtp -ql -qnodefaultpv -qdeferredlp -qreuse=self /tstdev/F2003/generic/assignment/dtIntrinAssgn/genericAssignmentDtIntrinAssgn039.f
! opt variations: -qnol -qdefaultpv -qnodeferredlp -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : Derived Type Intrinsic Assignment:
!*                                 - Deeper levels of components have elemental generic assignment defined
!*
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

   type level4(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)   :: i
      contains
         generic :: assignment(=) => l4assgn
         procedure :: l4assgn => assgn
   end type

   type level3(n2,k2)    ! (20,4)
      integer, kind       :: k2
      integer, len        :: n2
      integer(k2)         :: i
      type(level4(n2,k2)) :: l4(2)
   end type

   type level2(n3,k3)    ! (20,4)
      integer, kind       :: k3
      integer, len        :: n3
      integer(k3)         :: i
      type(level3(n3,k3)) :: l3(1)
   end type

   type level1(n4,k4)    ! (20,4)
      integer, kind       :: k4
      integer, len        :: n4
      integer(k4)         :: i
      type(level2(n4,k4)) :: l2(2)
   end type

   type level0(n5,k5)    ! (20,4)
      integer, kind       :: k5
      integer, len        :: n5
      integer(k5)         :: i
      type(level1(n5,k5)) :: l1
   end type

   contains

      elemental subroutine assgn ( a, b )
         class(level4(*,4)), intent(out) :: a
         class(level4(*,4)), intent(in)  :: b

         a%i = b%i + 1
      end subroutine

end module

program genericAssignmentDtIntrinAssgn039
   use m

   type(level0(20,4)) :: l01, l02, l03

   type(level1(20,4)) :: l10
   type(level2(20,4)) :: l20
   type(level3(20,4)) :: l30

   allocatable :: l02
   pointer :: l03

   allocate ( l02, l03 )

   l01 = level0(20,4)( 1, level1(20,4)(2, (/ level2(20,4)(3, (/ level3(20,4)( 4, (/ level4(20,4) ( 5 ), level4(20,4)(6) /) ) /) ), level2(20,4)(7, (/ level3(20,4)( 8, (/ level4(20,4) ( 9 ), level4(20,4)(10) /) ) /) ) /) ) )
   print *, l01

   l02 = l01
   print *, l02

   l03 = l02
   print *, l03

   l10 = l01%l1
   print *, l10

   l20 = l01%l1%l2(1)
   print *, l20

   l30 = l02%l1%l2(1)%l3(1)
   print *, l30

end program
