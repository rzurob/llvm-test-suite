! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=self /tstdev/F2003/generic/assignment/dtIntrinAssgn/genericAssignmentDtIntrinAssgn007.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : Derived Type Intrinsic Assignment:
!*                                  - A derived-type intrinsic assignment is performed as if each component of variable
!*                                    were assigned from the corresponding component of expr using pointer
!*                                    assignment for each pointer component
!*                                      - try derived type nonpointer nonallocatable component containing pointer components
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

   type innercomp1(n1,k1)    ! (20,4)
      integer, kind        :: k1
      integer, len         :: n1
      integer(k1), pointer :: i
      contains
         generic :: assignment(=) => c1assgn
         procedure, pass :: c1assgn
   end type

   type innercomp2(n2,k2)    ! (20,4)
      integer, kind        :: k2
      integer, len         :: n2
      integer(k2), pointer :: j
   end type


   type comp1(n3,k3)    ! (20,4)
      integer, kind                   :: k3
      integer, len                    :: n3
      integer(k3)                     :: i
      type(innercomp1(:,k3)), pointer :: ic1
   end type

   type comp2(n4,k4)    ! (20,4)
      integer, kind           :: k4
      integer, len            :: n4
      integer(k4)             :: j
      type(innercomp2(n4,k4)) :: ic2
   end type

   type base(k5,n5)    ! (4,20)
      integer, kind                   :: k5
      integer, len                    :: n5
      type(comp1(n5,k5))              :: c1
      type(comp2(n5,k5))              :: c2
      type(innercomp1(:,k5)), pointer :: ic1
      type(innercomp2(n5,k5))         :: ic2
   end type

   contains

      subroutine c1assgn ( a, b )
         class(innercomp1(*,4)), intent(out) :: a
         class(innercomp1(*,4)), intent(in) :: b

         error stop 1_4

      end subroutine

end module

program genericAssignmentDtIntrinAssgn007
   use m

   type(base(4,20)) :: b1
   class(base(4,20)), pointer :: b2
   class(base(4,20)), allocatable :: b3

   type(innercomp1(:,4)), pointer :: ic1
   integer, pointer :: i1

   allocate ( i1, source = 30 )
   allocate ( ic1 , source = innercomp1(20,4)(i1) )

   allocate ( b2, b3 )

   b1 = base(4,20)( comp1(20,4)( 10, ic1 ), comp2(20,4)(i1, innercomp2(20,4)(i1)), ic1, innercomp2(20,4)(i1) )
   print *, b1%c1%i, b1%c1%ic1%i, b1%c2%j, b1%c2%ic2%j, b1%ic1%i, b1%ic2%j

   select type ( b2 )
      type is ( base(4,*) )
         b2 = b1
         print *, b2%c1%i, b2%c1%ic1%i, b2%c2%j, b2%c2%ic2%j, b2%ic1%i, b2%ic2%j

         if (  ( .not. associated(b1%c1%ic1%i, b2%c1%ic1%i ) ) .or. &
         &     ( .not. associated(b1%c2%ic2%j, b2%c2%ic2%j ) ) .or. &
         &     ( .not. associated(b1%ic1%i, b2%ic1%i ) )       .or. &
         &     ( .not. associated(b1%ic2%j, b2%ic2%j ) ) )     error stop 1_4

   end select

   select type ( b3 )
      type is ( base(4,*) )
         b3 = b1
         print *, b3%c1%i, b3%c1%ic1%i, b3%c2%j, b3%c2%ic2%j, b3%ic1%i, b3%ic2%j

         if (  ( .not. associated(b1%c1%ic1%i, b3%c1%ic1%i ) ) .or. &
         &     ( .not. associated(b1%c2%ic2%j, b3%c2%ic2%j ) ) .or. &
         &     ( .not. associated(b1%ic1%i, b3%ic1%i ) )       .or. &
         &     ( .not. associated(b1%ic2%j, b3%ic2%j ) ) )     error stop 2_4

         if (  ( .not. associated(b2%c1%ic1%i, b3%c1%ic1%i ) ) .or. &
         &     ( .not. associated(b2%c2%ic2%j, b3%c2%ic2%j ) ) .or. &
         &     ( .not. associated(b2%ic1%i, b3%ic1%i ) )       .or. &
         &     ( .not. associated(b2%ic2%j, b3%ic2%j ) ) )     error stop 3_4

   end select

end program
