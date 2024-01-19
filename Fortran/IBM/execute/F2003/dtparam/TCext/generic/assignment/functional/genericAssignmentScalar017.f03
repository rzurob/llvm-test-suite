! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=self /tstdev/F2003/generic/assignment/functional/genericAssignmentScalar017.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : assignment: component that has generic assignment tb as well
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

   type inner(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)   :: i = -999
      contains
         procedure, private :: innerassignment
         generic, private :: assignment(=) => innerassignment
   end type

   type base(k2,n2)    ! (4,20)
      integer, kind      :: k2
      integer, len       :: n2
      type(inner(n2,k2)) :: i
      contains
         generic :: assignment(=) => assignment
         procedure :: assignment
   end type

   contains

   subroutine innerassignment ( a, b)
      class(inner(*,4)), intent(out) :: a
      type(inner(*,4)), intent(in) :: b

      print *, 'innerassgn'
      a%i = b%i

   end subroutine

   subroutine assignment ( a, b)
      class(base(4,*)), intent(out) :: a
      class(base(4,*)), intent(in) :: b

      print *, 'assign'
      a%i = b%i

   end subroutine

end module

program genericAssignmentScalar017
   use m

   type(base(4,20)), pointer :: b1
   class(base(4,20)), allocatable :: b2

   allocate ( b1, b2 )

   b1 = base(4,20)( inner(20,4)(10) )
   b2 = b1

   if ( ( b1%i%i /= 10 ) .or. ( b2%i%i /= 10 ) ) error stop 1_4

   print *, 'end'
   b1%i = inner(20,4)(20) !<- should not use generic assignment
   b2%i = b1%i

   if ( ( b1%i%i /= 20 ) .or. ( b2%i%i /= 20 ) ) error stop 2_4

end program
