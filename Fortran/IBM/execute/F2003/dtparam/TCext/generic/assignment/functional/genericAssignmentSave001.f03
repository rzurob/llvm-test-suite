! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/generic/assignment/functional/genericAssignmentSave001.f
! opt variations: -qnol -qnodeferredlp

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with assignment
!*
!*  DESCRIPTION                : assignment: pass-obj specified
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
         procedure, pass(a) :: ab
         generic :: assignment(=) => ab
   end type

   contains

   subroutine ab ( a, b )
      class(base(*,4)), intent(out)   :: a
      class(base(*,4)), intent(in)   :: b

      integer, save :: i = 0

      a%i = b%i + i

      i = i + 1

      print *,'ab'

   end subroutine

end module

program genericAssignmentSave001
   use m

   type(base(20,4)) :: b1
   class(base(:,4)), allocatable :: b2

   allocate ( b2, source = base(20,4)(100) )

   b1 = b2
   print *, b1

   b1 = b2
   print *, b1

   b1 = b2
   print *, b1

   b2 = b1
   print *, b2%i

   b2 = b1
   print *, b2%i

   b2 = b1
   print *, b2%i

end program
