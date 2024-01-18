! GB DTP extension using:
! ftcx_dtp -ql -qnodeferredlp /tstdev/F2003/generic/assignment/functional/genericAssignmentParameter002.f
! opt variations: -qnol -qdeferredlp

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
!*  SECONDARY FUNCTIONS TESTED : with assignment
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : assignment: named-constant (parameter) should still invoke the generic tb procedures
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
         procedure :: ab
         generic :: assignment(=) => ab
   end type

   type, extends( base ) :: child    ! (20,4)
      contains
         procedure :: c
         generic :: assignment(=) => c
   end type

   contains

   elemental subroutine ab ( a, b )
      class(base(*,4)), intent(out) :: a
      class(base(*,4)), intent(in) :: b
      a%i = b%i
   end subroutine

   subroutine c ( a, b )
      class(child(*,4)), intent(out) :: a
      class(base(*,4)), intent(in) :: b(:)

      a%i = b(1)%i
      do i = 2, size ( b )
         a%i = a%i + b(i)%i
      end do
   end subroutine


end module

program genericAssignmentParameter002
   use m

   type(base(20,4)), parameter :: b1(4) = (/ ( base(20,4)(10*i), i = 1, 4 ) /)
   type(child(20,4)), parameter :: c1(4) = (/ ( child(20,4)(100*i), i = 1, 4 ) /)
   
   class(base(20,4)), allocatable :: b2, b3(:)
   class(child(20,4)), pointer :: c2, c3(:)

   allocate ( b2, c2, b3(4), c3(4) )

   b2 = b1(4)
   print *, b2%i

   c2 = c1
   print *, c2%i

   b3 = b1
   print *, b3%i

   c3 = c1
   print *, c3%i

   deallocate ( b3 )
   allocate ( child(20,4) :: b3(4) )

   b3 = c1
   print *, b3%i

end program
