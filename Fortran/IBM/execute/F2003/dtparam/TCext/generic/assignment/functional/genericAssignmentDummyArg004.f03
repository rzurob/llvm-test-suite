! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/generic/assignment/functional/genericAssignmentDummyArg004.f
! opt variations: -qnol -qnodeferredlp

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : assignment: non-poly (explicit size) array dummy arguments being the operand
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
         procedure, pass :: bamt
         generic :: assignment(=) => bamt
   end type

   interface
      subroutine bamt ( a, b )
         import base
         class(base(*,4)), intent(out) :: a
         type(base(*,4)), intent(in) :: b(3)
      end subroutine
   end interface

end module

program genericAssignmentDummyArg004
   use m

   type(base(20,4)) :: b1, b2(5)
   type(base(:,4)), allocatable :: b3(:)

   b1 = base(20,4)(100)
   print *, b1%i

   b2 = base(20,4)(200)
   print *, b2%i

   call assignment( b1, b2 )
   print *, b1%i

   allocate ( b3(3), source = (/ base(20,4)(1), base(20,4)(2), base(20,4)(3) /) )

   call assignment (b1, b3)
   print *, b1

   call assignment1( b1, b2(5:1:-2))
    print *, b1%i

   call assignment1 (b1, b3((/2,1,2/)))
   print *, b1

   contains

      subroutine assignment(a, b)
         type(base(20,4)), intent(inout) :: a
         type(base(20,4)), intent(in)  :: b(5)

         print *, 'assignment'
         a = b

      end subroutine

      subroutine assignment1(a, b)
         type(base(20,4)), intent(inout) :: a
         type(base(20,4)), intent(in)  :: b(2:4)

         print *, 'assignment1'
         a = b(4:2:-1)

      end subroutine

end program

subroutine bamt ( a, b )
   use m, only: base
   class(base(*,4)), intent(out) :: a
   type(base(*,4)), intent(in) :: b(3)

   a%i = b(1)%i
   do k = 2,3
      a%i = b(k)%i + a%i
   end do

   print *, 'bamt'

end subroutine
