! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/generic/assignment/functional/genericAssignmentScalar006.f
! opt variations: -ql

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : assignment: operands with non-poly scalar with pointer dummy arg
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

   type base(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: i
      contains
         procedure, pass :: bassgn
         generic :: assignment(=) => bassgn
   end type

   contains

      subroutine bassgn ( a, b )
         class(base(4)), intent(out) :: a
         type(base(4)), intent(in) :: b

         a%i = b%i
         print *, a%i, '=', b%i
      end subroutine

      subroutine bassgnAlloc ( a, b )
         class(base(4)), intent(out) :: a
         type(base(4)), pointer, intent(in) :: b

         if ( associated ( b ) ) then
             a = b
         else
            a%i = -999
            print *, 'operand on RHS of assignment is disassociated'
         end if
      end subroutine

end module


program genericAssignmentScalar006
   use m

   type(base(4)) :: b1
   type(base(4)), pointer :: b2
   type(base(4)), allocatable :: b3

   b1 = base(4)(10)
   allocate ( b2, b3 )

   b2 = base(4)(20)
   b3 = base(4)(30)

   b2 = b1
   if ( b2%i /= 10 ) error stop 1_4

   b1 = b3
   if ( b1%i /= 30 ) error stop 2_4

!   b1 = b2
   call bassgnAlloc (b1, b2)
   if ( b1%i /= 10 ) error stop 3_4

   nullify ( b2 )

!   b3 = b2
   call bassgnAlloc (b3, b2)
   if ( b3%i /= -999 ) error stop 4_4

end program
