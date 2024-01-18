! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/generic/assignment/functional/genericAssignmentDummyArg002.f
! opt variations: -qnol -qnodeferredlp

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : assignment: non-poly pointer, allocatable dummy arguments being the operand
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
         type(base(*,4)), intent(in) :: b
      end subroutine
   end interface

end module

program genericAssignmentDummyArg002
   use m

   type(base(:,4)) :: b1, b2
   pointer :: b1
   allocatable :: b2

   integer, parameter :: null = 100
   integer, parameter :: twohundred = 200

   call assignment( b1, b2 )
   print *, b1%i

   deallocate ( b2 )
   allocate ( b2, source = base(20,4) (1000) )

   call assignment ( b1, b2 )
   print *, b1%i

   contains

      subroutine assignment(a, b)
         type(base(:,4)), pointer, intent(inout)     :: a
         type(base(:,4)), allocatable, intent(inout)  :: b

         print *, 'assignment'
         if ( .not. associated(a) ) allocate ( a , source = base(20,4)(null) )
         if ( .not. allocated(b) ) allocate ( b , source = base(20,4)(twohundred) )

         a = b

      end subroutine

end program

subroutine bamt ( a, b )
   use m, only: base
   class(base(*,4)), intent(out) :: a
   type(base(*,4)), intent(in) :: b

   a%i = b%i + 1

   print *, 'bamt'

end subroutine
