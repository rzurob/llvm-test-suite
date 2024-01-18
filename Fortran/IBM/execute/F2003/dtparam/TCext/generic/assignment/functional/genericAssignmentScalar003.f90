! GB DTP extension using:
! ftcx_dtp -ql -qnodeferredlp /tstdev/F2003/generic/assignment/functional/genericAssignmentScalar003.f
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
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : assignment: operands with non-poly scalar with pointer components
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
      integer, kind        :: k1
      integer, len         :: n1
      integer(k1), pointer :: i => null()
      contains
         procedure, pass :: bassgn
         generic :: assignment(=) => bassgn
   end type


   contains

      subroutine bassgn ( a, b )
         class(base(*,4)), intent(out) :: a
         type(base(*,4)), intent(in) :: b

         if ( .not. associated ( a%i ) ) allocate ( a%i )

         a%i => b%i
         print *, a%i, '=>', b%i

      end subroutine

end module


program genericAssignmentScalar003
   use m

   type(base(20,4)), pointer :: b1
   type(base(20,4)), allocatable :: b2
   type(base(20,4)), target :: b3

   allocate ( b1, source = base(20,4)() )
   allocate ( b1%i, source = 10 )

   allocate ( b2 )
   b2 = b1

   if ( ( b1%i /= 10 ) .or. ( b2%i /= 10 ) ) error stop 1_4

   b2%i = 100
   if ( ( b1%i /= 100 ) .or. ( b2%i /= 100 ) ) error stop 2_4

   b3 = b1
   if ( ( b1%i /= 100 ) .or. ( b2%i /= 100 ) .or. ( b3%i /= 100 ) ) error stop 3_4
   if ( ( .not. ( associated( b3%i, b2%i ) ) ) .or.  ( .not. ( associated( b3%i, b1%i ) ) ) ) error stop 4_4

   deallocate ( b1, b2 )

   allocate ( b1, b2 )
   allocate ( b1%i, b2%i )

   if ( associated ( b1%i, b2%i ) ) error stop 5_4

   b2%i = 1000
   b1 = b2

   if ( ( b1%i /= 1000 ) .or. ( b2%i /= 1000 ) .or. ( .not. associated ( b1%i , b2%i ) ) ) error stop 6_4

end program
