! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/F2003/generic/assignment/functional/genericAssignmentScalar019.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

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
!*  DESCRIPTION                : assignment: with some more class hierarchy
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
   end type

   type, extends(base) :: c1    ! (20,4)
      integer(k1) :: j = -999
      contains
         procedure, private :: c1ab
         generic :: assignment(=) => c1ab
   end type

   type, extends(base) :: c2    ! (20,4)
      integer(k1) :: j = -999
      contains
         procedure, private :: c2ab
         generic :: assignment(=) => c2ab
   end type

   contains

   subroutine c1ab ( a, b )
      class(c1(*,4)), intent(out) :: a
      class(base(*,4)), intent(in) :: b

      select type ( b )
         type is ( base(*,4) )
            a%i = b%i
         type is ( c1(*,4) )
            a%i = b%i
            a%j = b%j
         type is ( c2(*,4) )
            a%i = b%i
            a%j = b%j
      end select

      print *, 'c1ab'

   end subroutine

   subroutine c2ab ( a, b )
      class(c2(*,4)), intent(out) :: a
      class(base(*,4)), intent(in) :: b

      select type ( b )
         type is ( base(*,4) )
            a%i = b%i
         type is ( c1(*,4) )
            a%i = b%i
            a%j = b%j
         type is ( c2(*,4) )
            a%i = b%i
            a%j = b%j
      end select

      print *, 'c2ab'

   end subroutine

end module

program genericAssignmentScalar019
   use m

   class(base(:,4)), allocatable :: b1

   type (c1(20,4)) :: c11
   class(c1(20,4)), allocatable :: c12

   type (c2(20,4)) :: c21
   class(c2(20,4)), pointer :: c22

   allocate ( c12, c22 )
   allocate ( b1, source = base(20,4)(100) )

   c11 = b1
   c22 = b1

   if ( ( c11%i /= 100 ) .or. ( c11%j /= -999 ) .or. &
        ( c22%i /= 100 ) .or. ( c22%j /= -999 ) ) error stop 1_4

   c12 = c2(20,4)(300, 400 )
   c21 = c1(20,4)(500, 600 )

   if ( ( c12%i /= 300 ) .or. ( c12%j /= 400 ) .or. &
        ( c21%i /= 500 ) .or. ( c21%j /= 600 ) ) error stop 2_4

   c22 = c12
   c12 = c21

   if ( ( c12%i /= 500 ) .or. ( c12%j /= 600 ) .or. &
        ( c22%i /= 300 ) .or. ( c22%j /= 400 ) ) error stop 3_4

end program
