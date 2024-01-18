! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv /tstdev/F2003/generic/assignment/dtIntrinAssgn/genericAssignmentDtIntrinAssgn010.f
! opt variations: -qnok -ql -qdefaultpv -qreuse=self

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
!*  DESCRIPTION                : Derived Type Intrinsic Assignment:
!*                                      - try a derived type containing an derived type scalar and 1d array, but only
!*                                        scalar intrinic assignment is defined for generic type bound, 1d array assignment
!*                                        is defined with interface
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
         procedure :: bassgn
         generic :: assignment(=) => bassgn
   end type

   type container(k2)    ! (4)
      integer, kind  :: k2
      type(base(k2)) :: b1 ! scalar
      type(base(k2)) :: b2(5) ! array
   end type

   interface assignment(=)
      module procedure barrayassgn
   end interface

   contains

      subroutine bassgn ( a, b )
         class(base(4)), intent(out) :: a
         class(base(4)), intent(in) :: b

         a%i = b%i
         print *, 'bassgn'

      end subroutine

      subroutine barrayassgn ( a, b )
         class(base(4)), intent(out) :: a(:)
         class(base(4)), intent(in) :: b(:)

         if ( size ( a ) /= size ( b ) ) error stop 1_4

         do j = 1, size ( a )
            a(j)%i = b(j)%i
         end do

         print *, 'barrayassgn'

      end subroutine

end module

program genericAssignmentDtIntrinAssgn010
   use m

   type(container(4)) :: c1, c2
   type(container(4)), allocatable :: c3
   type(base(4)) :: b1(5) = (/ (base(4)(i), i = 5, 1, -1) /)

   c1 = container(4)( base(4)(1), (/ ( base(4)(i),i = 2, 6 ) /) )
   c2 = c1

   print *, c1
   print *, c2

   allocate ( c3 )
   c3 = c2

   print *, c3

   c2%b2 = c1%b2
   print *, c2%b2

   c3%b2 = b1
   print *, c3%b2

end program
