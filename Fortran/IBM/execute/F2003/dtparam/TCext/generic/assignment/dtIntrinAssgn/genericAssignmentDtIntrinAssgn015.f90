! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qnodeferredlp -qreuse=self /tstdev/F2003/generic/assignment/dtIntrinAssgn/genericAssignmentDtIntrinAssgn015.f
! opt variations: -qnok -qnol -qdefaultpv -qdeferredlp -qreuse=none

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
!*                                 - container type contains a derived type
!*                                     - container is an array object, and component
!*                                       is either scalar and array object that has
!*                                       elemental UD assignment
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
      integer(k1)   :: j
      contains
         procedure :: elemA
         generic :: assignment(=) => elemA
   end type inner

   type container(k2,n2)    ! (4,20)
      integer, kind      :: k2
      integer, len       :: n2
      type(inner(n2,k2)) :: i0
      type(inner(n2,k2)) :: i1(3)
   end type

   contains

      elemental subroutine elemA(a, b)
         class(inner(*,4)), intent(out) :: a
         class(inner(*,4)), intent(in)  :: b

         a%j = b%j + 1

      end subroutine

end module

program genericAssignmentDtIntrinAssgn015
   use m

   type(container(4,20)) :: a(3), b(:), c(:)

   pointer :: b
   allocatable :: c

   allocate ( b(3), c(3) )

   a = container(4,20)( i1= (/inner(20,4)(2), inner(20,4)(3), inner(20,4)(4) /), i0= inner(20,4)(1) )
   print *,a

   b = (/ container(4,20)( i0= inner(20,4)(1), i1= (/inner(20,4)(2), inner(20,4)(3), inner(20,4)(4) /) ), &
          ( container(4,20)( i0= inner(20,4)(i), i1= (/inner(20,4)(i+1), inner(20,4)(i+2), inner(20,4)(i+3) /) ), i = 5,9,4) /)
   print *, b

   c = b((/2,3,1/))
   print *, c

end program
