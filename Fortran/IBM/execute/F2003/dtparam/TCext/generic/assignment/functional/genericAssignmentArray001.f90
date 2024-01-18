! GB DTP extension using:
! ftcx_dtp -ql -qnodeferredlp /tstdev/F2003/generic/assignment/functional/genericAssignmentArray001.f
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
!*  DESCRIPTION                : assignment: operands with non-poly explicit shape array
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
      integer(k1)   :: i =0
      contains
         procedure, pass :: bassgn
         generic :: assignment(=) => bassgn
   end type

   contains

      subroutine bassgn ( a, b )
         class(base(*,4)), intent(out) :: a
         type(base(*,4)), intent(in) :: b(5)

         do i = 1, size(b)
            a%i = a%i + b(i)%i
         end do

         print *, 'bassgn'

      end subroutine

end module


program genericAssignmentArray001
   use m

   type(base(20,4)) :: b1
   type(base(20,4)), allocatable :: b2(:)
   type(base(20,4)), pointer :: b3(:)

   allocate ( b2(5), b3(7) )
   b2 = (/ (base(20,4)(i),i=10,14) /)

   b3 = (/ (base(20,4)(i),i=20,26) /)

   b1 = b2
   print *, b1

   b1 = b3
   print *, b1

   b1 = b3(2:6)
   print *, b1

end program
