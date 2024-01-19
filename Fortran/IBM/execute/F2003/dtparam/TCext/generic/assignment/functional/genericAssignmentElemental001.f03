! GB DTP extension using:
! ftcx_dtp -ql -qnodeferredlp /tstdev/F2003/generic/assignment/functional/genericAssignmentElemental001.f
! opt variations: -qnol -qdeferredlp

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : assignment: with non-poly elemental assignment
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
         procedure, pass :: bassgnelem

         generic :: assignment(=) => bassgnelem
   end type

   contains

      elemental subroutine bassgnelem ( a, b )
         class(base(*,4)), intent(inout) :: a
         type(base(*,4)), intent(in) :: b

         a%i = b%i+ 1
      end subroutine

end module

program genericAssignmentElemental001
   use m

   type(base(20,4)) :: b1
   type(base(20,4)), allocatable :: b2(:), b3(:)

   type(base(20,4)), pointer :: b4(:,:), b5(:,:)

   b1 = base(20,4)(10)
   print *,b1%i

   allocate ( b2(4), b3(4) )

   do i=1,4
      b2(i)%i = i*10
      b3(i)%i = i*20
   end do

   b2 = b3
   print *, b2%i

   b3 = b1
   print *, b3%i

   allocate ( b4(2,2), b5(2,2) )

   do i=1,2
      do j=1,2
         b4(j,i) = base(20,4)(j*i*10)
      end do
   end do

   print *, b4%i

   b5=b4
   print *, b5%i

end program
