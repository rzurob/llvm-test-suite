! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=none /tstdev/F2003/generic/assignment/functional/genericAssignmentElemental002.f
! opt variations: -qnol -qnodeferredlp -qreuse=base

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
!*  DESCRIPTION                : assignment: polymorphic elemental assignment
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

   type, extends(base) :: child(n2,k2)    ! (20,4,20,4)
      integer, kind :: k2
      integer, len  :: n2
      integer(k2)   :: j = -999
   end type

   contains

      elemental subroutine bassgnelem ( a, b )
         class(base(*,4)), intent(inout) :: a
         class(base(*,4)), intent(in) :: b

         a%i = b%i+ 1

         select type ( a )
            type is ( child(*,4,*,4) )
               select type ( b )
                  type is ( child(*,4,*,4) )
                     a%j = b%j + 1
               end select
         end select

      end subroutine

end module

program genericAssignmentElemental002
   use m

   class(base(:,4)), pointer :: b1
   class(base(:,4)), allocatable :: b2(:)
   class(base(:,4)), pointer :: b3(:)
   class(child(:,4,:,4)), pointer :: b4(:,:)
   class(child(:,4,:,4)), allocatable :: b5(:,:)

   allocate ( b1, source = base(20,4)(10) )
   allocate ( base(20,4) :: b2(4), b3(4) )

   do i=1,4
      b2(i)%i = i*10
      b3(i)%i = i*20
   end do

   b2 = b3
   print *, b2%i

   b3 = b1
   print *, b3%i

   allocate ( child(20,4,20,4) :: b4(2,2), b5(2,2) )

   do i=1,2
      do j=1,2
         b4(j,i) = child(20,4,20,4)(j*i*10, -1*j*i*20)
      end do
   end do

   print *, b4%i
   print *, b4%j

   b5=b4
   print *, b5%i
   print *, b5%j

end program
