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

   type base
      integer :: i = -999
      contains
         procedure, pass :: bassgnelem
         generic :: assignment(=) => bassgnelem
   end type

   type, extends(base) :: child
      integer :: j = -999
   end type

   contains

      elemental subroutine bassgnelem ( a, b )
         class(base), intent(inout) :: a
         class(base), intent(in) :: b

         a%i = b%i+ 1

         select type ( a )
            type is ( child )
               select type ( b )
                  type is ( child )
                     a%j = b%j + 1
               end select
         end select

      end subroutine

end module

program genericAssignmentElemental002
   use m

   class(base), pointer :: b1
   class(base), allocatable :: b2(:)
   class(base), pointer :: b3(:)
   class(child), pointer :: b4(:,:)
   class(child), allocatable :: b5(:,:)

   allocate ( b1, source = base(10) )
   allocate ( base :: b2(4), b3(4) )

   do i=1,4
      b2(i)%i = i*10
      b3(i)%i = i*20
   end do

   b2 = b3
   print *, b2%i

   b3 = b1
   print *, b3%i

   allocate ( child :: b4(2,2), b5(2,2) )

   do i=1,2
      do j=1,2
         b4(j,i) = child(j*i*10, -1*j*i*20)
      end do
   end do

   print *, b4%i
   print *, b4%j

   b5=b4
   print *, b5%i
   print *, b5%j

end program
