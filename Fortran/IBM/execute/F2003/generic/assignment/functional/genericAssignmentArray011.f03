!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : assignment: with multiple different rank of assume-shape array
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
      integer :: i =0
      contains
         procedure, pass :: bassgn1d
         procedure, pass :: bassgn2d
         procedure, pass :: bassgn3d
         procedure, pass :: bassgn4d

         generic :: assignment(=) => bassgn1d, bassgn2d, bassgn3d, bassgn4d
   end type

   contains

      subroutine bassgn1d ( a, b )
         class(base), intent(out) :: a
         class(base), intent(in) :: b(:)

         print *, 'bassgn1d'

         do j = 1, size(b)
            a%i = a%i + b(j)%i
         end do

      end subroutine

      subroutine bassgn2d ( a, b )
         class(base), intent(out) :: a
         class(base), intent(in) :: b(:,:)

         print *, 'bassgn2d'

         ! transform into rank 1 array and perform generic assignment again
         a = reshape ( source = b, shape = (/ size ( b ) /) )

      end subroutine

      subroutine bassgn3d ( a, b )
         class(base), intent(out) :: a
         class(base), intent(in) :: b(:,:,:)

         print *, 'bassgn3d'

         ! transform into rank 1 array and perform generic assignment again
         a = reshape ( source = b, shape = (/ size ( b ) /) )

      end subroutine

      subroutine bassgn4d ( a, b )
         class(base), intent(out) :: a
         class(base), intent(in) :: b(:,:,:,:)
         integer :: bsize = 0

         print *, 'bassgn4d'

         ! transform into rank 2 array and perform generic assignment again
         a = reshape ( source = b, shape = (/ int(sqrt(real(size ( b )))) , int(sqrt(real(size ( b )))) /) )

      end subroutine


end module

program genericAssignmentArray011
   use m

   type(base) :: b0
   type(base), pointer :: b1(:)
   type(base), allocatable :: b2(:,:)
   type(base), pointer :: b3(:,:,:)
   type(base), allocatable :: b4(:,:,:,:)

   allocate ( b1(4), b2(3,3), b3(2,2,2), b4(2,2,2,2) )

   do i = 1,4
      b1(i)%i = i
   end do

   do i = 1,3
      do j = 1,3
         b2(j,i)%i = j*i
      end do
   end do

   do i = 1,2
      do j = 1,2
         do k = 1,2
            b3(k,j,i)%i = i*j*k
         end do
      end do
   end do

   do i = 1,2
      do j = 1,2
         do k = 1,2
            do l = 1,2
               b4(l,k,j,i)%i = i*j*k*l
            end do
         end do
      end do
   end do

   b0 = b1
   print *, b0%i

   b0 = b2
   print *, b0%i

   b0 = b3
   print *, b0%i

   b0 = b4
   print *, b0%i

   b0 = reshape ( source = b3, shape = (/ 4,2 /) )
   print *, b0

end program
