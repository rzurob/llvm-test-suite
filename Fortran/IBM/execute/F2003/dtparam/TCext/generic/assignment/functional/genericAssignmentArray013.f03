! GB DTP extension using:
! ftcx_dtp -ql -qnodeferredlp -qreuse=base /tstdev/F2003/generic/assignment/functional/genericAssignmentArray013.f
! opt variations: -qnol -qdeferredlp -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : assignment: polymorphic array with multiple different rank
!*                                           child type providing overridding tb
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
      integer(k1)   :: i = 0
      contains
         procedure, pass :: bassgn1d
         procedure, pass :: bassgn2d
         procedure, pass :: bassgn3d
         procedure, pass :: bassgn4d

         generic :: assignment(=) => bassgn1d, bassgn2d, bassgn3d, bassgn4d
   end type

   type, extends(base) :: child    ! (20,4)
      integer(k1) :: j = 0
      contains
         procedure, pass :: bassgn1d => cassgn1d
         procedure, pass :: bassgn2d => cassgn2d
         procedure, pass :: bassgn3d => cassgn3d
         procedure, pass :: bassgn4d => cassgn4d

   end type

   contains

      subroutine cassgn1d ( a, b )
         class(child(*,4)), intent(out) :: a
         class(base(*,4)), intent(in) :: b(:)

         print *, 'cassgn1d'

         do j = 1, size(b)
            a%i = a%i + b(j)%i
         end do

         select type ( b )
            type is ( child(*,4) )
               do j = 1, size(b)
                  a%j = a%j + b(j)%j
               end do
         end select

      end subroutine

      subroutine cassgn2d ( a, b )
         class(child(*,4)), intent(out) :: a
         class(base(*,4)), intent(in) :: b(:,:)

         print *, 'cassgn2d'

         ! transform into rank 1 array and perform generic assignment again
         a = reshape ( source = b, shape = (/ size ( b ) /) )

      end subroutine

      subroutine cassgn3d ( a, b )
         class(child(*,4)), intent(out) :: a
         class(base(*,4)), intent(in) :: b(:,:,:)

         print *, 'cassgn3d'

         ! transform into rank 1 array and perform generic assignment again
         a = reshape ( source = b, shape = (/ size ( b ) /) )

      end subroutine

      subroutine cassgn4d ( a, b )
         class(child(*,4)), intent(out) :: a
         class(base(*,4)), intent(in) :: b(:,:,:,:)
         integer :: bsize = 0

         print *, 'cassgn4d'

         ! transform into rank 2 array and perform generic assignment again
         a = reshape ( source = b, shape = (/ int(sqrt(real(size ( b )))) , int(sqrt(real(size ( b )))) /) )

      end subroutine

      subroutine bassgn1d ( a, b )
         class(base(*,4)), intent(out) :: a
         class(base(*,4)), intent(in) :: b(:)

         print *, 'bassgn1d'

         do j = 1, size(b)
            a%i = a%i + b(j)%i
         end do

      end subroutine

      subroutine bassgn2d ( a, b )
         class(base(*,4)), intent(out) :: a
         class(base(*,4)), intent(in) :: b(:,:)

         print *, 'bassgn2d'

         ! transform into rank 1 array and perform generic assignment again
         a = reshape ( source = b, shape = (/ size ( b ) /) )

      end subroutine

      subroutine bassgn3d ( a, b )
         class(base(*,4)), intent(out) :: a
         class(base(*,4)), intent(in) :: b(:,:,:)

         print *, 'bassgn3d'

         ! transform into rank 1 array and perform generic assignment again
         a = reshape ( source = b, shape = (/ size ( b ) /) )

      end subroutine

      subroutine bassgn4d ( a, b )
         class(base(*,4)), intent(out) :: a
         class(base(*,4)), intent(in) :: b(:,:,:,:)
         integer :: bsize = 0

         print *, 'bassgn4d'

         ! transform into rank 2 array and perform generic assignment again
         a = reshape ( source = b, shape = (/ int(sqrt(real(size ( b )))) , int(sqrt(real(size ( b )))) /) )

      end subroutine

end module

program genericAssignmentArray013
   use m

   class(base(20,4)), allocatable :: b0
   class(base(20,4)), pointer :: b1(:)
   class(base(20,4)), allocatable :: b2(:,:)
   class(base(20,4)), pointer :: b3(:,:,:)
   class(base(20,4)), allocatable :: b4(:,:,:,:)

   type(child(20,4)) :: c0
   class(child(20,4)), pointer :: c1(:)
   class(child(20,4)), allocatable :: c2(:,:)
   class(child(20,4)), pointer :: c3(:,:,:)
   class(child(20,4)), allocatable :: c4(:,:,:,:)

   allocate ( b0, b1(4), b2(3,3), b3(2,2,2), b4(2,2,2,2) )
   allocate ( c1(4), c2(3,3), c3(2,2,2), c4(2,2,2,2) )

   do i = 1,4
      b1(i)%i = i
      c1(i)%i = i
      c1(i)%j = -1*i
   end do

   do i = 1,3
      do j = 1,3
         b2(j,i)%i = j*i
         c2(j,i)%i = j*i
         c2(j,i)%j = -1*j*i
      end do
   end do

   do i = 1,2
      do j = 1,2
         do k = 1,2
            b3(k,j,i)%i = i*j*k
            c3(k,j,i)%i = i*j*k
            c3(k,j,i)%j = -1*i*j*k
         end do
      end do
   end do

   do i = 1,2
      do j = 1,2
         do k = 1,2
            do l = 1,2
               b4(l,k,j,i)%i = i*j*k*l
               c4(l,k,j,i)%i = i*j*k*l
               c4(l,k,j,i)%j = -1*i*j*k*l
            end do
         end do
      end do
   end do

   print *, 'BASE=BASE'
   b0 = b1
   print *, b0%i

   b0 = b2
   print *, b0%i

   b0 = b3
   print *, b0%i

   b0 = b4
   print *, b0%i

   b0 = reshape ( source = b3, shape = (/ 4,2 /) )
   print *, b0%i

   print *, 'BASE=CHILD'
   b0 = c1
   print *, b0%i

   b0 = c2
   print *, b0%i

   b0 = c3
   print *, b0%i

   b0 = c4
   print *, b0%i

   b0 = reshape ( source = c3, shape = (/ 4,2 /) )
   print *, b0%i

   print *, 'CHILD=CHILD'
   c0 = c1
   print *, c0%i
   print *, c0%j

   c0 = c2
   print *, c0%i
   print *, c0%j

   c0 = c3
   print *, c0%i
   print *, c0%j

   c0 = c4
   print *, c0%i
   print *, c0%j

   c0 = reshape ( source = c3, shape = (/ 4,2 /) )
   print *, c0%i
   print *, c0%j

   deallocate ( b0, b1, b2, b3, b4 )
   allocate ( b0, source = c0 )
   allocate ( b1(4), source = c1 )
   allocate ( b2(3,3), source = c2 )
   allocate ( b3(2,2,2), source = c3 )
   allocate ( b4(2,2,2,2), source = c4 )

   print *, 'CHILD=CHILD'
   b0 = b1
   select type ( b0 )
      type is ( child(*,4) )
         print *, b0%i
         print *, b0%j
   end select

   b0 = b2
   select type ( b0 )
      type is ( child(*,4) )
         print *, b0%i
         print *, b0%j
   end select

   b0 = b3
   select type ( b0 )
      type is ( child(*,4) )
         print *, b0%i
         print *, b0%j
   end select

   b0 = b4
   select type ( b0 )
      type is ( child(*,4) )
         print *, b0%i
         print *, b0%j
   end select


end program