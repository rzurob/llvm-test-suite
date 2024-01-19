! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/F2003/generic/operator/functional/genericOperatorDummyArg007.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator(.ggg.)
!*
!*  DESCRIPTION                : Operator: poly (assumed-size) array dummy arguments being the operand
!*                                           also defined Operator arguments are assumed-size
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
         procedure :: ab
         generic :: operator(.ggg.) => ab
   end type

   type, extends(base) :: child    ! (20,4)
      integer(k1) :: j = -999
      contains
         procedure :: ab => c
   end type

   contains

      integer function ab( a, b )
         class(base(*,4)), intent(in) :: a
         class(base(*,4)), intent(in)  :: b(*)

         allocatable :: ab(:)

         allocate ( ab(4) )

         do k = 1,4
            ab(k) = b(k)%i + a%i
         end do

         print *, 'ab'

      end function

      integer function c( a, b )
         class(child(*,4)), intent(in) :: a
         class(base(*,4)), intent(in)  :: b(*)
         allocatable :: c(:)

         allocate ( c(4))
         do k = 1,4
            c(k) = b(k)%i + a%i
         end do


         select type ( b )
            type is ( child(*,4) )
               do k = 1,4
                  c(k) = c(k) + b(k)%j + a%j
               end do
         end select

         print *, 'c'

      end function

      integer function assumedsize(a, b)
         class(base(*,4)), intent(in) :: a
         class(base(*,4)), intent(in) :: b(*)

         allocatable :: assumedsize(:)

         print *, 'assumedsize'

         allocate ( assumedsize(4), source = a .ggg. b(1:4) )

      end function

end module

program genericOperatorDummyArg007
   use m

   class(base(:,4)), allocatable  :: b1, b2(:), b3(:,:)
   class(child(:,4)), allocatable :: c1, c2(:), c3(:,:)

   integer :: i(4)

   allocate ( base(20,4):: b1, b2(4), b3(2,2) )
   allocate ( child(20,4):: c1, c2(4), c3(2,2) )

   b1%i = 100
   c1%i = 1000
   c1%j = 2000

   print *, '============================================='
   i= assumedsize( b1, (/ base(20,4)(1), base(20,4)(2), base(20,4)(3), base(20,4)(4) /) )
   print *, i

   do k = 1, 4
      b2(k)%i = k*10
   end do

   i= assumedsize ( b1, b2 )
   print *, i

   do k = 1, 2
      do j = 1, 2
         b3(j,k)%i = k*j*10
      end do
   end do

   i= assumedsize ( b1, b3 )
   print *, i

   i= assumedsize2d( b1, (/ base(20,4)(1), base(20,4)(2), base(20,4)(3), base(20,4)(4) /) )
   print *, i

   i= assumedsize2d ( b1, b2 )
   print *, i

   i= assumedsize2d ( b1, b3 )
   print *, i


   print *, '============================================='
   i= assumedsize( c1, (/ ( child(20,4)(j, -1*j ), j = 1, 4 ) /) )
   print *,i

   do k = 1, 4
      c2(k)%i = k*10
      c2(k)%j = -1*c2(k)%i
   end do

   i= assumedsize ( c1, c2 )
   print *,i

   do k = 1, 2
      do j = 1, 2
         c3(j,k)%i = k*j
         c3(j,k)%j = k*j*k*j
      end do
   end do

   i= assumedsize ( c1, c3 )
   print *,i

   i= assumedsize2d( c1, (/ ( child(20,4)(j, -1*j ), j = 1, 4 ) /) )
   print *,i

   i= assumedsize2d ( c1, c2 )
   print *,i

   i= assumedsize2d ( c1, c3 )
   print *,i

   print *, '============================================='

   deallocate ( b1 )
   allocate ( b1, source = child(20,4)(100, 200 ) )

   i= assumedsize( b1, (/ ( child(20,4)(j, -1*j ), j = 1, 4 ) /) )
   print *,i

   do k = 1, 4
      c2(k)%i = k*10
      c2(k)%j = -1*c2(k)%i
   end do

   i= assumedsize ( b1, c2 )
   print *,i

   do k = 1, 2
      do j = 1, 2
         c3(j,k)%i = k*j
         c3(j,k)%j = k*j*k*j
      end do
   end do

   i= assumedsize ( b1, c3 )
   print *,i

   i= assumedsize2d( b1, (/ ( child(20,4)(j, -1*j ), j = 1, 4 ) /) )
   print *,i

   i= assumedsize2d ( b1, c2 )
   print *,i

   i= assumedsize2d ( b1, c3 )
   print *,i

   print *, '============================================='

   contains

      integer function assumedsize2d(a, b)
         class(base(*,4)), intent(in) :: a
         class(base(*,4)), intent(in) :: b(1,*)
         allocatable :: assumedsize2d(:)
         print *, 'assumedsize2d'

         allocate ( assumedsize2d(4), source = a .ggg. b(1,1:4) )

      end function

end program
