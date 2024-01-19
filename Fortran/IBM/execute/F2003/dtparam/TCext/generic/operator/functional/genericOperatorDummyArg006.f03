! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/F2003/generic/operator/functional/genericOperatorDummyArg006.f
! opt variations: -ql -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator(**)
!*
!*  DESCRIPTION                : Operator: poly (assumed-size) array dummy arguments being the operand
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
      integer(k1)   :: i = -999
      contains
         procedure :: ab
         generic :: operator(**) => ab
   end type

   type, extends(base) :: child    ! (4)
      integer(k1) :: j = -999
      contains
         procedure :: ab => c
   end type

   contains

      integer function ab( a, b )
         class(base(4)), intent(in) :: a
         class(base(4)), intent(in)  :: b(:)
         allocatable :: ab(:)

         allocate ( ab(size(b)))

         do k = 1,size(b)
            ab(k) = b(k)%i + a%i
         end do

         print *, 'ab'

      end function

      integer function c( a, b )
         class(child(4)), intent(in) :: a
         class(base(4)), intent(in)  :: b(:)
         allocatable :: c(:)

         allocate ( c(size(b)))

         do k = 1,size(b)
            c(k) = b(k)%i + a%i
         end do

         select type ( b )
            type is ( child(4) )
               do k = 1,size(b)
                  c(k) = b(k)%j + a%j
               end do
         end select

         print *, 'c'

      end function

end module

program genericOperatorDummyArg006
   use m

   class(base(4)), allocatable :: b1, b2(:)
   class(child(4)), allocatable :: c1(:)

   integer :: i(4)

   allocate ( b1, b2(4), c1(4))

   b1%i = 100
   do k=1,4
      b2(k)%i = k*100
      c1(k)%i = k*200
      c1(k)%j = -1*k*200
   end do

   i= assumedsize (b1, b2)
   print *, i

   i= assumedsize (b1, c1)
   print *, i

   i= assumedsize2d (b1, b2)
   print *, i

   i= assumedsize2d (b1, c1)
   print *, i

   deallocate ( b1 )
   allocate ( b1, source = child(4) ( 200, 300 ) )

   i= assumedsize (b1, b2)
   print *, i

   i= assumedsize (b1, c1)
   print *, i

   i= assumedsize2d (b1, b2)
   print *, i

   i= assumedsize2d (b1, c1)
   print *, i


   contains

      integer function assumedsize(a, b)
         class(base(4)), intent(in) :: a
         class(base(4)), intent(in) :: b(*)

         allocatable :: assumedsize(:)
         print *, 'assumedsize'

         allocate ( assumedsize(4) )

         assumedsize = a ** b(1:4)

      end function

      integer function assumedsize2d(a, b)
         class(base(4)), intent(in) :: a
         class(base(4)), intent(in) :: b(2,*)

         allocatable :: assumedsize2d(:)
         print *, 'assumedsize2d'
         allocate ( assumedsize2d(4) )

         assumedsize2d(1:2) = a ** b(1:2,2)
         assumedsize2d(3:4) = a ** (/ b(1,1), b(2,1) /)

      end function

end program
