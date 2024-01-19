! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/F2003/generic/assignment/functional/genericAssignmentDummyArg007.f
! opt variations: -ql -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : assignment: poly (assumed-size) array dummy arguments being the operand
!*                                           also defined assignment arguments are assumed-size
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
         generic :: assignment(=) => ab
   end type

   type, extends(base) :: child    ! (4)
      integer(k1) :: j = -999
      contains
         procedure :: ab => c
   end type

   contains

      subroutine ab( a, b )
         class(base(4)), intent(out) :: a
         class(base(4)), intent(in)  :: b(*)

         a%i = 0
         do k = 1,4
            a%i = b(k)%i + a%i
         end do

         print *, 'ab'

      end subroutine

      subroutine c( a, b )
         class(child(4)), intent(out) :: a
         class(base(4)), intent(in)  :: b(*)

         a%i = 0
         do k = 1,4
            a%i = b(k)%i + a%i
         end do

         select type ( b )
            type is ( child(4) )
               a%j = 0
               do k = 1,4
                  a%j = b(k)%j + a%j
               end do
         end select

         print *, 'c'

      end subroutine

      subroutine assumedsize(a, b)
         class(base(4)), intent(out) :: a
         class(base(4)), intent(in) :: b(*)

         print *, 'assumedsize'
         a = b(1:4)

      end subroutine

end module

program genericAssignmentDummyArg007
   use m

   class(base(4)), allocatable  :: b1, b2(:), b3(:,:)
   class(child(4)), allocatable :: c1, c2(:), c3(:,:)

   allocate ( b1, b2(4), b3(2,2), c1, c2(4), c3(2,2) )

   print *, '============================================='
   call assumedsize( b1, (/ base(4)(1), base(4)(2), base(4)(3), base(4)(4) /) )
   print *, b1%i

   do i = 1, 4
      b2(i)%i = i*10
   end do

   call assumedsize ( b1, b2 )
   print *, b1%i

   do i = 1, 2
      do j = 1, 2
         b3(j,i)%i = i*j*10
      end do
   end do

   call assumedsize ( b1, b3 )
   print *, b1%i

   call assumedsize2d( b1, (/ base(4)(1), base(4)(2), base(4)(3), base(4)(4) /) )
   print *, b1%i

   call assumedsize2d ( b1, b2 )
   print *, b1%i

   call assumedsize2d ( b1, b3 )
   print *, b1%i


   print *, '============================================='
   call assumedsize( c1, (/ ( child(4)(j, -1*j ), j = 1, 4 ) /) )
   print *, c1%i, c1%j

   do i = 1, 4
      c2(i)%i = i*10
      c2(i)%j = -1*c2(i)%i
   end do

   call assumedsize ( c1, c2 )
   print *, c1%i, c1%j

   do i = 1, 2
      do j = 1, 2
         c3(j,i)%i = i*j
         c3(j,i)%j = i*j*i*j
      end do
   end do

   call assumedsize ( c1, c3 )
   print *, c1%i, c1%j

   call assumedsize2d( c1, (/ ( child(4)(j, -1*j ), j = 1, 4 ) /) )
   print *, c1%i, c1%j

   call assumedsize2d ( c1, c2 )
   print *, c1%i, c1%j

   call assumedsize2d ( c1, c3 )
   print *, c1%i, c1%j

   print *, '============================================='

   deallocate ( b1 )
   allocate ( child(4) :: b1 )

   call assumedsize( b1, (/ ( child(4)(j, -1*j ), j = 1, 4 ) /) )
   select type ( b1 )
      type is ( child(4) )
         print *, b1%i, b1%j
   end select

   do i = 1, 4
      c2(i)%i = i*10
      c2(i)%j = -1*c2(i)%i
   end do

   call assumedsize ( b1, c2 )
   select type ( b1 )
      type is ( child(4) )
         print *, b1%i, b1%j
   end select

   do i = 1, 2
      do j = 1, 2
         c3(j,i)%i = i*j
         c3(j,i)%j = i*j*i*j
      end do
   end do

   call assumedsize ( b1, c3 )
   select type ( b1 )
      type is ( child(4) )
         print *, b1%i, b1%j
   end select

   call assumedsize2d( b1, (/ ( child(4)(j, -1*j ), j = 1, 4 ) /) )
   select type ( b1 )
      type is ( child(4) )
         print *, b1%i, b1%j
   end select

   call assumedsize2d ( b1, c2 )
   select type ( b1 )
      type is ( child(4) )
         print *, b1%i, b1%j
   end select

   call assumedsize2d ( b1, c3 )
   select type ( b1 )
      type is ( child(4) )
         print *, b1%i, b1%j
   end select

   print *, '============================================='

   contains

      subroutine assumedsize2d(a, b)
         class(base(4)), intent(out) :: a
         class(base(4)), intent(in) :: b(1,*)

         print *, 'assumedsize2d'
         a = b(1,1:4)

      end subroutine

end program
