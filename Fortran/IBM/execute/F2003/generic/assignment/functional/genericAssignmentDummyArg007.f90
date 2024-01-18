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

   type base
      integer :: i = -999
      contains
         procedure :: ab
         generic :: assignment(=) => ab
   end type

   type, extends(base) :: child
      integer :: j = -999
      contains
         procedure :: ab => c
   end type

   contains

      subroutine ab( a, b )
         class(base), intent(out) :: a
         class(base), intent(in)  :: b(*)

         a%i = 0
         do k = 1,4
            a%i = b(k)%i + a%i
         end do

         print *, 'ab'

      end subroutine

      subroutine c( a, b )
         class(child), intent(out) :: a
         class(base), intent(in)  :: b(*)

         a%i = 0
         do k = 1,4
            a%i = b(k)%i + a%i
         end do

         select type ( b )
            type is ( child )
               a%j = 0
               do k = 1,4
                  a%j = b(k)%j + a%j
               end do
         end select

         print *, 'c'

      end subroutine

      subroutine assumedsize(a, b)
         class(base), intent(out) :: a
         class(base), intent(in) :: b(*)

         print *, 'assumedsize'
         a = b(1:4)

      end subroutine

end module

program genericAssignmentDummyArg007
   use m

   class(base), allocatable  :: b1, b2(:), b3(:,:)
   class(child), allocatable :: c1, c2(:), c3(:,:)

   allocate ( b1, b2(4), b3(2,2), c1, c2(4), c3(2,2) )

   print *, '============================================='
   call assumedsize( b1, (/ base(1), base(2), base(3), base(4) /) )
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

   call assumedsize2d( b1, (/ base(1), base(2), base(3), base(4) /) )
   print *, b1%i

   call assumedsize2d ( b1, b2 )
   print *, b1%i

   call assumedsize2d ( b1, b3 )
   print *, b1%i


   print *, '============================================='
   call assumedsize( c1, (/ ( child(j, -1*j ), j = 1, 4 ) /) )
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

   call assumedsize2d( c1, (/ ( child(j, -1*j ), j = 1, 4 ) /) )
   print *, c1%i, c1%j

   call assumedsize2d ( c1, c2 )
   print *, c1%i, c1%j

   call assumedsize2d ( c1, c3 )
   print *, c1%i, c1%j

   print *, '============================================='

   deallocate ( b1 )
   allocate ( child :: b1 )

   call assumedsize( b1, (/ ( child(j, -1*j ), j = 1, 4 ) /) )
   select type ( b1 )
      type is ( child )
         print *, b1%i, b1%j
   end select

   do i = 1, 4
      c2(i)%i = i*10
      c2(i)%j = -1*c2(i)%i
   end do

   call assumedsize ( b1, c2 )
   select type ( b1 )
      type is ( child )
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
      type is ( child )
         print *, b1%i, b1%j
   end select

   call assumedsize2d( b1, (/ ( child(j, -1*j ), j = 1, 4 ) /) )
   select type ( b1 )
      type is ( child )
         print *, b1%i, b1%j
   end select

   call assumedsize2d ( b1, c2 )
   select type ( b1 )
      type is ( child )
         print *, b1%i, b1%j
   end select

   call assumedsize2d ( b1, c3 )
   select type ( b1 )
      type is ( child )
         print *, b1%i, b1%j
   end select

   print *, '============================================='

   contains

      subroutine assumedsize2d(a, b)
         class(base), intent(out) :: a
         class(base), intent(in) :: b(1,*)

         print *, 'assumedsize2d'
         a = b(1,1:4)

      end subroutine

end program
