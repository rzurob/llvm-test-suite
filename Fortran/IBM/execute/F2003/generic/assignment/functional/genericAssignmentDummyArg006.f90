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
         class(base), intent(in)  :: b(:)

         a%i = 0
         do k = 1,size(b)
            a%i = b(k)%i + a%i
         end do

         print *, 'ab'

      end subroutine

      subroutine c( a, b )
         class(child), intent(out) :: a
         class(base), intent(in)  :: b(:)

         a%i = 0
         do k = 1,size(b)
            a%i = b(k)%i + a%i
         end do

         select type ( b )
            type is ( child )
               a%j = 0
               do k = 1,size(b)
                  a%j = b(k)%j + a%j
               end do
         end select

         print *, 'c'

      end subroutine

end module

program genericAssignmentDummyArg006
   use m

   class(base), allocatable :: b1, b2(:)
   class(child), allocatable :: c1(:)

   allocate ( b1, b2(4), c1(4))

   do i=1,4
      b2(i)%i = i*100
      c1(i)%i = i*200
      c1(i)%j = -1*i*200
   end do

   call assumedsize (b1, b2)
   print *, b1%i

   call assumedsize (b1, c1)
   print *, b1%i

   call assumedsize2d (b1, b2)
   print *, b1%i

   call assumedsize2d (b1, c1)
   print *, b1%i

   deallocate ( b1 )
   allocate ( child :: b1 )

   call assumedsize (b1, b2)
   select type ( b1 )
      type is ( child )
         print *, b1%i, b1%j
   end select

   call assumedsize (b1, c1)
   select type ( b1 )
      type is ( child )
         print *, b1%i, b1%j
   end select

   call assumedsize2d (b1, b2)
   select type ( b1 )
      type is ( child )
         print *, b1%i, b1%j
   end select

   call assumedsize2d (b1, c1)
   select type ( b1 )
      type is ( child )
         print *, b1%i, b1%j
   end select

   contains

      subroutine assumedsize(a, b)
         class(base), intent(out) :: a
         class(base), intent(in) :: b(*)

         print *, 'assumedsize'
         a = b(1:4)



      end subroutine

      subroutine assumedsize2d(a, b)
         class(base), intent(out) :: a
         class(base), intent(in) :: b(2,*)

         print *, 'assumedsize2d'
         a = b(1:2,2)

      end subroutine

end program
