! GB DTP extension using:
! ftcx_dtp -ql -qnodeferredlp -qreuse=base /tstdev/F2003/generic/assignment/functional/genericAssignmentDummyArg006.f
! opt variations: -qnol -qdeferredlp -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
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

   type base(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)   :: i = -999
      contains
         procedure :: ab
         generic :: assignment(=) => ab
   end type

   type, extends(base) :: child    ! (20,4)
      integer(k1) :: j = -999
      contains
         procedure :: ab => c
   end type

   contains

      subroutine ab( a, b )
         class(base(*,4)), intent(out) :: a
         class(base(*,4)), intent(in)  :: b(:)

         a%i = 0
         do k = 1,size(b)
            a%i = b(k)%i + a%i
         end do

         print *, 'ab'

      end subroutine

      subroutine c( a, b )
         class(child(*,4)), intent(out) :: a
         class(base(*,4)), intent(in)  :: b(:)

         a%i = 0
         do k = 1,size(b)
            a%i = b(k)%i + a%i
         end do

         select type ( b )
            type is ( child(*,4) )
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

   class(base(20,4)), allocatable :: b1, b2(:)
   class(child(20,4)), allocatable :: c1(:)

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
   allocate ( child(20,4) :: b1 )

   call assumedsize (b1, b2)
   select type ( b1 )
      type is ( child(*,4) )
         print *, b1%i, b1%j
   end select

   call assumedsize (b1, c1)
   select type ( b1 )
      type is ( child(*,4) )
         print *, b1%i, b1%j
   end select

   call assumedsize2d (b1, b2)
   select type ( b1 )
      type is ( child(*,4) )
         print *, b1%i, b1%j
   end select

   call assumedsize2d (b1, c1)
   select type ( b1 )
      type is ( child(*,4) )
         print *, b1%i, b1%j
   end select

   contains

      subroutine assumedsize(a, b)
         class(base(*,4)), intent(out) :: a
         class(base(*,4)), intent(in) :: b(*)

         print *, 'assumedsize'
         a = b(1:4)



      end subroutine

      subroutine assumedsize2d(a, b)
         class(base(*,4)), intent(out) :: a
         class(base(*,4)), intent(in) :: b(2,*)

         print *, 'assumedsize2d'
         a = b(1:2,2)

      end subroutine

end program
