! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/F2003/generic/assignment/functional/genericAssignmentPass010.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with assignment
!*
!*  DESCRIPTION                : assignment: pass-obj specified with array dummy arg
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
         procedure, pass(b) :: ab
         generic :: assignment(=) => ab
   end type

   type, extends(base) :: child    ! (20,4)
      integer(k1) :: j = -999
      contains
         procedure, pass(b) :: ab => cb
         generic :: assignment(=) => ab
   end type

   contains

   subroutine ab ( a, b )
      class(base(*,4)), intent(out) :: a(:)
      class(base(*,4)), intent(in) :: b

      do i =0, size(a)-1
         a(i+1)%i = b%i + i
      end do

      print *,'ab'

   end subroutine

   subroutine cb ( a, b )
      class(base(*,4)), intent(out) :: a(:)
      class(child(*,4)), intent(in) :: b

      do i =0, size(a)-1
         a(i+1)%i = b%i + i
         select type ( a )
            type is ( child(*,4) )
               a(i+1)%j = b%j + i
         end select
      end do

      print *,'cb'

   end subroutine

end module

program genericAssignmentPass010
   use m

   class(base(:,4)), allocatable :: b1, b2(:)

   allocate ( b1, source = base(20,4)(100) )
   allocate ( base(20,4) :: b2(5) )

   b2 = b1
   print *, b2%i

   !! this is to avoid b2 and b2(5) passed to sub ab the same time
   b2 = base(20,4)(b2(5)%i)
   print *, b2%i

   b2 = base(20,4)(50)
   print *, b2%i

   deallocate ( b1 )
   allocate ( b1, source = child(20,4) (10, 20) )

   b2 = b1
   print *, b2%i

   deallocate ( b2 )
   allocate ( child(20,4) :: b2(4) )

   b2 = b1
   select type ( b2 )
      type is ( child(*,4) )
         print *, b2%i
         print *, b2%j
   end select

end program
