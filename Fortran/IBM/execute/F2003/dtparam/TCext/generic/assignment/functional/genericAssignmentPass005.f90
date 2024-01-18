! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/F2003/generic/assignment/functional/genericAssignmentPass005.f
! opt variations: -ql -qreuse=none

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
!*  SECONDARY FUNCTIONS TESTED : with assignment
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : assignment: pass-obj specified polymorphic assignment to different derived types
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
         procedure, pass(b) :: base1_base
         generic :: assignment(=) => base1_base
   end type

   type, extends(base) :: child    ! (4)
      integer(k1) :: j = -999
      contains
         procedure, pass(b) :: base1_base => base1_child
   end type

   type base1(k2)    ! (8)
      integer, kind :: k2
      integer(k2)   :: k = -999
      contains
         procedure, pass(b) :: base_base1
         generic :: assignment(=) => base_base1
   end type

   type, extends(base1) :: child1    ! (8)
      integer(k2) :: l = -999
      contains
         procedure, pass(b) :: base_base1 => base_child1
   end type

   contains

   subroutine base1_base ( a, b )
      class(base1(8)), intent(out) :: a
      class(base(4)), intent(in)   :: b

      a%k = INT(b%i,8)

      print *,'base1_base'

   end subroutine

   subroutine base_base1 ( a, b )
      class(base(4)), intent(out) :: a
      class(base1(8)), intent(in) :: b

      a%i = INT(b%k,4)

      print *,'base_base1'

   end subroutine

   subroutine base1_child ( a, b )
      class(base1(8)), intent(out) :: a
      class(child(4)), intent(in)   :: b

      a%k = INT(b%i,8)

      select type ( a )
         type is ( child1(8) )
            a%l = INT(b%j,8)
      end select

      print *,'base1_child'

   end subroutine

   subroutine base_child1 ( a, b )
      class(base(4)), intent(out) :: a
      class(child1(8)), intent(in) :: b

      a%i = INT(b%k,4)

      select type ( a )
         type is ( child(4) )
            a%j = b%l
      end select

      print *,'base_child1'

   end subroutine


end module

program genericAssignmentPass005
   use m

   class(base(4)), allocatable  :: b1
   class(base1(8)), allocatable :: bb1

   allocate ( b1, bb1 )

   b1 = base1(8)(1000)
   print *, b1%i

   b1 = child1(8)(500, 1000)
   print *, b1%i

   bb1 = base(4)(10)
   print *, bb1%k

   bb1 = child(4)(5, 10)
   print *, bb1%k

   bb1 = b1
   print *, bb1%k

   bb1%k = 50

   b1 = bb1
   print *, b1%i

   deallocate ( b1, bb1 )

   allocate ( child(4) :: b1 )
   allocate ( child1(8) :: bb1 )

   b1 = base1(8)(1)
   print *, b1%i

   b1 = child1(8)(5, 10)
   print *, b1%i

   bb1 = base(4)(1)
   print *, bb1%k

   bb1 = child(4)(5, 10)
   print *, bb1%k

   bb1 = b1
   print *, bb1%k

   bb1%k = 50

   b1 = bb1
   print *, b1%i

end program
