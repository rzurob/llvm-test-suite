! GB DTP extension using:
! ftcx_dtp -qnol -qnodefaultpv -qreuse=base /tstdev/F2003/generic/assignment/functional/genericAssignmentScalar018.f
! opt variations: -ql -qdefaultpv -qreuse=self -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : assignment: polymorphic component that has generic assignment tb as well
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

   type inner(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: x = -999
      contains
         procedure, private :: innerassignment
         generic, private :: assignment(=) => innerassignment
   end type

   type, extends(inner) :: innerchild    ! (4)
      integer(k1) :: y = -999
      contains
         procedure, private :: innerassignment => innerchildassignment
   end type

   type base(k2)    ! (4)
      integer, kind                 :: k2
      class(inner(k2)), allocatable :: i
      integer(k2)                   :: j
      contains
         generic :: assignment(=) => assignment
         procedure :: assignment
   end type

   contains

   subroutine innerassignment ( a, b)
      class(inner(4)), intent(out) :: a
      class(inner(4)), intent(in) :: b

      print *, 'innerassgn'
      a%x = b%x

   end subroutine

   subroutine innerchildassignment ( a, b)
      class(innerchild(4)), intent(out) :: a
      class(inner(4)), intent(in) :: b

      print *, 'innerchildassgn'
      a%inner = inner(4)( b%x )

      select type ( b )
         type is ( innerchild(4) )
            a%y = b%y
      end select

   end subroutine

   subroutine assignment ( a, b)
      class(base(4)), intent(out) :: a
      class(base(4)), intent(in) :: b

      print *, 'assign'
      if ( .not. allocated (a%i) ) allocate ( a%i , source = b%i )

      a%i = b%i
      a%j = b%j

   end subroutine

end module

program genericAssignmentScalar018
   use m

   type(base(4)) :: b1
   type(base(4)), allocatable :: b2

   b1 = base(4) ( inner(4)(50), 100 )

   print *, b1%i%x
   print *, b1%j

   allocate (  b2 )
   b2 = b1
   print *, b2%i%x
   print *, b2%j

   b1 = base(4) ( innerchild(4)(500, 1000) , 20000 )

   select type ( g => b1%i )
      type is (innerchild(4))
         print *, g%x, g%y
         print *, b1%j
   end select

   b2 = b1
   select type ( g => b1%i )
      type is (innerchild(4))
         print *, g%x, g%y
         print *, b1%j
   end select

end program
