! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/generic/ambiguity/genericAmbiguityInterface007d.f
! opt variations: -qnol

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : ambiguious generic interfaces
!*
!*  DESCRIPTION                : ambiguous but between interface and type bounds, external procs
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module binoperator

   type base1(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)   :: i
   end type

   type, extends(base1) :: child1    ! (20,4)
   end type

   type, extends(child1) :: gen1    ! (20,4)
      contains
         procedure, pass :: adda
         generic :: operator(*) => adda
   end type

   type, extends(child1) :: gen2    ! (20,4)
   end type

   interface
      type(base1(20,4)) function adda(a, b)
         import gen1, child1, base1
         class(gen1(*,4)), intent(in) :: a
         class(child1(*,4)), intent(in)  :: b
      end function
   end interface

   interface
      type(gen1(20,4)) function addb(a, b)
         import gen1
         class(gen1(*,4)), intent(in) :: a
         type(gen1(*,4)), intent(in)  :: b
      end function
   end interface

end module

module assignment

   type base2(n2,k2)    ! (20,4)
     integer, kind :: k2
     integer, len  :: n2
     integer(k2)   :: i, j
   end type

   type, extends(base2) :: child2    ! (20,4)
   end type

   type, extends(base2) :: child3    ! (20,4)
      contains
         procedure, pass :: assgn1
         generic :: assignment(=) => assgn1
   end type

   interface
      subroutine assgn1(a, b)
         import child2, child3
         class(child3(*,4)), intent(out) :: a
         class(child2(*,4)), intent(in)  :: b

      end subroutine
   end interface

end module

type(gen1(20,4)) function addb(a, b)
   use binoperator, only: gen1
   class(gen1(*,4)), intent(in) :: a
   type(gen1(*,4)), intent(in)  :: b

   addb = gen1(20,4)(10)

end function

subroutine assgn2(a, b)
   use assignment
   class(child3(*,4)), intent(out) :: a
   class(base2(*,4)), intent(in)  :: b

end subroutine

subroutine assgn1(a, b)
   use assignment, only: child3, child2
   class(child3(*,4)), intent(out) :: a
   class(child2(*,4)), intent(in)  :: b

end subroutine

program genericAmbiguityInterface007d
   use assignment
   use binoperator

   interface
      subroutine assgn2(a, b)
         import child3, base2
         class(child3(*,4)), intent(out) :: a
         class(base2(*,4)), intent(in)  :: b
      end subroutine
   end interface

   interface assignment(=)
      procedure assgn2
   end interface

   interface operator(*)
      procedure addb
   end interface

end program

type(base1(20,4)) function adda(a, b)
   use binoperator, only: base1, child1
   class(base1(*,4)), intent(in) :: a
   class(child1(*,4)), intent(in)  :: b

   adda = base1(20,4)(10)

end function