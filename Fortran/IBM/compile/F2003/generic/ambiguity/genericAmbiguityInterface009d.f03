!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : ambiguious generic interfaces
!*
!*  DESCRIPTION                : ambiguous but between interface and type bounds, external procs with arrays
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

   type base1
      integer :: i
   end type

   type, extends(base1) :: child1
   end type

   type, extends(child1) :: gen1
      contains
         procedure, pass :: adda
         generic :: operator(*) => adda
   end type

   type, extends(child1) :: gen2
   end type

   interface
      type(base1) function adda(a, b)
         import gen1, child1, base1
         class(gen1), intent(in) :: a
         class(child1), intent(in)  :: b(:,:)
      end function
   end interface

   interface
      type(gen1) function addb(a, b)
         import gen1
         class(gen1), intent(in) :: a
         type(gen1), intent(in)  :: b(:,:)
      end function
   end interface

end module

module assignment

   type base2
     integer :: i, j
   end type

   type, extends(base2) :: child2
   end type

   type, extends(base2) :: child3
      contains
         procedure, pass :: assgn1
         generic :: assignment(=) => assgn1
   end type

   interface
      recursive subroutine assgn1(a, b)
         import child2, child3
         class(child3), intent(out) :: a
         class(child2), intent(in)  :: b(:,:,:)

      end subroutine
   end interface

end module

type(gen1) function addb(a, b)
   use binoperator, only: gen1
   class(gen1), intent(in) :: a
   type(gen1), intent(in)  :: b(:,:)

   addb = gen1(10)

end function

subroutine assgn2(a, b)
   use assignment
   class(child3), intent(out) :: a
   class(base2), intent(in)  :: b(:,:,:)

end subroutine

subroutine assgn1(a, b)
   use assignment, only: base2, child2
   class(base2), intent(out) :: a
   class(child2), intent(in)  :: b(:,:,:)

end subroutine

program genericAmbiguityInterface009d
   use assignment
   use binoperator

   interface
      subroutine assgn2(a, b)
         import child3, base2
         class(child3), intent(out) :: a
         class(base2), intent(in)  :: b(:,:,:)
      end subroutine
   end interface

   interface assignment(=)
      procedure assgn2
   end interface

   interface operator(*)
      procedure addb
   end interface

end program

type(base1) function adda(a, b)
   use binoperator, only: base1, child1
   class(base1), intent(in) :: a
   class(child1), intent(in)  :: b(:,:)

   adda = base1(10)

end function
