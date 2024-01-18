! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/generic/ambiguity/genericAmbiguityInterface004d.f
! opt variations: -ql

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : ambiguious generic interfaces
!*
!*  DESCRIPTION                : ambiguous but between interface and type bounds
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

   type base1(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: i
      contains
         procedure, pass :: adda
         generic :: operator(*) => adda
   end type

   type, extends(base1) :: child1    ! (4)
   end type

   type, extends(child1) :: gen1    ! (4)
   end type

   contains

      type(base1(4)) function adda(a, b)
         class(base1(4)), intent(in) :: a
         class(child1(4)), intent(in)  :: b

         adda = base1(4)(10)

      end function

      type(gen1(4)) function addb(a, b)
         class(gen1(4)), intent(in) :: a
         type(gen1(4)), intent(in)  :: b

         addb = gen1(4)(10)

      end function

end module

module assignment

   type base2(k2)    ! (4)
     integer, kind :: k2
     integer(k2)   :: i, j
      contains
         procedure, pass :: assgn1
         generic :: assignment(=) => assgn1
   end type

   type, extends(base2) :: child2    ! (4)
   end type

   type, extends(base2) :: child3    ! (4)
   end type

   contains

      subroutine assgn1(a, b)
         class(base2(4)), intent(out) :: a
         class(child2(4)), intent(in)  :: b

      end subroutine

      subroutine assgn2(a, b)
         class(child3(4)), intent(out) :: a
         class(base2(4)), intent(in)  :: b

      end subroutine

end module

program genericAmbiguityInterface004d
   use assignment
   use binoperator

   interface assignment(=)
      procedure assgn2
   end interface

   interface operator(*)
      procedure addb
   end interface

end program
