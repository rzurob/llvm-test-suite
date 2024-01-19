!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : ambiguious generic interfaces
!*
!*  DESCRIPTION                : With Class Hierarchy
!*
!*                                      ( Base )
!*                                      /        \
!*                                 (Child1)   (Child2)
!*                                     |         |
!*                                 (Gen31)    (Gen32)
!*
!*                                - positive test
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

module genericName

   type b1
      integer :: i
      contains
         procedure, nopass :: twoargs1
         procedure, nopass :: twoargs2
         generic :: twoargs => twoargs1, twoargs2
   end type

   type, extends(b1) :: c1
   end type

   type, extends(b1) :: c2
   end type

   type, extends(c1) :: g1
      contains
         procedure, nopass :: twoargs4
         generic :: twoargs => twoargs4
   end type

   type, extends(c2) :: g2
      contains
         procedure, nopass :: twoargs3
         generic :: twoargs => twoargs3
   end type

   contains

      subroutine twoargs1(a, b)
         class(b1), intent(in) :: a
         type(b1), intent(in) :: b

         print *, 'twoargs1'

      end subroutine

      subroutine twoargs2(a, b)
         type(b1), intent(in) :: a
         class(c1), intent(in) :: b

         print *, 'twoargs2'

      end subroutine

      subroutine twoargs3(a, b)
         class(c1), intent(in) :: a
         class(c1), intent(in) :: b

         print *, 'twoargs3'

      end subroutine

      subroutine twoargs4(a, b)
         class(c1), intent(in) :: a
         class(c2), intent(in) :: b

         print *, 'twoargs4'

      end subroutine

end module

module binoperator

   type b11
      integer :: i
   end type

   type, extends(b11) :: c11
      contains
         procedure, pass :: add1
         generic :: operator(.mybin.) => add1
   end type

   type, extends(b11) :: c12
   end type

   type, extends(c11) :: g11
      contains
         procedure, pass :: add2
         generic :: operator(.mybin.) => add2
   end type

   type, extends(c12) :: g12
      contains
         procedure, pass(a) :: add3
         generic :: operator(.mybin.) => add3
   end type

   contains

      type(b11) function add1(a,b)
         class(c11), intent(in) :: a
         type(b11), intent(in) :: b

         add1 = b11(10)
         print *,'add1'

      end function

      type(b11) function add2(b,a)
         class(g11), intent(in) :: b
         class(c12), intent(in) :: a

         add2 = b11(10)
         print *,'add2'

      end function

      type(b11) function add3(b,a)
         type(b11), intent(in) :: b
         class(g12), intent(in) :: a

         add3 = b11(10)
         print *,'add3'

      end function

end module

module assignment

   type b21
      integer :: i
   end type

   type, extends(b21) :: c21
      contains
         procedure, pass(b) :: assgn1
         generic :: assignment(=) => assgn1
   end type

   type, extends(b21) :: c22
      contains
         procedure, pass :: assgn2
         generic :: assignment(=) => assgn2
   end type

   type, extends(c21) :: g21

   end type

   type, extends(c22) :: g22
   end type

   contains

      subroutine assgn1(a, b)
         type(g22), intent(out) :: a
         class(c21), intent(in)  :: b

         print *, 'assgn1'

      end subroutine

      subroutine assgn2(a, b)
         class(c22), intent(out) :: a
         type(b21), intent(in)  :: b

         print *, 'assgn2'

      end subroutine

end module

program genericAmbiguityTypeBound024
   use genericname
   use binoperator
   use assignment

   type(b1) :: b1_1
   type(c1) :: c1_1
   type(c2) :: c2_1
   type(g1) :: g1_1
   type(g2) :: g2_1

   type(b11) :: b11_1
   type(c11) :: c11_1
   type(c12) :: c12_1
   type(g11) :: g11_1
   type(g12) :: g12_1

   type(b21) :: b21_1
   type(c21) :: c21_1
   type(c22) :: c22_1
   type(g21) :: g21_1
   type(g22) :: g22_1

   call b1_1%twoargs(b1_1,b1_1)
   call b1_1%twoargs(b1_1,c1_1)
   call g1_1%twoargs(g1_1,c2_1)
   call b1_1%twoargs(b1_1,g1_1)
   call g2_1%twoargs(g1_1,c1_1)

   b11_1 = c11_1 .mybin. b11_1
   b11_1 = g11_1 .mybin. g12_1
   b11_1 = b11_1 .mybin. g12_1

   g22_1 = c21_1
   g22_1 = g21_1

   c22_1 = b21_1
   g22_1 = b21_1


end program
