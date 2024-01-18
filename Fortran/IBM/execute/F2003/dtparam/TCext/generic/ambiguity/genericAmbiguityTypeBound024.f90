! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/generic/ambiguity/genericAmbiguityTypeBound024.f
! opt variations: -ql

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
!*  SECONDARY FUNCTIONS TESTED : ambiguious generic interfaces
!*
!*  DRIVER STANZA              : xlf2003
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

   type b1(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: i
      contains
         procedure, nopass :: twoargs1
         procedure, nopass :: twoargs2
         generic :: twoargs => twoargs1, twoargs2
   end type

   type, extends(b1) :: c1    ! (4)
   end type

   type, extends(b1) :: c2    ! (4)
   end type

   type, extends(c1) :: g1    ! (4)
      contains
         procedure, nopass :: twoargs4
         generic :: twoargs => twoargs4
   end type

   type, extends(c2) :: g2    ! (4)
      contains
         procedure, nopass :: twoargs3
         generic :: twoargs => twoargs3
   end type

   contains

      subroutine twoargs1(a, b)
         class(b1(4)), intent(in) :: a
         type(b1(4)), intent(in) :: b

         print *, 'twoargs1'

      end subroutine

      subroutine twoargs2(a, b)
         type(b1(4)), intent(in) :: a
         class(c1(4)), intent(in) :: b

         print *, 'twoargs2'

      end subroutine

      subroutine twoargs3(a, b)
         class(c1(4)), intent(in) :: a
         class(c1(4)), intent(in) :: b

         print *, 'twoargs3'

      end subroutine

      subroutine twoargs4(a, b)
         class(c1(4)), intent(in) :: a
         class(c2(4)), intent(in) :: b

         print *, 'twoargs4'

      end subroutine

end module

module binoperator

   type b11(k2)    ! (4)
      integer, kind :: k2
      integer(k2)   :: i
   end type

   type, extends(b11) :: c11    ! (4)
      contains
         procedure, pass :: add1
         generic :: operator(.mybin.) => add1
   end type

   type, extends(b11) :: c12    ! (4)
   end type

   type, extends(c11) :: g11    ! (4)
      contains
         procedure, pass :: add2
         generic :: operator(.mybin.) => add2
   end type

   type, extends(c12) :: g12    ! (4)
      contains
         procedure, pass(a) :: add3
         generic :: operator(.mybin.) => add3
   end type

   contains

      type(b11(4)) function add1(a,b)
         class(c11(4)), intent(in) :: a
         type(b11(4)), intent(in) :: b

         add1 = b11(4)(10)
         print *,'add1'

      end function

      type(b11(4)) function add2(b,a)
         class(g11(4)), intent(in) :: b
         class(c12(4)), intent(in) :: a

         add2 = b11(4)(10)
         print *,'add2'

      end function

      type(b11(4)) function add3(b,a)
         type(b11(4)), intent(in) :: b
         class(g12(4)), intent(in) :: a

         add3 = b11(4)(10)
         print *,'add3'

      end function

end module

module assignment

   type b21(k3)    ! (4)
      integer, kind :: k3
      integer(k3)   :: i
   end type

   type, extends(b21) :: c21    ! (4)
      contains
         procedure, pass(b) :: assgn1
         generic :: assignment(=) => assgn1
   end type

   type, extends(b21) :: c22    ! (4)
      contains
         procedure, pass :: assgn2
         generic :: assignment(=) => assgn2
   end type

   type, extends(c21) :: g21    ! (4)

   end type

   type, extends(c22) :: g22    ! (4)
   end type

   contains

      subroutine assgn1(a, b)
         type(g22(4)), intent(out) :: a
         class(c21(4)), intent(in)  :: b
         
         print *, 'assgn1'

      end subroutine

      subroutine assgn2(a, b)
         class(c22(4)), intent(out) :: a
         type(b21(4)), intent(in)  :: b
         
         print *, 'assgn2'

      end subroutine

end module

program genericAmbiguityTypeBound024
   use genericname
   use binoperator
   use assignment

   type(b1(4)) :: b1_1
   type(c1(4)) :: c1_1
   type(c2(4)) :: c2_1
   type(g1(4)) :: g1_1
   type(g2(4)) :: g2_1
   
   type(b11(4)) :: b11_1
   type(c11(4)) :: c11_1
   type(c12(4)) :: c12_1
   type(g11(4)) :: g11_1
   type(g12(4)) :: g12_1
   
   type(b21(4)) :: b21_1
   type(c21(4)) :: c21_1
   type(c22(4)) :: c22_1
   type(g21(4)) :: g21_1
   type(g22(4)) :: g22_1
   
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
