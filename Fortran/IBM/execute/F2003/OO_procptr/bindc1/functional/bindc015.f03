!*  ===================================================================
!*
!*  DATE                       : 06/07/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Procedure Pointer with BIND(C) feature
!*                                        Dummy Argument being Procedure pointer ( containing BIND(C) type dummy argument ) pass by reference
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

   use ISO_C_BINDING

   type, bind(C) :: base
      integer(C_INT) :: i1, i2
   end type

   interface
      subroutine print1(t) bind(c)
         import base
         type(base), intent(in) :: t
      end subroutine
   end interface

   interface
      subroutine print2(t) bind(c,name='second')
         import base
         type(base), intent(in) :: t
      end subroutine
   end interface

   interface
      integer(C_INT) function geti1(t) bind(c,name='firstget')
         import base, C_INT
         type(base), intent(in) :: t
      end function
   end interface

   interface
      integer(C_INT) function geti2(t) bind(c,name='secondget')
         import base, C_INT
         type(base), intent(in) :: t
      end function
   end interface

end module

   use m

      type(base) :: b1, b2(3)
      procedure(print2), pointer, bind(C) :: pp
      procedure(geti2), pointer, bind(C) :: ppp

      b1 = base(101, 102)
      b2 =(/ b1, base(201, 202), base(301, 302) /)

      pp => print1
      call foo(pp,b1)
      pp => print2
      call foo(pp,b1)

      pp => print1
      call foo(pp,b2(2))
      pp => print2
      call foo(pp,b2(2))

      ppp => geti1
      if ( ( bar(ppp, b1) /= 101 ) .or. ( bar(ppp, b2(3)) /= 301 ) ) error stop 1_4

      ppp => geti2
      if ( ( bar(ppp, b1) /= 102 ) .or. ( bar(ppp, b2(3)) /= 302 ) ) error stop 2_4

   contains

      subroutine foo (a,t)
         procedure(print1), pointer, bind(C) :: a
         type(base), intent(in) :: t

         call a(t)

      end subroutine

      integer(C_INT) function bar (a,t)
         procedure(geti1), pointer, bind(C) :: a
         type(base), intent(in) :: t

         bar = a(t)

      end function

end
