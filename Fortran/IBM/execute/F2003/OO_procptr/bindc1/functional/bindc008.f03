!*  ===================================================================
!*
!*  DATE                       : 06/07/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Procedure Pointer with BIND(C) feature
!*                                        Procedure Pointer pointing at FORTRAN functions which returns a procedure pointer that
!*                                        is pointing to C functions with BIND(C) derived type function return and dummy arguments
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

   type, BIND(C) :: bc
      integer(C_INT)    :: i
      real(C_FLOAT)     :: d
      character(kind=C_CHAR) :: c(4)
   end type

end module

module m1
   use m, only: bc, C_INT, C_FLOAT, C_CHAR

   interface
      subroutine cprint( dtv ) BIND(C, NAME="c_PrInT")
         import bc
         type(bc), intent(in) :: dtv
      end subroutine
   end interface

   interface
      function combine( dtv1, dtv2 ) BIND(C, NAME="CCombine")
         import bc
         type(bc) :: combine
         type(bc), intent(in) :: dtv1, dtv2
      end function
   end interface

end module

program bindc008
   use m1

   type(bc) :: b1, b2, b3
   procedure(cprint), pointer  :: pp1
   procedure(combine), pointer :: pp2

   b1 = bc(101_C_INT, 101.0_C_FLOAT, (/ C_CHAR_"a", C_CHAR_"b", C_CHAR_"c", C_CHAR_"d" /) )
   b2 = bc(102_C_INT, 102.0_C_FLOAT, (/ C_CHAR_"A", C_CHAR_"B", C_CHAR_"C", C_CHAR_"D" /) )

   pp1 => cprint

   call pp1( b1 )
   call pp1( b2 )

   pp2 => combine

   b3 = pp2(b1, b2)
   call pp1 ( b3 )

   nullify ( pp1 )
   nullify ( pp2 )

   if ( ( associated( pp1 ) ) .or. ( associated( pp2 ) ) .or. ( associated( pp1, pp2 ) ) ) error stop 1_4

end
