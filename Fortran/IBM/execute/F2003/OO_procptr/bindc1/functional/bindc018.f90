!*  ===================================================================
!*
!*  DATE                       : 06/07/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Procedure Pointer with BIND(C) feature
!*                                        Try procedure pointer pointing C procedure when dummy argument and
!*                                        function return are of C INTRINSIC types but defined with typedef
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

   use ISO_C_BINDING

   interface
      function printints(i1, i2, i3) bind(C, name="pint")
         import C_INT, C_SHORT, C_LONG
         integer(C_INT) :: printints
         integer(C_INT), intent(in), value   :: i1
         integer(C_SHORT), intent(in), value :: i2
         integer(C_LONG), intent(in)         :: i3
      end function
   end interface

   interface
      function printreals(r1, r2, r3) bind(C, name="preal")
         import C_FLOAT, C_DOUBLE, C_LONG_DOUBLE
         real(C_FLOAT) :: printreals
         real(C_FLOAT), intent(in), value          :: r1
         real(C_DOUBLE), intent(in), value         :: r2
         real(C_LONG_DOUBLE), intent(in)           :: r3
      end function
   end interface

   interface
      subroutine printlogandcomplex(l1,c1) bind(C, name="pLnC")
         import C_BOOL, C_FLOAT_COMPLEX
         logical(C_BOOL), intent(in), value   :: l1
         complex(C_FLOAT_COMPLEX), intent(in) :: c1
      end subroutine
   end interface

   procedure(printints), pointer, bind(C)          :: pp1
   procedure(printreals), pointer, bind(C)         :: pp2
   procedure(printlogandcomplex), pointer, bind(C) :: pp3

   logical :: precision_r4

   integer(C_INT)   :: i11, i12
   integer(C_SHORT) :: i13
   integer(C_LONG)  :: i14

   real(C_FLOAT)    :: r11
   real(C_DOUBLE)   :: r12 = 2002.0_C_DOUBLE

   pp1 => printints
   pp2 => printreals
   pp3 => printlogandcomplex

   i12 = 1001_C_INT
   i13 = 1002_C_SHORT
   i14 = 1003_C_LONG

   i11 = pp1 ( i12, i13, i14 )
   if ( i11 /= 3006 ) error stop 1_4

   r11 = pp2 ( 2001.0_C_FLOAT, r12, 2003.0_C_LONG_DOUBLE )
   if ( precision_r4( r1, 6006.0 ) ) error stop 1_4

   call pp3 ( .true._C_BOOL , (3001.0_C_FLOAT, 3002.0_C_FLOAT ) )

end
