!*  ===================================================================
!*
!*  DATE                       : 06/07/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Procedure Pointer with BIND(C) feature
!*                                        Procedure Pointer pointing at C functions with BIND(C) interface subroutine
!*                                        - dummy argument function return with mix of other types array explicit shape/assumed size
!*                                          and actual argument being pointer/allocatable (C_INT, C_SHORT)
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

   interface
      subroutine print1(i1, i2, i3) BIND(C)
         import C_DOUBLE, C_CHAR, C_FLOAT_COMPLEX
         REAL(C_DOUBLE), intent(in)                    :: i1(3)
         character(kind=C_CHAR), intent(in)                 :: i2(2,2,*)
         COMPLEX(C_FLOAT_COMPLEX), intent(in)          :: i3(3)
     end subroutine
   end interface

   interface
      logical(C_BOOL) function print2(i1, i2) BIND(C)
         import C_DOUBLE, C_LONG_DOUBLE, C_BOOL
         real(C_DOUBLE), intent(in)             :: i1(3)
         real(C_LONG_DOUBLE), intent(in)        :: i2(5)
     end function
   end interface

end module

   use m

   procedure(print1), bind(c), pointer :: p1
   procedure(print2), bind(c), pointer :: p2

   REAL(C_DOUBLE), allocatable :: r1(:)
   REAL(C_LONG_DOUBLE), pointer :: r2(:)

   logical(C_BOOL) :: l1

   p1 => print1

   allocate ( r1(3), r2(5) )

   r1 = (/ 101.0_C_DOUBLE,102.0_C_DOUBLE,103.0_C_DOUBLE /)
   r2 = (/ 401.0_C_LONG_DOUBLE, 402.0_C_LONG_DOUBLE, 403.0_C_LONG_DOUBLE, 404.0_C_LONG_DOUBLE, 405.0_C_LONG_DOUBLE /)

   call p1(  r1 , (/ C_CHAR_"a", C_CHAR_"b" , C_CHAR_"c" , C_CHAR_"d" , C_CHAR_"e" , C_CHAR_"f", &
               C_CHAR_"g" , C_CHAR_"h" /) , (/ (201.0_C_FLOAT,202.0_C_FLOAT), (203.0_C_FLOAT,204.0_C_FLOAT), (205.0_C_FLOAT,206.0_C_FLOAT) /) )

   p2 => print2

   l1 = p2( (/ 301.0_C_DOUBLE,302.0_C_DOUBLE,303.0_C_DOUBLE /), r2 )

   if ( l1 .neqv. .true. ) error stop 1_4

end
