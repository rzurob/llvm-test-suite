!*  ===================================================================
!*
!*  DATE                       : 06/07/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Procedure Pointer with BIND(C) feature
!*                                        Procedure Pointer pointing at C functions with BIND(C) interface subroutine
!*                                        - dummy argument with INTEGERS Array explicit shape/assumed size
!*                                          and actual argument being allocatable (C_INT, C_SHORT)
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module cinterfaces

   use ISO_C_BINDING

   interface
      subroutine printinteger(i1, i2) BIND(C)
         import C_INT, C_SHORT
         integer(C_INT), intent(in)          :: i1(3)
         integer(C_SHORT), intent(in)        :: i2(3,*)
     end subroutine
   end interface

   interface
      subroutine printadd(i1, i2) BIND(C, name='addarray')
         import C_INT, C_SHORT
         integer(C_INT), intent(in)          :: i1(3)
         integer(C_SHORT), intent(in)        :: i2(3,*)
     end subroutine
   end interface

end module

   use cinterfaces

   interface
      subroutine intinterface1(i1, i2) BIND(C)
         import C_INT, C_SHORT
         integer(C_INT), intent(in)          :: i1(3)
         integer(C_SHORT), intent(in)        :: i2(3,*)
      end subroutine
   end interface

   procedure(intinterface1), bind(c), pointer :: p1, p2
   integer(C_INT), dimension(3) :: i1 = (/ 101_C_INT, 102_C_INT, 103_C_INT /)
   integer(C_SHORT), dimension(:), allocatable :: i2

   allocate ( i2(6) )

   i2 = (/ 201_C_INT, 202_C_INT, 203_C_INT, 211_C_INT, 212_C_INT, 213_C_INT /)

   p1 => printinteger
   call p1 ( i1, i2 )

   p1 => printadd
   call p1 ( i1, i2 )

end
