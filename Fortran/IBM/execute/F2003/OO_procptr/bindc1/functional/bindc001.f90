!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 06/07/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Testing: Procedure Pointer with BIND(C) feature
!*                                        Procedure Pointer pointing at C functions with BIND(C) interface subroutine
!*                                        - dummy argument with INTEGERS (C_INT, C_SHORT, C_LONG, C_LONG_LONG, CSIGNED_CHAR)
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
      subroutine printinteger(i1, i2, i3, i4, i5) BIND(C)
         import C_INT, C_SHORT, C_LONG, C_LONG_LONG, C_SIGNED_CHAR
         integer(C_INT), value, intent(in)          :: i1
         integer(C_SHORT), value, intent(in)        :: i2
         integer(C_LONG), value, intent(in)         :: i3
         integer(C_LONG_LONG), value, intent(in)    :: i4
         integer(C_SIGNED_CHAR), value, intent(in)  :: i5
     end subroutine
   end interface

   interface
      subroutine printintegerplus100(i1, i2, i3, i4, i5) BIND(C)
         import C_INT, C_SHORT, C_LONG, C_LONG_LONG, C_SIGNED_CHAR
         integer(C_INT), value, intent(in)          :: i1
         integer(C_SHORT), value, intent(in)        :: i2
         integer(C_LONG), value, intent(in)         :: i3
         integer(C_LONG_LONG), value, intent(in)    :: i4
         integer(C_SIGNED_CHAR), value, intent(in)  :: i5
     end subroutine
   end interface

end module

   use cinterfaces

   interface
      subroutine intinterface1(i1, i2, i3, i4, i5) BIND(C)
         import C_INT, C_SHORT, C_LONG, C_LONG_LONG, C_SIGNED_CHAR
         integer(C_INT), value, intent(in)          :: i1
         integer(C_SHORT), value, intent(in)        :: i2
         integer(C_LONG), value, intent(in)         :: i3
         integer(C_LONG_LONG), value, intent(in)    :: i4
         integer(C_SIGNED_CHAR), value, intent(in)  :: i5
      end subroutine
   end interface

   procedure(intinterface1), bind(c), pointer :: p1, p2
   p1 => printinteger

   if ( .not. associated ( p1 ) ) error stop 1_4

   call p1(101_C_INT, 102_C_SHORT, 103_C_LONG, 104_C_LONG_LONG, 105_C_SIGNED_CHAR )

   p2 => printintegerplus100

   call p2(101_C_INT, 102_C_SHORT, 103_C_LONG, 104_C_LONG_LONG, 105_C_SIGNED_CHAR )

   if ( associated( p1, p2 ) ) error stop 2_4

   p2 => p1

   call p2(101_C_INT, 102_C_SHORT, 103_C_LONG, 104_C_LONG_LONG, 105_C_SIGNED_CHAR )

end
