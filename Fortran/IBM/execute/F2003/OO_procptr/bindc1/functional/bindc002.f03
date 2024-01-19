!*  ===================================================================
!*
!*  DATE                       : 06/07/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Procedure Pointer with BIND(C) feature
!*                                        Procedure Pointer pointing at C functions with BIND(C) function interface, pass by reference
!*                                        - dummy argument and function result with INTEGERS (C_SIZE_T, C_INT8_T, C_INT16_T, C_INT32_T, C_INT64_T)
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
      integer(C_INT64_T) function getsumplus1(i1, i2, i3, i4, i5) BIND(C)
         import C_SIZE_T, C_INT8_T, C_INT16_T, C_INT32_T, C_INT64_T
         integer(C_SIZE_T), intent(inout)     :: i1
         integer(C_INT8_T), intent(inout)     :: i2
         integer(C_INT16_T), intent(inout)    :: i3
         integer(C_INT32_T), intent(inout)    :: i4
         integer(C_INT64_T), intent(inout)    :: i5
     end function
   end interface

   interface
      integer(C_INT64_T) function getsumminus1(i1, i2, i3, i4, i5) BIND(C)
         import C_SIZE_T, C_INT8_T, C_INT16_T, C_INT32_T, C_INT64_T
         integer(C_SIZE_T), intent(inout)     :: i1
         integer(C_INT8_T), intent(inout)     :: i2
         integer(C_INT16_T), intent(inout)    :: i3
         integer(C_INT32_T), intent(inout)    :: i4
         integer(C_INT64_T), intent(inout)    :: i5
     end function
   end interface

end module

   use cinterfaces

   interface
      function intinterface1(i1, i2, i3, i4, i5) BIND(C)
         import C_SIZE_T, C_INT8_T, C_INT16_T, C_INT32_T, C_INT64_T
         integer(C_INT64_T)                :: intinterface1
         integer(C_SIZE_T), intent(inout)     :: i1
         integer(C_INT8_T), intent(inout)     :: i2
         integer(C_INT16_T), intent(inout)    :: i3
         integer(C_INT32_T), intent(inout)    :: i4
         integer(C_INT64_T), intent(inout)    :: i5
      end function
   end interface

   procedure(intinterface1), pointer, BIND(C) :: p1, p2

   integer(kind=C_SIZE_T)    :: ii1 = 11
   integer(C_INT8_T)    :: ii2 = 12
   integer(C_INT16_T)   :: ii3 = 13
   integer(C_INT32_T)   :: ii4 = 14
   integer(kind=C_INT64_T)   :: ii5 = 15
   integer(C_INT64_T)   :: sum = 0

   p1 => getsumplus1

   sum = p1( ii1, ii2, ii3, ii4, ii5 )

   if ( ( ii1 /= 12 ) .or. ( ii2 /= 13 ) .or. ( ii3 /= 14 ) .or.  ( ii4 /= 15 ) .or. ( ii5 /= 16 ) .or. ( sum /= 65 ) ) error stop 1_4

   p2 => getsumminus1

   sum = p2( ii1, ii2, ii3, ii4, ii5 )

   if ( ( ii1 /= 11 ) .or. ( ii2 /= 12 ) .or. ( ii3 /= 13 ) .or.  ( ii4 /= 14 ) .or. ( ii5 /= 15 ) .or. ( sum /= 70 ) ) error stop 2_4

end
