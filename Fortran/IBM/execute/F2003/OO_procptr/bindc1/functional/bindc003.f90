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
!*                                        Procedure Pointer pointing at C functions with BIND(C) function interface, pass by reference
!*                                        and inside C function will again call FORTRAN subprograms (pass by value and pass by reference)
!*                                        - dummy argument and function result with INTEGERS (C_INTMAX_T, C_INTPTR_T, C_INTLEAST8_T, C_INT_FAST16_T, C_INT_FAST64_T)
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
      integer(C_INTMAX_T) function getproduct(i1, i2, i3, i4, i5) BIND(C)
         import C_INTMAX_T, C_INTPTR_T, C_INT_LEAST8_T, C_INT_FAST16_T, C_INT_FAST64_T
         integer(C_INTMAX_T), intent(in), value        :: i1
         integer(C_INTPTR_T), intent(in), value        :: i2
         integer(C_INT_LEAST8_T), intent(in), value    :: i3
         integer(C_INT_FAST16_T), intent(in), value    :: i4
         integer(C_INT_FAST64_T), intent(in), value    :: i5
     end function
   end interface

   interface
      integer(C_INTMAX_T) function getsum(i1, i2, i3, i4, i5) BIND(C)
         import C_INTMAX_T, C_INTPTR_T, C_INT_LEAST8_T, C_INT_FAST16_T, C_INT_FAST64_T
         integer(C_INTMAX_T), intent(in), value        :: i1
         integer(C_INTPTR_T), intent(in), value        :: i2
         integer(C_INT_LEAST8_T), intent(in), value    :: i3
         integer(C_INT_FAST16_T), intent(in), value    :: i4
         integer(C_INT_FAST64_T), intent(in), value    :: i5
      end function
   end interface

   interface
      function intinterface1(i1, i2, i3, i4, i5) BIND(C)
         import C_INTMAX_T, C_INTPTR_T, C_INT_LEAST8_T, C_INT_FAST16_T, C_INT_FAST64_T
         integer(C_INTMAX_T) :: intinterface1
         integer(C_INTMAX_T), intent(in), value        :: i1
         integer(C_INTPTR_T), intent(in), value        :: i2
         integer(C_INT_LEAST8_T), intent(in), value    :: i3
         integer(C_INT_FAST16_T), intent(in), value    :: i4
         integer(C_INT_FAST64_T), intent(in), value    :: i5
     end function
   end interface

   contains

   integer(C_INTMAX_T) function multiply(i1, i2, i3, i4, i5) BIND(C)
      integer(C_INTMAX_T), intent(in)        :: i1
      integer(C_INTPTR_T), intent(in)        :: i2
      integer(C_INT_LEAST8_T), intent(in)    :: i3
      integer(C_INT_FAST16_T), intent(in)    :: i4
      integer(C_INT_FAST64_T), intent(in)    :: i5

      multiply = i1 * i2 * i3 * i4 * i5

   end function
   
   integer(C_INTMAX_T) function add(i1, i2, i3, i4, i5) BIND(C)
      integer(C_INTMAX_T), intent(in), value        :: i1
      integer(C_INTPTR_T), intent(in), value        :: i2
      integer(C_INT_LEAST8_T), intent(in), value    :: i3
      integer(C_INT_FAST16_T), intent(in), value    :: i4
      integer(C_INT_FAST64_T), intent(in), value    :: i5

      add = i1 + i2 + i3 + i4 + i5

   end function

end module

   use cinterfaces

   procedure(intinterface1), pointer, bind(C) :: p1

   integer(C_INTMAX_T)        :: i1 = 12
   integer(C_INTPTR_T)        :: i2 = 13
   integer(C_INT_LEAST8_T)    :: i3 = 14
   integer(C_INT_FAST16_T)    :: i4 = 15
   integer(C_INT_FAST64_T)    :: i5 = 16

   integer(C_INTMAX_T)   :: pdt, sum

   p1 => getproduct

   pdt = p1 ( i1, i2, i3, i4, i5 )

   if ( pdt /= 524160 ) error stop 1_4

   p1 => getsum

   sum = p1 ( i5 = i5, i4 = i4, i3 = i3, i2 = i2, i1 = i1 )
   if ( sum /= 70 )     error stop 2_4

   if ( .not. associated(p1) ) error stop 3_4
   nullify (p1)
   if ( associated(p1) )       error stop 4_4

end
