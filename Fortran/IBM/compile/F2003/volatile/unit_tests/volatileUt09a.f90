!*  ===================================================================
!*
!*  TEST CASE NAME             : volatileUt09a.f
!*
!*  DATE                       : May 26, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Supporting VOLATILE for F2003
!*
!*  KEYWORD(S)                 : VOLATILE
!*
!*  DESCRIPTION                : C1233
!*
!* (C1233) If an actual argument is a pointer array, and the
!*  corresponding dummy argument has either the VOLATILE or ASYNCHRONOUS
!*  attribute, that dummy argument shall be an assumed-shape array
!*  or a pointer array.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program non
   integer, target :: t_a1(5), t_a2(9)
   integer, pointer:: a1(:), a2(:)

   interface
     subroutine sub1 (arg1)
      integer, volatile :: arg1 (5)
     end subroutine
   end interface

   a1 => t_a1
   a2 => t_a2

   a1 = (/ 1, 3, 9, 2, 7 /)
   call sub1 (a1)
end

subroutine sub1 (arg1)
   integer, volatile :: arg1 (5)
end subroutine
