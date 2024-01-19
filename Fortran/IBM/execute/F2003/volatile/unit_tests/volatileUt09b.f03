!*  ===================================================================
!*
!*  DATE                       : May 26, 2006
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

program point
   integer, target :: t_a1(5), t_a2(3)
   integer, pointer:: a1(:), a2(:)

   interface
     subroutine sub1 (arg1)
      integer, volatile, pointer :: arg1 (:)
     end subroutine
   end interface

   a1 => t_a1
   a2 => t_a2

   a1 = (/ 5, 4, 7, 2, 3 /)
   call sub1 (a1)
end

subroutine sub1 (arg1)
   integer, volatile :: arg1 (9)
end subroutine
