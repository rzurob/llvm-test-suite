!*  ===================================================================
!*
!*  TEST CASE NAME             : volatileUt08a.f
!*
!*  DATE                       : May 26, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Supporting VOLATILE for F2003
!*
!*  KEYWORD(S)                 : VOLATILE
!*
!*  DESCRIPTION                : C1232
!*
!* (C1232) If an actual argument is an array section or an
!* assumed-shape array, and the corresponding dummy argument has either
!* the VOLATILE or ASYNCHRONOUS attribute, that dummy argument shall be
!* an assumed-shape array.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program sec
   integer :: a1(9), a2(9,8)

   interface
     subroutine sub1 (arg1)
      integer, volatile :: arg1 (9)
     end subroutine
   end interface

   a1 = (/ 1, 3, 5, 2, 9, 2, 1, 3, 6 /)
   a2 (:, 1) = a1
   call sub1(a1(:))
end

subroutine sub1 (arg1)
   integer, volatile :: arg1 (9)
end subroutine
