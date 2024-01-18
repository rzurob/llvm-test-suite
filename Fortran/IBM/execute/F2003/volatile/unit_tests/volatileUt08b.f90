!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : volatileUt08b.f
!*
!*  PROGRAMMER                 : Vince Yuen
!*  DATE                       : May 26, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
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

program assume
   integer :: a1(9)

   interface
     subroutine sub1 (arg1)
      integer, volatile :: arg1 (:)
     end subroutine
   end interface

   a1 = (/ 4, 3, 5, 2, 3, 4, 1, 1, 2 /)
   call sub1(a1(1:2))

end

subroutine sub1 (arg1)
   integer, volatile :: arg1 (:)
end subroutine
