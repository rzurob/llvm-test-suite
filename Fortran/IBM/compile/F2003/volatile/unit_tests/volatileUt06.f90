!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : volatileUt06.f
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
!*  DESCRIPTION                : 12.4.1.2
!*
!* If the actual argument is an array section having a vector subscript, 
!* the dummy argument is not definable and shall not have the INTENT (OUT), 
!* INTENT (INOUT), VOLATILE, or ASYNCHRONOUS attributes.
!*
!234567890123456789012345678901234567890123456789012345678901234567890


program vec
   integer a1(9)
   interface
     subroutine sub1 (arg1)
      integer, volatile :: arg1 (9)
     end subroutine
   end interface

   call sub1(a1((/5,1,4,6,6,7,9,8,1/)))

end

subroutine sub1 (arg1)
   integer, volatile :: arg1 (9)
end subroutine
