!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : volatileUt05.f
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
!*  DESCRIPTION                : Volatile statement on a use-associated
!*                               entity
!*
!234567890123456789012345678901234567890123456789012345678901234567890


module foo
   integer, public :: fooint
end module foo

program boo
   use foo
   VOLATILE fooint
   fooint = 5
end
