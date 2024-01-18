!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : volatileUt02a.f
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
!*  DESCRIPTION                : Volatile statement on a host-associated
!*                               entity
!*
!234567890123456789012345678901234567890123456789012345678901234567890


program boo
   integer moo
   moo = 2
   CONTAINS
   subroutine foo
      integer x
      volatile moo
      moo = 6
      if (moo .ne. 2) error stop 2_1
   end subroutine foo
end
