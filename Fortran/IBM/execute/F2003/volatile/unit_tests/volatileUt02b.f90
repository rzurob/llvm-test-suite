!*  ===================================================================
!*
!*  TEST CASE NAME             : volatileUt02b.f
!*
!*  DATE                       : May 26, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Supporting VOLATILE for F2003
!*
!*  KEYWORD(S)                 : VOLATILE
!*
!*  DESCRIPTION                : Volatile attribute on an internally
!*                               defined entity with the same name as
!*                               entity defined in enclosing scope
!*
!234567890123456789012345678901234567890123456789012345678901234567890


program boo
   integer moo
   moo = 5
   CONTAINS
   subroutine foo
      integer x
      integer, volatile :: moo
      moo = 3
      if (moo .ne. 3) error stop 2_2
   end subroutine foo
end
