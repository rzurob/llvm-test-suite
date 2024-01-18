!*  ===================================================================
!*
!*  TEST CASE NAME             : volatileUt07.f
!*
!*  DATE                       : May 26, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Supporting VOLATILE for F2003
!*
!*  KEYWORD(S)                 : VOLATILE
!*
!*  DESCRIPTION                : C526
!*
!* C526 (R501) If the VOLATILE attribute is specified, the PARAMETER,
!* INTRINSIC, EXTERNAL, or INTENT(IN) attribute shall not be specified.
!*
!234567890123456789012345678901234567890123456789012345678901234567890


subroutine sub(i)
    integer, volatile, intent(in) :: i
end subroutine

end
