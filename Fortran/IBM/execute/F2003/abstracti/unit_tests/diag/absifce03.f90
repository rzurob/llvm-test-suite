!*  ===================================================================
!*
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Diagnostic test
!*
!*  REQUIRED COMPILER OPTIONS  : -qlanglvl=95std
!*
!*  DESCRIPTION                : F95 and before standards doesn't support
!*                               anstract interface
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
! compile with ?qlanglvl=77std/90std/95std issue the error message

abstract interface
end interface
end

