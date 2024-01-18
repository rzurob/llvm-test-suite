!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : Abstractr Interface
!*
!*  PROGRAMMER                 : James Ren
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Diagnostic test
!*
!*  DRIVER STANZA              : xlf90/95
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : The procedure name in the ABSTRACT
!*                               INTERFACE block must not be the name 
!*                               of an intrinsic type
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
abstract interface
 function integer()  ! Error: intrinsic name is illegal here
 end function
end interface
end

