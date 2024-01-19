! *********************************************************************
!*
!*  ===================================================================
!*
!*  DIAGNOSES TESTED           : The name of a subroutine can be
!*                               the same as an intrinsic module.
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

       Module ieee_exceptions
         integer aa
       end module

       subroutine ieee_exceptions
         use, intrinsic ::  ieee_exceptions
       end subroutine ieee_exceptions
