! *********************************************************************
!*
!*  ===================================================================
!*
!*  DIAGNOSES TESTED           : Cannot access an intrinsic module and
!*                               a nonintrinsic module with the same
!*                               name in the same scope unit.
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

         use  ieee_exceptions
         use, intrinsic :: ieee_exceptions
       end
