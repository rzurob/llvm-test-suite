! *********************************************************************
!*
!*  ===================================================================
!*
!*  DIAGNOSES TESTED           : The name of a common block cannot be
!*                               the same as a nonintrinsic module.
!*
!*  REQUIRED COMPILER OPTIONS  : -c
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

       use, non_intrinsic ::  ieee_exceptions
       common /ieee_exceptions/ i, j
       end
