! *********************************************************************
!*
!*  ===================================================================
!*
!*  DIAGNOSES TESTED           : The name of an external function can
!*                               be the same as an intrinsic module.
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

       integer function ieee_exceptions()
         use, intrinsic ::  ieee_exceptions
         ieee_exceptions = 5
       end function ieee_exceptions
