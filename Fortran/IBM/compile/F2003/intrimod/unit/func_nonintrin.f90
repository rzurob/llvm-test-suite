! *********************************************************************
!*
!*  ===================================================================
!*
!*  DIAGNOSES TESTED           : The name of a function cannot be
!*                               the same as a nonintrinsic module.
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

       integer function ieee_exceptions()
         use, non_intrinsic ::  ieee_exceptions
         ieee_exceptions = 5
       end function ieee_exceptions
