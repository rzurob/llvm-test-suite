! *********************************************************************
!*
!*  ===================================================================
!*
!*  DIAGNOSES TESTED           : The name of an internal function cannot
!*                               be the same as a module.
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

       program ieee_exceptions
         use, non_intrinsic::  ieee_exceptions
       end program ieee_exceptions
