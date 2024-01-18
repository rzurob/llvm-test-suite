!#######################################################################
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  PROGRAMMER                 : William Zhang 
!*  DATE                       : 30/05/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : INTENT, VOLATILE
!*
!*  DESCRIPTION                : diagnostic TC for  C526
!*
!*   C526: if the volatile attribute is specified, the PARAMETER, EXTERNAL
!*         ,INTRINSIC OR INTENT(IN) shall not be specified.
!* ===================================================================

  program volatileC526Intrinsic 

     intrinsic sin 

     VOLATILE sin 

  end program volatileC526Intrinsic 

