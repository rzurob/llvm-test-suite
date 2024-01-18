!*  ===================================================================
!*
!*  DATE                       : 30/05/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : INTENT, VOLATILE
!*
!*  DESCRIPTION                : diagnostic TC for  C526
!*
!*   C526: if the volatile attribute is specified, the PARAMETER, EXTERNAL
!*         ,INTRINSIC OR INTENT(IN) shall not be specified.
!* ===================================================================

  program volatileC526External

     external cos

     VOLATILE cos

  end program volatileC526External

