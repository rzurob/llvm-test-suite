!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Alberto Alvarez-Mesquida
!*  DATE                       : 02/20/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : Testing SIGN specifier with the WRITE stmt with
!*  format declarations for F edit descriptor, and testing format SIGN selectors
!*  (SS,SP,S), printing to the console. Using complex variables.
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program iosign099

    real plus_inf, plus_nanq, nans

    data plus_inf /z'7f800000'/
    data plus_nanq /z'7fc00000'/
    data nans /z'7f800001'/

    print *, "\nTest 1\n"

13  format(A5,X,SP,F5.2)

    write (*,13) "Inf=", plus_inf
    write (*,13) "NANQ=", plus_nanq
    write (*,13) "NANS=", nans

    print *, "\nTest 2\n"

    write (*,*,sign='plus') "Inf=", plus_inf
    write (*,*,sign='plus') "NANQ=", plus_nanq
    write (*,*,sign='plus') "NANS=", nans

end program iosign099

