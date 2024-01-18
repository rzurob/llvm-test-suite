!***********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : IEEESelectedRealKind02d.f
!*
!*  DATE                       : 10/20/2010
!*
!*  PRIMARY FUNCTIONS TESTED   : Expected error message should be emitted when
!*                               extra parameter is passed into it.
!*
!*  SECONDARY FUNCTIONS TESTED : see below
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : The testcase is testing the if the proper error
!*                               message is generated when extra parameter is
!*                               passed into the function IEEE_selected_real_kind()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

        program IEEESelectedRealKind01d
                use, intrinsic :: ieee_arithmetic

                IEEE_selected_real_kind(P=2, R=2, RADIX=2, BLAH=10);

        end program


