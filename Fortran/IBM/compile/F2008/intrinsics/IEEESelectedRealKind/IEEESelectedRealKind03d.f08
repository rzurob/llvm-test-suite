!***********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/20/2010
!*
!*  PRIMARY FUNCTIONS TESTED   : Expected error message should be emitted when
!*                               input for RADIX is not supported
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
!*                               message is generated when input for RADIX is
!*                               not supported
!*
!234567890123456789012345678901234567890123456789012345678901234567890

        program IEEESelectedRealKind01d
                use, intrinsic :: ieee_arithmetic
                integer iarray(3)

                !-- non-integer value for radix parameter
                IEEE_selected_real_kind(P=2, R=2, RADIX=1.333)

                !-- value of array for radix parameter
                iarray = (/ 1, 2, 3 /)
                IEEE_selected_real_kind(P=2, R=2, RADIX=iarray)

        end program

