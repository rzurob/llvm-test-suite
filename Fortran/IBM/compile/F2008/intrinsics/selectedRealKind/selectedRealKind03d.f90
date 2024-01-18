!***********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : selectedRealKind03d.f
!*  TEST CASE TITLE            :
!*
!*
!*  PROGRAMMER                 : Jin Li
!*  DATE                       : 10/20/2010
!*  ORIGIN                     : XL Fortran Compiler Development, IBM Torolab
!*
!*  PRIMARY FUNCTIONS TESTED   : Expected error message should be emitted when
!*                               input for RADIX is not supported
!*                               
!*  SECONDARY FUNCTIONS TESTED : see below
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  :
!*
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : The testcase is testing the if the proper error
!*                               message is generated when input for RADIX is
!*                               not supported
!*
!*
!*
!234567890123456789012345678901234567890123456789012345678901234567890

        program selectedRealKind01d
                integer iarray(3)

                !-- non-integer value for radix parameter
                selected_real_kind(P=2, R=2, RADIX=1.333)

                !-- value of array for radix parameter
                iarray = (/ 1, 2, 3 /)
                selected_real_kind(P=2, R=2, RADIX=iarray)

        end program


