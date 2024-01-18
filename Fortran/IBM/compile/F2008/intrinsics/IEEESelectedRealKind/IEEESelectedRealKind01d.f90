!***********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : IEEESelectedRealKind01d.f
!*  TEST CASE TITLE            :
!*
!*
!*  PROGRAMMER                 : Jin Li
!*  DATE                       : 10/20/2010
!*  ORIGIN                     : XL Fortran Compiler Development, IBM Torolab
!*
!*  PRIMARY FUNCTIONS TESTED   : Expected error message should be emitted when
!*                               no parameter is given.
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
!*                               message is generated when no parameter is
!*                               passed into the function IEEE_selected_real_kind()
!*
!*
!*
!234567890123456789012345678901234567890123456789012345678901234567890

        program IEEESelectedRealKind01d
                use, intrinsic :: ieee_arithmetic

                IEEE_selected_real_kind()

        end program


