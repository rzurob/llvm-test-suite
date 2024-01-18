! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS:
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: dcomp fxi3ed28.f
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : F2K IEEE Modules
!*
!*  PROGRAMMER                 : Marcus Yu 
!*  DATE                       : March 14, 2002
!*  ORIGIN                     : XL Fortran Development
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_SUPPORT_ROUNDING
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf90
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : data type not supported
!*
!*
!234567890123456789012345678901234567890123456789012345678901234567890
        program fxi3ed28
      
        use ieee_exceptions
		use ieee_arithmetic
		
        integer :: i
        real :: x
	    type(ieee_round_type) :: r_type
		type(ieee_flag_type) :: f_type
		logical :: yn 
		
        yn = ieee_support_rounding()
		yn = ieee_support_rounding(i)
        yn = ieee_support_rounding(x, i)
		yn = ieee_support_rounding(r_type, i)
		yn = ieee_support_rounding(x)
		yn = ieee_support_rounding(i,f_type)
		yn = ieee_support_rounding(f_type, x)
		
        end 
