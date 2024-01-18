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
! %POSTCMD: dcomp fxi3ed08.f
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : F2K IEEE Modules
!*
!*  PROGRAMMER                 : Marcus Yu 
!*  DATE                       : March 13, 2002
!*  ORIGIN                     : XL Fortran Development
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_IS_NEGATIVE
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
        program fxi3ed08
      
        use ieee_arithmetic
		
        integer :: i
        real :: x
	    type(ieee_round_type) :: r_type
		logical :: yn 
				
        yn = ieee_is_negative()
        yn = ieee_is_negative(i)
        yn = ieee_is_negative(r_type, i)
		yn = ieee_is_negative(x, i)
		
        end 
