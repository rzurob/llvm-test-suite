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
! %POSTCMD: dcomp fxi3ed11.f
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
!*  PRIMARY FUNCTIONS TESTED   : IEEE_NEXT_AFTER
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
        program fxi3ed11
      
        use ieee_arithmetic
		
        type(ieee_class_type) :: result
        integer :: i
        real :: x, y

        y = ieee_next_after(x,i)
		y = ieee_next_after()
		y = ieee_next_after(x)
		y = ieee_next_after(i,i)
		y = ieee_next_after(x, y, i)
		
        end 
