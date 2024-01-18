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
! %POSTCMD: dcomp fxi3ed12.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 6, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_REM
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : data type not supported
!*
!234567890123456789012345678901234567890123456789012345678901234567890
        program fxi3ed12

        use ieee_arithmetic

        type(ieee_class_type) :: result
        integer :: i
        real :: x, y

        y = ieee_copy_sign(x,i)
		y = ieee_copy_sign()
		y = ieee_copy_sign(x)
		y = ieee_copy_sign(i,i)
		y = ieee_copy_sign(x, y, i)

        end
