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
! %POSTCMD: dcomp fxi3ed31.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 6, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_UNORDERED
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
        program fxi3ed31

        use ieee_arithmetic

        type(ieee_class_type) :: result
        integer :: i
        real :: x, y
		logical :: yn

        yn = ieee_unordered(x,i)
		yn = ieee_unordered()
		yn = ieee_unordered(x)
		yn = ieee_unordered(result)
		yn = ieee_unordered(i,i)
		yn = ieee_unordered(x, y, i)

        end
