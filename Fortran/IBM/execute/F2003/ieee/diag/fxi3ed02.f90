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
! %POSTCMD: dcomp fxi3ed02.f
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : F2K IEEE Modules
!*
!*  PROGRAMMER                 : Marcus Yu 
!*  DATE                       : March 6, 2002
!*  ORIGIN                     : XL Fortran Development
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : IEEE_COPY_SIGN
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
        program fxi3ed02
      
        use ieee_arithmetic
		
        type(ieee_class_type) :: result
        integer :: i
        real :: x, y

        x = 8.0
        i = 1
        y = ieee_copy_sign(x,i)
		y = ieee_copy_sign()
		y = ieee_copy_sign(x)
		y = ieee_copy_sign(i,i)
		
        end 
