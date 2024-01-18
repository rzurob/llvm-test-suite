! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: iostatspecend001.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan 20, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : is_iostat_eor
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Ensure that basic funcationailty works for instrinsic in spec expression
	program iostatspec

	  integer :: iarr( size((/is_iostat_end(-1),is_iostat_end(-1),is_iostat_end(-1)/)) )
	  integer :: iarr2( size((/is_iostat_end(-1),is_iostat_end(-1),is_iostat_end(8),is_iostat_end(8)/)) )

	  print *, "size of array 1:" ,size(iarr)
	  print *, "size of array 2:" ,size(iarr2)

	end program iostatspec