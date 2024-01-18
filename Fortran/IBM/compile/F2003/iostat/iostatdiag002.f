!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: diag1.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan 9, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : is_iostat_eor iostat_end
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  : -qdebug=intmsg
!*
!*  DESCRIPTION                : Ensure that only one integer can be passed to is_iostat_end io_stat_eor

	implicit none

	integer :: i=-1,j=-2,k=-3,l=-4,m=0,n=5,iarr(3)=(/1,-2,0/)
	logical :: result

	result = is_iostat_end(i,j)
	result = is_iostat_end(i,k)
	result = is_iostat_end(i,iarr)
	result = is_iostat_end(i,i,i)

	result = is_iostat_eor(i,j)
	result = is_iostat_eor(i,k)
	result = is_iostat_eor(i,iarr)
	result = is_iostat_eor(i,i,i)

	end

