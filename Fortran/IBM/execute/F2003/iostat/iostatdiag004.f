!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan 9, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : is_iostat_eor is_iostat_end
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  : -qdebug=intmsg
!*
!*  DESCRIPTION                : Ensure that langlvl 2003std,extended,2003pure work and other langlvls do not

	implicit none
	integer ::i=-1
	logical ::l
	l=is_iostat_end(i)
	l=is_iostat_eor(i)
	end

