! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: 
! %GROUP: iostatinit001a.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : iostatinit001a 
!*
!*  PROGRAMMER                 : Rob Wheeler
!*  DATE                       : Jan 20, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : is_iostat_end 
!*  SECONDARY FUNCTIONS TESTED : None 
!*
!*  DRIVER STANZA              : xlf
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  DESCRIPTION                : Ensure that basic funcationailty works for instrinsic in init expression, with value
	program iostatinit
	  !singletons
		logical :: log1=is_iostat_end(0)
		logical :: log2=is_iostat_end(1)
		logical :: log3=is_iostat_end(4000)
		logical :: log4=is_iostat_end(-1)
		logical :: log5=is_iostat_end(-2)
		logical :: log6=is_iostat_end(-4)
		logical :: log7=is_iostat_end(-5000)
		
		!arrays
		
		logical :: loga1(3)=is_iostat_end((/0,1,-4/))
		logical :: loga2(2)=is_iostat_end((/-1,-2/))
		logical :: loga3(4)=is_iostat_end((/4000,-2,0,-1/))
		logical :: loga4(2)=is_iostat_end((/-1,-1/))
		logical :: loga5(3)=is_iostat_end((/-2,-1,-2/))
		logical :: loga6(4)=is_iostat_end((/-4,-1,-4,-2/))
		logical :: loga7(2)=is_iostat_end((/-5000,0/))
		
		write (6,*) log1
		write (6,*) log2
		write (6,*) log3
		write (6,*) log4
		write (6,*) log5
		write (6,*) log6
		write (6,*) log7
		
		write (6,*) loga1
		write (6,*) loga2
		write (6,*) loga3
		write (6,*) loga4
		write (6,*) loga5
		write (6,*) loga6
		write (6,*) loga7
	end program iostatinit