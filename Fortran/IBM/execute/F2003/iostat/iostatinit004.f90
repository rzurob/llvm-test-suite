! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: 
! %GROUP: iostatinit004.f
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
!*  TEST CASE TITLE            : iostatinit004 
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
!*  DESCRIPTION                : Ensure that basic funcationailty works for instrinsic in init expression, with intrinsics
	program iostatinit
	  !singletons
	  real,parameter:: a=0.8,b=1.3,c=686.4,d=-1.6,e=-2.0,f=-4.88
		logical :: log1=is_iostat_end(int(a))
		logical :: log2=is_iostat_end(int(b))
		logical :: log3=is_iostat_end(int(c))
		logical :: log4=is_iostat_end(int(d))
		logical :: log5=is_iostat_end(int(e))
		logical :: log6=is_iostat_end(int(f))
		
		!arrays
		
		logical :: loga1(3)=is_iostat_end((/int(a),int(b),int(f)/))
		logical :: loga2(2)=is_iostat_end((/int(d),int(e)/))
		logical :: loga3(4)=is_iostat_end((/int(c),int(e),int(a),int(d)/)) 
		logical :: loga4(2)=is_iostat_end((/int(d),int(d)/))
		logical :: loga5(3)=is_iostat_end((/int(e),int(d),int(e)/))
		logical :: loga6(4)=is_iostat_end((/int(f),int(d),int(f),int(e)/))
		logical :: loga7(2)=is_iostat_end((/int(f),int(f)/))
		
		write (6,*) log1
		write (6,*) log2
		write (6,*) log3
		write (6,*) log4
		write (6,*) log5
		write (6,*) log6
		
		
		write (6,*) loga1
		write (6,*) loga2
		write (6,*) loga3
		write (6,*) loga4
		write (6,*) loga5
		write (6,*) loga6
		write (6,*) loga7
	end program iostatinit