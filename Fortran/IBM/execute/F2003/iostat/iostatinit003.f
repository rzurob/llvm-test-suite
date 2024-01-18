! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: iostatinit003.f
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
!*  PRIMARY FUNCTIONS TESTED   : is_iostat_end
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Ensure that basic funcationailty works for instrinsic in init expression, with value
	program iostatinit
	  !singletons
	  integer,parameter:: a=0,b=1,c=873686,d=-1,e=-2,f=-4,g=-187498
		logical :: log1=is_iostat_end(a)
		logical :: log2=is_iostat_end(b)
		logical :: log3=is_iostat_end(c)
		logical :: log4=is_iostat_end(d)
		logical :: log5=is_iostat_end(e)
		logical :: log6=is_iostat_end(f)
		logical :: log7=is_iostat_end(g)

		!arrays

		logical :: loga1(3)=is_iostat_end((/a,b,f/))
		logical :: loga2(2)=is_iostat_end((/d,e/))
		logical :: loga3(4)=is_iostat_end((/c,e,a,d/))
		logical :: loga4(2)=is_iostat_end((/d,d/))
		logical :: loga5(3)=is_iostat_end((/e,d,e/))
		logical :: loga6(4)=is_iostat_end((/f,d,f,e/))
		logical :: loga7(2)=is_iostat_end((/g,a/))

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