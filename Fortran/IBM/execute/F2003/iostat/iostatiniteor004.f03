!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan 20, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : is_iostat_eor
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Ensure that basic funcationailty works for instrinsic in init expression, with intrinsics
	program iostatinit
	  !singletons
	  real,parameter:: a=0.8,b=1.3,c=873686.4,d=-1.6,e=-2.0,f=-4.88
		logical :: log1=is_iostat_eor(int(a))
		logical :: log2=is_iostat_eor(int(b))
		logical :: log3=is_iostat_eor(int(c))
		logical :: log4=is_iostat_eor(int(d))
		logical :: log5=is_iostat_eor(int(e))
		logical :: log6=is_iostat_eor(int(f))

		!arrays

		logical :: loga1(3)=is_iostat_eor((/int(a),int(b),int(f)/))
		logical :: loga2(2)=is_iostat_eor((/int(d),int(e)/))
		logical :: loga3(4)=is_iostat_eor((/int(c),int(e),int(a),int(d)/))
		logical :: loga4(2)=is_iostat_eor((/int(d),int(d)/))
		logical :: loga5(3)=is_iostat_eor((/int(e),int(d),int(e)/))
		logical :: loga6(4)=is_iostat_eor((/int(f),int(d),int(f),int(e)/))
		logical :: loga7(2)=is_iostat_eor((/int(f),int(f)/))

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