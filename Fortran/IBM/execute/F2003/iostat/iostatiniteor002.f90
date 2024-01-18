! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: iostatiniteor002.f
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
!*  DESCRIPTION                : Ensure that basic functionailty works for instrinsic in init expression, with named constants
	program iostatinit
		use iso_c_binding
		!c_signed_char = int 1
		!c_short = int 2
		!c_int = int 4

	  !singletons
		logical :: log1=is_iostat_eor(c_signed_char)
		logical :: log2=is_iostat_eor(-c_signed_char)
		logical :: log3=is_iostat_eor(c_short)
		logical :: log4=is_iostat_eor(-c_short)
		logical :: log5=is_iostat_eor(c_int)
		logical :: log6=is_iostat_eor(-c_int)

		!arrays

		logical :: loga1(3)=is_iostat_eor((/c_int,c_signed_char,c_int/))
		logical ::    loga1a(3)=    is_iostat_eor((/c_int,c_signed_char,c_short/))

		logical :: loga2(2)=is_iostat_eor((/-c_signed_char,-c_short/))
		logical :: loga3(4)=is_iostat_eor((/873686,-c_short,0,-c_int/))

		write (6,*) log1
		write (6,*) log2
		write (6,*) log3
		write (6,*) log4
		write (6,*) log5
		write (6,*) log6

		write (6,*) loga1
		write (6,*) loga1a
		write (6,*) loga2
		write (6,*) loga3

	end program iostatinit