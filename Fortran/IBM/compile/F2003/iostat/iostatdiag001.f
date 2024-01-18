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
!*  DESCRIPTION                : Ensure that only integers can be passed to is_iostat_end io_stat_eor

	implicit none
	real*4 :: r4=-2.0, rarr4(2)=(/1.0,-1.0/)
	real*8 :: r8=4.0, rarr8(2)=(/1.0,-1.0/)

	character*1 :: c1='a' ,carr1(3)=(/'a','b','c'/)
	character*2 :: c2='b' ,carr2(3)=(/'a','b','c'/)
	character*4 :: c4='c' ,carr4(3)=(/'a','b','c'/)
	character*8 :: c8='d' ,carr8(3)=(/'a','b','c'/)

	double precision:: d1=9.99, darr1(4)=(/1.0,-2.0,1245.4214,0.0/)

	logical*1 :: l1=.true., larr1(2)=(/.true.,.false./)
	logical*2 :: l2=.false., larr2(2)=(/.true.,.false./)
	logical*4 :: l4=.true., larr4(2)=(/.true.,.false./)
	logical*8 :: l8=.false., larr8(2)=(/.true.,.false./)

	complex*8 :: cx8=(2.3,-4.5),cxarr8(2)=(/(2.3,-4.5),(2.3,-4.5)/)
	complex*16 :: cx16=(2.3,-4.5),cxarr16(2)=(/(2.3,-4.5),(2.3,-4.5)/)

	type dtype1
		integer :: i
		real :: r
	end type

	type dtype2
		integer :: i
	end type
	logical :: junk,junk2(2),junk3(3),junk4(4)
	type(dtype1) :: dt1
	type(dtype2) :: dt2

	dt1%i=2
	dt1%r=3.0
	dt2%i=-1

	! check that types other than int are not accepted
	junk=is_iostat_end(r4)
	junk=is_iostat_eor(r4)
	junk=is_iostat_end(r8)
	junk=is_iostat_eor(r8)
	junk=is_iostat_end(c1)
	junk=is_iostat_eor(c1)
	junk=is_iostat_end(c2)
	junk=is_iostat_eor(c2)
	junk=is_iostat_end(c4)
	junk=is_iostat_eor(c4)
	junk=is_iostat_end(c8)
	junk=is_iostat_eor(c8)
	junk=is_iostat_end(d1)
	junk=is_iostat_eor(d1)
	junk=is_iostat_end(l1)
	junk=is_iostat_eor(l1)
	junk=is_iostat_end(l2)
	junk=is_iostat_eor(l2)
	junk=is_iostat_end(l4)
	junk=is_iostat_eor(l4)
	junk=is_iostat_end(l8)
	junk=is_iostat_eor(l8)
	junk=is_iostat_end(cx8)
	junk=is_iostat_eor(cx8)
	junk=is_iostat_end(cx16)
	junk=is_iostat_eor(cx16)
	junk=is_iostat_end(dt1)
	junk=is_iostat_eor(dt2)

	!check that arrays other than ints are not accepted
	junk2=is_iostat_end(rarr4)
	junk2=is_iostat_eor(rarr4)
	junk2=is_iostat_end(rarr8)
	junk2=is_iostat_eor(rarr8)
	junk2=is_iostat_end(larr1)
	junk2=is_iostat_eor(larr1)
	junk2=is_iostat_end(larr2)
	junk2=is_iostat_eor(larr2)
	junk2=is_iostat_end(larr4)
	junk2=is_iostat_eor(larr4)
	junk2=is_iostat_end(larr8)
	junk2=is_iostat_eor(larr8)
	junk2=is_iostat_end(cxarr8)
	junk2=is_iostat_eor(cxarr8)
	junk2=is_iostat_end(cxarr16)
	junk2=is_iostat_eor(cxarr16)

	junk3=is_iostat_end(carr1)
	junk3=is_iostat_eor(carr1)
	junk3=is_iostat_end(carr2)
	junk3=is_iostat_eor(carr2)
	junk3=is_iostat_end(carr4)
	junk3=is_iostat_eor(carr4)
	junk3=is_iostat_end(carr8)
	junk3=is_iostat_eor(carr8)

	junk4=is_iostat_end(darr1)
	junk4=is_iostat_eor(darr1)

	end

