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
!*  DESCRIPTION                : Ensure that basic funcationailty works
!*                               for instrinsic in init expression, with various
!*                               other intrinsics passed as argument.
!***********************************************************************

	program iostatinit

	  implicit none
	  ! tests some intrinsics from various categories
	  real,parameter:: a=0.8,b=1.3,c=8738.4,d=-1.6,e=-2.0,f=-4.88
	  character(5),parameter:: char1='test '
	  character,parameter:: char2='a',char3='b'
	  integer,parameter::i1=4,iarr1(2)=(/-2,-4/),                  &
	  &  iarr2(2,2)=reshape((/2,0,-1,1/),(/2,2/))
	  real, parameter :: pi = acos(-1.0), f2=2.76**4, f3=10.2

	  !numeric
	  logical :: log1=is_iostat_eor(int(f))
	  logical :: log2=is_iostat_eor(int(e))
	  logical :: log3=is_iostat_eor(int(aint(f)))
	  logical :: log4=is_iostat_eor(int(aint(d)))

	  !math
	  logical :: log5=is_iostat_eor(int(sin(3*pi/2))*4)
	  logical :: log6=is_iostat_eor(int(sin(pi)))
	  logical :: log7=is_iostat_eor(-int(log(f2)))
	  logical :: log8=is_iostat_eor(int(log(b)))

	  !character
	  logical :: log9=is_iostat_eor(-len(trim(char1)))
	  logical :: log10=is_iostat_eor(2-len(trim(char1)))
	  logical :: log11=is_iostat_eor(-len(max(char2,char3))*4)
	  logical :: log12=is_iostat_eor(len(max(char2,char3)))

	  !numeric
	  logical :: log13=is_iostat_eor(20-digits(c))
	  logical :: log14=is_iostat_eor(digits(c))
	  logical :: log15=is_iostat_eor(range(a)-41)
	  logical :: log16=is_iostat_eor(range(a))

	  !array inq
	  logical :: log17=is_iostat_eor(-size(iarr1)*2)
	  logical :: log18=is_iostat_eor(size(iarr1))

	  !bit
	  logical :: log19=is_iostat_eor(not(i1)+1)
	  logical :: log20=is_iostat_eor(not(i1))

	  !fp
	  logical :: log21=is_iostat_eor(-exponent(f3))
	  logical :: log22=is_iostat_eor(exponent(c))

	  !vecmat mult
	  logical :: log23(2)=is_iostat_eor(matmul(iarr1,iarr2))

	  !arr const
	  logical :: log24(2)=is_iostat_eor(cshift(iarr1,1))

	  !arr location
	  logical :: log25(2)=is_iostat_eor(-maxloc(iarr2,1)*2)

	  write(*,*) log1, log2, log3, log4, log5, log6, log7,    &
	  &           log8, log9, log10, log11, log12, log13,     &
	  &           log14, log15, log16, log17, log18, log19,   &
	  &           log20, log21, log22, log23, log24, log25

	end program iostatinit
