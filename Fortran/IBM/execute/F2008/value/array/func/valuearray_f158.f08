!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2015-09-24
!*
!*  PRIMARY FUNCTIONS TESTED   : VALUE(F2008 extension) - dummy argument arrays allowed with value
!*
!*  DESCRIPTION                : testing the extensions to the VALUE attribute
!*                                for an array of a derived type:
!*								- the derived type includes pointer and allocatable components
!*								-passing an array to a subroutine to check that
!*								1. dummy argument is equal to the actual argument
!*								2. actual argument doesn't change
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890


implicit none

type t1
  real r1
  real, pointer :: r2
  real, pointer :: r3
  real, allocatable :: r4(:)
  real, dimension(3) :: r5
end type

integer SIZEOFA,doCounter
parameter (SIZEOFA = 10)
logical, external :: precision_r4
logical, external :: precision_x8

 real, target :: tar1, tar2
 real, pointer :: po1
 type(t1) dvt1(10),dvt1_r(10)
  tar1=5*atan(1.0)
  tar2=7*atan(1.0)
  po1=>tar2

  dvt1%r1=atan(1.0)

	do doCounter=1,SIZEOFA
		dvt1(doCounter)%r1=atan(1.0)
		dvt1(doCounter)%r2=>tar1
		dvt1(doCounter)%r3=>po1
		allocate(dvt1(doCounter)%r4(2))
		dvt1(doCounter)%r4=(/atan(1.0),2*atan(1.0)/)
		dvt1(doCounter)%r5=(/2*atan(1.0),3*atan(1.0),5*atan(1.0)/)

		dvt1_r(doCounter)%r1=atan(1.0)
		dvt1_r(doCounter)%r2=>tar1
    	dvt1_r(doCounter)%r3=>po1
		allocate(dvt1_r(doCounter)%r4(2))
		dvt1_r(doCounter)%r4=(/atan(1.0),2*atan(1.0)/)
		dvt1_r(doCounter)%r5=(/2*atan(1.0),3*atan(1.0),5*atan(1.0)/)
	end do

call sub11(dvt1,10)
do doCounter=1,SIZEOFA
		if (.not. precision_r4 (dvt1(doCounter)%r1		,dvt1_r(doCounter)%r1		))	error stop 1101
		if (.not. precision_r4 (dvt1(doCounter)%r2		,dvt1_r(doCounter)%r2		))	error stop 1102
		if (.not. precision_r4 (dvt1(doCounter)%r3		,dvt1_r(doCounter)%r3		))	error stop 1103
		if (.not. allocated(dvt1(doCounter)%r4) )										error stop 1110
		if (.not. precision_r4 (dvt1(doCounter)%r4(1)	,dvt1_r(doCounter)%r4(1)	))	error stop 1114
		if (.not. precision_r4 (dvt1(doCounter)%r4(2)	,dvt1_r(doCounter)%r4(2)	))	error stop 1124
		if (.not. precision_r4 (dvt1(doCounter)%r5(1)	,dvt1_r(doCounter)%r5(1)	))	error stop 1115
		if (.not. precision_r4 (dvt1(doCounter)%r5(2)	,dvt1_r(doCounter)%r5(2)	))	error stop 1125
		if (.not. precision_r4 (dvt1(doCounter)%r5(3)	,dvt1_r(doCounter)%r5(3)	))	error stop 1135
end do

call sub12(dvt1)
do doCounter=1,SIZEOFA
		if (.not. precision_r4 (dvt1(doCounter)%r1		,dvt1_r(doCounter)%r1		))	error stop 1201
		if (.not. precision_r4 (dvt1(doCounter)%r2		,dvt1_r(doCounter)%r2		))	error stop 1202
		if (.not. precision_r4 (dvt1(doCounter)%r3		,dvt1_r(doCounter)%r3		))	error stop 1203
		if (.not. allocated(dvt1(doCounter)%r4) )										error stop 1210
		if (.not. precision_r4 (dvt1(doCounter)%r4(1)	,dvt1_r(doCounter)%r4(1)	))	error stop 1214
		if (.not. precision_r4 (dvt1(doCounter)%r4(2)	,dvt1_r(doCounter)%r4(2)	))	error stop 1224
		if (.not. precision_r4 (dvt1(doCounter)%r5(1)	,dvt1_r(doCounter)%r5(1)	))	error stop 1215
		if (.not. precision_r4 (dvt1(doCounter)%r5(2)	,dvt1_r(doCounter)%r5(2)	))	error stop 1225
		if (.not. precision_r4 (dvt1(doCounter)%r5(3)	,dvt1_r(doCounter)%r5(3)	))	error stop 1235
end do

call sub13(dvt1)
do doCounter=1,SIZEOFA
		if (.not. precision_r4 (dvt1(doCounter)%r1		,dvt1_r(doCounter)%r1		))	error stop 1301
		if (.not. precision_r4 (dvt1(doCounter)%r2		,dvt1_r(doCounter)%r2		))	error stop 1302
		if (.not. precision_r4 (dvt1(doCounter)%r3		,dvt1_r(doCounter)%r3		))	error stop 1303
		if (.not. allocated(dvt1(doCounter)%r4) )										error stop 1310
		if (.not. precision_r4 (dvt1(doCounter)%r4(1)	,dvt1_r(doCounter)%r4(1)	))	error stop 1314
		if (.not. precision_r4 (dvt1(doCounter)%r4(2)	,dvt1_r(doCounter)%r4(2)	))	error stop 1324
		if (.not. precision_r4 (dvt1(doCounter)%r5(1)	,dvt1_r(doCounter)%r5(1)	))	error stop 1315
		if (.not. precision_r4 (dvt1(doCounter)%r5(2)	,dvt1_r(doCounter)%r5(2)	))	error stop 1325
		if (.not. precision_r4 (dvt1(doCounter)%r5(3)	,dvt1_r(doCounter)%r5(3)	))	error stop 1335
end do

call sub14(dvt1)
do doCounter=1,SIZEOFA
		if (.not. precision_r4 (dvt1(doCounter)%r1		,dvt1_r(doCounter)%r1		))	error stop 1401
		if (.not. precision_r4 (dvt1(doCounter)%r2		,dvt1_r(doCounter)%r2		))	error stop 1402
		if (.not. precision_r4 (dvt1(doCounter)%r3		,dvt1_r(doCounter)%r3		))	error stop 1403
		if (.not. allocated(dvt1(doCounter)%r4) )										error stop 1410
		if (.not. precision_r4 (dvt1(doCounter)%r4(1)	,dvt1_r(doCounter)%r4(1)	))	error stop 1414
		if (.not. precision_r4 (dvt1(doCounter)%r4(2)	,dvt1_r(doCounter)%r4(2)	))	error stop 1424
		if (.not. precision_r4 (dvt1(doCounter)%r5(1)	,dvt1_r(doCounter)%r5(1)	))	error stop 1415
		if (.not. precision_r4 (dvt1(doCounter)%r5(2)	,dvt1_r(doCounter)%r5(2)	))	error stop 1425
		if (.not. precision_r4 (dvt1(doCounter)%r5(3)	,dvt1_r(doCounter)%r5(3)	))	error stop 1435
end do

call sub21(dvt1,10)
do doCounter=1,SIZEOFA
		if (.not. precision_r4 (dvt1(doCounter)%r1		,dvt1_r(doCounter)%r1		))	error stop 2101
		if (.not. precision_r4 (dvt1(doCounter)%r2		,dvt1_r(doCounter)%r2		))	error stop 2102
		if (.not. precision_r4 (dvt1(doCounter)%r3		,dvt1_r(doCounter)%r3		))	error stop 2103
		if (.not. allocated(dvt1(doCounter)%r4) )										error stop 2110
		if (.not. precision_r4 (dvt1(doCounter)%r4(1)	,dvt1_r(doCounter)%r4(1)	))	error stop 2114
		if (.not. precision_r4 (dvt1(doCounter)%r4(2)	,dvt1_r(doCounter)%r4(2)	))	error stop 2124
		if (.not. precision_r4 (dvt1(doCounter)%r5(1)	,dvt1_r(doCounter)%r5(1)	))	error stop 2115
		if (.not. precision_r4 (dvt1(doCounter)%r5(2)	,dvt1_r(doCounter)%r5(2)	))	error stop 2125
		if (.not. precision_r4 (dvt1(doCounter)%r5(3)	,dvt1_r(doCounter)%r5(3)	))	error stop 2135
end do

call sub22(dvt1)
do doCounter=1,SIZEOFA
		if (.not. precision_r4 (dvt1(doCounter)%r1		,dvt1_r(doCounter)%r1		))	error stop 2201
		if (.not. precision_r4 (dvt1(doCounter)%r2		,dvt1_r(doCounter)%r2		))	error stop 2202
		if (.not. precision_r4 (dvt1(doCounter)%r3		,dvt1_r(doCounter)%r3		))	error stop 2203
		if (.not. allocated(dvt1(doCounter)%r4) )										error stop 2210
		if (.not. precision_r4 (dvt1(doCounter)%r4(1)	,dvt1_r(doCounter)%r4(1)	))	error stop 2214
		if (.not. precision_r4 (dvt1(doCounter)%r4(2)	,dvt1_r(doCounter)%r4(2)	))	error stop 2224
		if (.not. precision_r4 (dvt1(doCounter)%r5(1)	,dvt1_r(doCounter)%r5(1)	))	error stop 2215
		if (.not. precision_r4 (dvt1(doCounter)%r5(2)	,dvt1_r(doCounter)%r5(2)	))	error stop 2225
		if (.not. precision_r4 (dvt1(doCounter)%r5(3)	,dvt1_r(doCounter)%r5(3)	))	error stop 2235
end do

call sub23(dvt1)
do doCounter=1,SIZEOFA
		if (.not. precision_r4 (dvt1(doCounter)%r1		,dvt1_r(doCounter)%r1		))	error stop 2301
		if (.not. precision_r4 (dvt1(doCounter)%r2		,dvt1_r(doCounter)%r2		))	error stop 2302
		if (.not. precision_r4 (dvt1(doCounter)%r3		,dvt1_r(doCounter)%r3		))	error stop 2303
		if (.not. allocated(dvt1(doCounter)%r4) )										error stop 2310
		if (.not. precision_r4 (dvt1(doCounter)%r4(1)	,dvt1_r(doCounter)%r4(1)	))	error stop 2314
		if (.not. precision_r4 (dvt1(doCounter)%r4(2)	,dvt1_r(doCounter)%r4(2)	))	error stop 2324
		if (.not. precision_r4 (dvt1(doCounter)%r5(1)	,dvt1_r(doCounter)%r5(1)	))	error stop 2315
		if (.not. precision_r4 (dvt1(doCounter)%r5(2)	,dvt1_r(doCounter)%r5(2)	))	error stop 2325
		if (.not. precision_r4 (dvt1(doCounter)%r5(3)	,dvt1_r(doCounter)%r5(3)	))	error stop 2335
end do

call sub24(dvt1)
do doCounter=1,SIZEOFA
		if (.not. precision_r4 (dvt1(doCounter)%r1		,dvt1_r(doCounter)%r1		))	error stop 2401
		if (.not. precision_r4 (dvt1(doCounter)%r2		,dvt1_r(doCounter)%r2		))	error stop 2402
		if (.not. precision_r4 (dvt1(doCounter)%r3		,dvt1_r(doCounter)%r3		))	error stop 2403
		if (.not. allocated(dvt1(doCounter)%r4) )										error stop 2410
		if (.not. precision_r4 (dvt1(doCounter)%r4(1)	,dvt1_r(doCounter)%r4(1)	))	error stop 2414
		if (.not. precision_r4 (dvt1(doCounter)%r4(2)	,dvt1_r(doCounter)%r4(2)	))	error stop 2424
		if (.not. precision_r4 (dvt1(doCounter)%r5(1)	,dvt1_r(doCounter)%r5(1)	))	error stop 2415
		if (.not. precision_r4 (dvt1(doCounter)%r5(2)	,dvt1_r(doCounter)%r5(2)	))	error stop 2425
		if (.not. precision_r4 (dvt1(doCounter)%r5(3)	,dvt1_r(doCounter)%r5(3)	))	error stop 2435
end do

call sub31(dvt1,dvt1)
do doCounter=1,SIZEOFA
		if (.not. precision_r4 (dvt1(doCounter)%r1		,dvt1_r(doCounter)%r1		))	error stop 3101
		if (.not. precision_r4 (dvt1(doCounter)%r2		,dvt1_r(doCounter)%r2		))	error stop 3102
		if (.not. precision_r4 (dvt1(doCounter)%r3		,dvt1_r(doCounter)%r3		))	error stop 3103
		if (.not. allocated(dvt1(doCounter)%r4) )										error stop 3110
		if (.not. precision_r4 (dvt1(doCounter)%r4(1)	,dvt1_r(doCounter)%r4(1)	))	error stop 3114
		if (.not. precision_r4 (dvt1(doCounter)%r4(2)	,dvt1_r(doCounter)%r4(2)	))	error stop 3124
		if (.not. precision_r4 (dvt1(doCounter)%r5(1)	,dvt1_r(doCounter)%r5(1)	))	error stop 3115
		if (.not. precision_r4 (dvt1(doCounter)%r5(2)	,dvt1_r(doCounter)%r5(2)	))	error stop 3125
		if (.not. precision_r4 (dvt1(doCounter)%r5(3)	,dvt1_r(doCounter)%r5(3)	))	error stop 3135
end do

call sub32(dvt1,dvt1)
do doCounter=1,SIZEOFA
		if (.not. precision_r4 (dvt1(doCounter)%r1		,dvt1_r(doCounter)%r1		))	error stop 3201
		if (.not. precision_r4 (dvt1(doCounter)%r2		,dvt1_r(doCounter)%r2		))	error stop 3202
		if (.not. precision_r4 (dvt1(doCounter)%r3		,dvt1_r(doCounter)%r3		))	error stop 3203
		if (.not. allocated(dvt1(doCounter)%r4) )										error stop 3210
		if (.not. precision_r4 (dvt1(doCounter)%r4(1)	,dvt1_r(doCounter)%r4(1)	))	error stop 3214
		if (.not. precision_r4 (dvt1(doCounter)%r4(2)	,dvt1_r(doCounter)%r4(2)	))	error stop 3224
		if (.not. precision_r4 (dvt1(doCounter)%r5(1)	,dvt1_r(doCounter)%r5(1)	))	error stop 3215
		if (.not. precision_r4 (dvt1(doCounter)%r5(2)	,dvt1_r(doCounter)%r5(2)	))	error stop 3225
		if (.not. precision_r4 (dvt1(doCounter)%r5(3)	,dvt1_r(doCounter)%r5(3)	))	error stop 3235
end do


contains

subroutine sub11(arg,n)
    type(t1) :: arg(n)
	integer n
	value arg
	real, target :: t1, t2
	real, pointer :: p1
	t1=11*atan(1.0)
	t2=13*atan(1.0)
	p1=>t2

	do doCounter=1,SIZEOFA
		if (.not. precision_r4 (arg(doCounter)%r1,atan(1.0))) 		error stop 11001
		if (.not. precision_r4 (arg(doCounter)%r2,5*atan(1.0))) 	error stop 11002
		if (.not. precision_r4 (arg(doCounter)%r3,7*atan(1.0))) 	error stop 11003
		if (.not. allocated(arg(doCounter)%r4) )					error stop 11010
		if (.not. precision_r4 (arg(doCounter)%r4(1),atan(1.0))) 	error stop 11014
		if (.not. precision_r4 (arg(doCounter)%r4(2),2*atan(1.0))) 	error stop 11024
		if (.not. precision_r4 (arg(doCounter)%r5(1),2*atan(1.0))) 	error stop 11015
		if (.not. precision_r4 (arg(doCounter)%r5(2),3*atan(1.0))) 	error stop 11025
		if (.not. precision_r4 (arg(doCounter)%r5(3),5*atan(1.0))) 	error stop 11035
	end do

	if (size(arg) .ne. SIZEOFA) 		error stop 111
	if ( any(lbound(arg) .ne. 1)) 		error stop 112
	if ( any(ubound(arg) .ne. SIZEOFA)) error stop 113
	if (rank(arg) .ne. 1) 				error stop 114
	if (any(shape(arg) .ne. SIZEOFA)) 	error stop 115

	do doCounter=1,SIZEOFA
		arg(doCounter)%r1=2*atan(1.0)
		arg(doCounter)%r2=>t1
		arg(doCounter)%r3=>p1
		deallocate(arg(doCounter)%r4)
		allocate(arg(doCounter)%r4(2))
		arg(doCounter)%r4=(/5*atan(1.0),3*atan(1.0)/)
		arg(doCounter)%r5=(/3*atan(1.0),2*atan(1.0),atan(1.0)/)
	end do
end subroutine

subroutine sub12(arg)
    type(t1) :: arg(10)
	value arg
	real, target :: t1, t2
	real, pointer :: p1
	t1=11*atan(1.0)
	t2=13*atan(1.0)
	p1=>t2

	do doCounter=1,SIZEOFA
		if (.not. precision_r4 (arg(doCounter)%r1,atan(1.0))) 		error stop 12001
		if (.not. precision_r4 (arg(doCounter)%r2,5*atan(1.0))) 	error stop 12002
		if (.not. precision_r4 (arg(doCounter)%r3,7*atan(1.0))) 	error stop 12003
		if (.not. allocated(arg(doCounter)%r4) )					error stop 12010
		if (.not. precision_r4 (arg(doCounter)%r4(1),atan(1.0))) 	error stop 12014
		if (.not. precision_r4 (arg(doCounter)%r4(2),2*atan(1.0))) 	error stop 12024
		if (.not. precision_r4 (arg(doCounter)%r5(1),2*atan(1.0))) 	error stop 12015
		if (.not. precision_r4 (arg(doCounter)%r5(2),3*atan(1.0))) 	error stop 12025
		if (.not. precision_r4 (arg(doCounter)%r5(3),5*atan(1.0))) 	error stop 12035
	end do

	if (size(arg) .ne. SIZEOFA) 		error stop 121
	if ( any(lbound(arg) .ne. 1)) 		error stop 122
	if ( any(ubound(arg) .ne. SIZEOFA)) error stop 123
	if (rank(arg) .ne. 1) 				error stop 124
	if (any(shape(arg) .ne. SIZEOFA)) 	error stop 125

	do doCounter=1,SIZEOFA
		arg(doCounter)%r1=2*atan(1.0)
		arg(doCounter)%r2=>t1
		arg(doCounter)%r3=>p1
		deallocate(arg(doCounter)%r4)
		allocate(arg(doCounter)%r4(2))
		arg(doCounter)%r4=(/5*atan(1.0),3*atan(1.0)/)
		arg(doCounter)%r5=(/3*atan(1.0),2*atan(1.0),atan(1.0)/)
	end do
end subroutine

subroutine sub13(arg)
    type(t1) :: arg(SIZEOFA)
	value arg
	real, target :: t1, t2
	real, pointer :: p1
	t1=11*atan(1.0)
	t2=13*atan(1.0)
	p1=>t2

	do doCounter=1,SIZEOFA
		if (.not. precision_r4 (arg(doCounter)%r1,atan(1.0))) 		error stop 13001
		if (.not. precision_r4 (arg(doCounter)%r2,5*atan(1.0))) 	error stop 13002
		if (.not. precision_r4 (arg(doCounter)%r3,7*atan(1.0))) 	error stop 13003
		if (.not. allocated(arg(doCounter)%r4) )					error stop 13010
		if (.not. precision_r4 (arg(doCounter)%r4(1),atan(1.0))) 	error stop 13014
		if (.not. precision_r4 (arg(doCounter)%r4(2),2*atan(1.0))) 	error stop 13024
		if (.not. precision_r4 (arg(doCounter)%r5(1),2*atan(1.0))) 	error stop 13015
		if (.not. precision_r4 (arg(doCounter)%r5(2),3*atan(1.0))) 	error stop 13025
		if (.not. precision_r4 (arg(doCounter)%r5(3),5*atan(1.0))) 	error stop 13035
	end do

	if (size(arg) .ne. SIZEOFA) 		error stop 131
	if ( any(lbound(arg) .ne. 1)) 		error stop 132
	if ( any(ubound(arg) .ne. SIZEOFA)) error stop 133
	if (rank(arg) .ne. 1) 				error stop 134
	if (any(shape(arg) .ne. SIZEOFA)) 	error stop 135

	do doCounter=1,SIZEOFA
		arg(doCounter)%r1=2*atan(1.0)
		arg(doCounter)%r2=>t1
		arg(doCounter)%r3=>p1
		deallocate(arg(doCounter)%r4)
		allocate(arg(doCounter)%r4(2))
		arg(doCounter)%r4=(/5*atan(1.0),3*atan(1.0)/)
		arg(doCounter)%r5=(/3*atan(1.0),2*atan(1.0),atan(1.0)/)
	end do
end subroutine

subroutine sub14(arg)
    type(t1) :: arg(:)
	value arg
	real, target :: t1, t2
	real, pointer :: p1
	t1=11*atan(1.0)
	t2=13*atan(1.0)
	p1=>t2

	do doCounter=1,SIZEOFA
		if (.not. precision_r4 (arg(doCounter)%r1,atan(1.0))) 		error stop 14001
		if (.not. precision_r4 (arg(doCounter)%r2,5*atan(1.0))) 	error stop 14002
		if (.not. precision_r4 (arg(doCounter)%r3,7*atan(1.0))) 	error stop 14003
		if (.not. allocated(arg(doCounter)%r4) )					error stop 14010
		if (.not. precision_r4 (arg(doCounter)%r4(1),atan(1.0))) 	error stop 14014
		if (.not. precision_r4 (arg(doCounter)%r4(2),2*atan(1.0))) 	error stop 14024
		if (.not. precision_r4 (arg(doCounter)%r5(1),2*atan(1.0))) 	error stop 14015
		if (.not. precision_r4 (arg(doCounter)%r5(2),3*atan(1.0))) 	error stop 14025
		if (.not. precision_r4 (arg(doCounter)%r5(3),5*atan(1.0))) 	error stop 14035
	end do

	if (size(arg) .ne. SIZEOFA) 		error stop 141
	if ( any(lbound(arg) .ne. 1)) 		error stop 142
	if ( any(ubound(arg) .ne. SIZEOFA)) error stop 143
	if (rank(arg) .ne. 1) 				error stop 144
	if (any(shape(arg) .ne. SIZEOFA)) 	error stop 145

	do doCounter=1,SIZEOFA
		arg(doCounter)%r1=2*atan(1.0)
		arg(doCounter)%r2=>t1
		arg(doCounter)%r3=>p1
		deallocate(arg(doCounter)%r4)
		allocate(arg(doCounter)%r4(2))
		arg(doCounter)%r4=(/5*atan(1.0),3*atan(1.0)/)
		arg(doCounter)%r5=(/3*atan(1.0),2*atan(1.0),atan(1.0)/)
	end do
end subroutine

subroutine sub21(arg,n)
	integer n
	type(t1), DIMENSION(n) :: arg
	value arg
	real, target :: t1, t2
	real, pointer :: p1
	t1=11*atan(1.0)
	t2=13*atan(1.0)
	p1=>t2

	do doCounter=1,SIZEOFA
		if (.not. precision_r4 (arg(doCounter)%r1,atan(1.0))) 		error stop 21001
		if (.not. precision_r4 (arg(doCounter)%r2,5*atan(1.0))) 	error stop 21002
		if (.not. precision_r4 (arg(doCounter)%r3,7*atan(1.0))) 	error stop 21003
		if (.not. allocated(arg(doCounter)%r4) )					error stop 21010
		if (.not. precision_r4 (arg(doCounter)%r4(1),atan(1.0))) 	error stop 21014
		if (.not. precision_r4 (arg(doCounter)%r4(2),2*atan(1.0))) 	error stop 21024
		if (.not. precision_r4 (arg(doCounter)%r5(1),2*atan(1.0))) 	error stop 21015
		if (.not. precision_r4 (arg(doCounter)%r5(2),3*atan(1.0))) 	error stop 21025
		if (.not. precision_r4 (arg(doCounter)%r5(3),5*atan(1.0))) 	error stop 21035
	end do

	if (size(arg) .ne. SIZEOFA) 		error stop 211
	if ( any(lbound(arg) .ne. 1)) 		error stop 212
	if ( any(ubound(arg) .ne. SIZEOFA)) error stop 213
	if (rank(arg) .ne. 1) 				error stop 214
	if (any(shape(arg) .ne. SIZEOFA)) 	error stop 215

	do doCounter=1,SIZEOFA
		arg(doCounter)%r1=2*atan(1.0)
		arg(doCounter)%r2=>t1
		arg(doCounter)%r3=>p1
		deallocate(arg(doCounter)%r4)
		allocate(arg(doCounter)%r4(2))
		arg(doCounter)%r4=(/5*atan(1.0),3*atan(1.0)/)
		arg(doCounter)%r5=(/3*atan(1.0),2*atan(1.0),atan(1.0)/)
	end do
end subroutine

subroutine sub22(arg)
	type(t1), DIMENSION(10) :: arg
	value arg
	real, target :: t1, t2
	real, pointer :: p1
	t1=11*atan(1.0)
	t2=13*atan(1.0)
	p1=>t2

	do doCounter=1,SIZEOFA
		if (.not. precision_r4 (arg(doCounter)%r1,atan(1.0))) 		error stop 22001
		if (.not. precision_r4 (arg(doCounter)%r2,5*atan(1.0))) 	error stop 22002
		if (.not. precision_r4 (arg(doCounter)%r3,7*atan(1.0))) 	error stop 22003
		if (.not. allocated(arg(doCounter)%r4) )					error stop 22010
		if (.not. precision_r4 (arg(doCounter)%r4(1),atan(1.0))) 	error stop 22014
		if (.not. precision_r4 (arg(doCounter)%r4(2),2*atan(1.0))) 	error stop 22024
		if (.not. precision_r4 (arg(doCounter)%r5(1),2*atan(1.0))) 	error stop 22015
		if (.not. precision_r4 (arg(doCounter)%r5(2),3*atan(1.0))) 	error stop 22025
		if (.not. precision_r4 (arg(doCounter)%r5(3),5*atan(1.0))) 	error stop 22035
	end do

	if (size(arg) .ne. SIZEOFA) 		error stop 221
	if ( any(lbound(arg) .ne. 1)) 		error stop 222
	if ( any(ubound(arg) .ne. SIZEOFA)) error stop 223
	if (rank(arg) .ne. 1) 				error stop 224
	if (any(shape(arg) .ne. SIZEOFA)) 	error stop 225

	do doCounter=1,SIZEOFA
		arg(doCounter)%r1=2*atan(1.0)
		arg(doCounter)%r2=>t1
		arg(doCounter)%r3=>p1
		deallocate(arg(doCounter)%r4)
		allocate(arg(doCounter)%r4(2))
		arg(doCounter)%r4=(/5*atan(1.0),3*atan(1.0)/)
		arg(doCounter)%r5=(/3*atan(1.0),2*atan(1.0),atan(1.0)/)
	end do
end subroutine

subroutine sub23(arg)
	type(t1), DIMENSION(SIZEOFA) :: arg
	value arg
	real, target :: t1, t2
	real, pointer :: p1
	t1=11*atan(1.0)
	t2=13*atan(1.0)
	p1=>t2

	do doCounter=1,SIZEOFA
		if (.not. precision_r4 (arg(doCounter)%r1,atan(1.0))) 		error stop 23001
		if (.not. precision_r4 (arg(doCounter)%r2,5*atan(1.0))) 	error stop 23002
		if (.not. precision_r4 (arg(doCounter)%r3,7*atan(1.0))) 	error stop 23003
		if (.not. allocated(arg(doCounter)%r4) )					error stop 23010
		if (.not. precision_r4 (arg(doCounter)%r4(1),atan(1.0))) 	error stop 23014
		if (.not. precision_r4 (arg(doCounter)%r4(2),2*atan(1.0))) 	error stop 23024
		if (.not. precision_r4 (arg(doCounter)%r5(1),2*atan(1.0))) 	error stop 23015
		if (.not. precision_r4 (arg(doCounter)%r5(2),3*atan(1.0))) 	error stop 23025
		if (.not. precision_r4 (arg(doCounter)%r5(3),5*atan(1.0))) 	error stop 23035
	end do

	if (size(arg) .ne. SIZEOFA) 		error stop 231
	if ( any(lbound(arg) .ne. 1)) 		error stop 232
	if ( any(ubound(arg) .ne. SIZEOFA)) error stop 233
	if (rank(arg) .ne. 1) 				error stop 234
	if (any(shape(arg) .ne. SIZEOFA)) 	error stop 235

	do doCounter=1,SIZEOFA
		arg(doCounter)%r1=2*atan(1.0)
		arg(doCounter)%r2=>t1
		arg(doCounter)%r3=>p1
		deallocate(arg(doCounter)%r4)
		allocate(arg(doCounter)%r4(2))
		arg(doCounter)%r4=(/5*atan(1.0),3*atan(1.0)/)
		arg(doCounter)%r5=(/3*atan(1.0),2*atan(1.0),atan(1.0)/)
	end do
end subroutine

subroutine sub24(arg)
	type(t1), DIMENSION(:) :: arg
	value arg
	real, target :: t1, t2
	real, pointer :: p1
	t1=11*atan(1.0)
	t2=13*atan(1.0)
	p1=>t2

	do doCounter=1,SIZEOFA
		if (.not. precision_r4 (arg(doCounter)%r1,atan(1.0))) 		error stop 24001
		if (.not. precision_r4 (arg(doCounter)%r2,5*atan(1.0))) 	error stop 24002
		if (.not. precision_r4 (arg(doCounter)%r3,7*atan(1.0))) 	error stop 24003
		if (.not. allocated(arg(doCounter)%r4) )					error stop 24010
		if (.not. precision_r4 (arg(doCounter)%r4(1),atan(1.0))) 	error stop 24014
		if (.not. precision_r4 (arg(doCounter)%r4(2),2*atan(1.0))) 	error stop 24024
		if (.not. precision_r4 (arg(doCounter)%r5(1),2*atan(1.0))) 	error stop 24015
		if (.not. precision_r4 (arg(doCounter)%r5(2),3*atan(1.0))) 	error stop 24025
		if (.not. precision_r4 (arg(doCounter)%r5(3),5*atan(1.0))) 	error stop 24035
	end do

	if (size(arg) .ne. SIZEOFA) 		error stop 241
	if ( any(lbound(arg) .ne. 1)) 		error stop 242
	if ( any(ubound(arg) .ne. SIZEOFA)) error stop 243
	if (rank(arg) .ne. 1) 				error stop 244
	if (any(shape(arg) .ne. SIZEOFA)) 	error stop 245

	do doCounter=1,SIZEOFA
		arg(doCounter)%r1=2*atan(1.0)
		arg(doCounter)%r2=>t1
		arg(doCounter)%r3=>p1
		deallocate(arg(doCounter)%r4)
		allocate(arg(doCounter)%r4(2))
		arg(doCounter)%r4=(/5*atan(1.0),3*atan(1.0)/)
		arg(doCounter)%r5=(/3*atan(1.0),2*atan(1.0),atan(1.0)/)
	end do
end subroutine

subroutine sub31(arg,arg2)
	type(t1) arg2(:)
	type(t1) arg(size(arg2))
	value arg
	real, target :: t1, t2
	real, pointer :: p1
	t1=11*atan(1.0)
	t2=13*atan(1.0)
	p1=>t2

	do doCounter=1,SIZEOFA
		if (.not. precision_r4 (arg(doCounter)%r1,atan(1.0))) 		error stop 31001
		if (.not. precision_r4 (arg(doCounter)%r2,5*atan(1.0))) 	error stop 31002
		if (.not. precision_r4 (arg(doCounter)%r3,7*atan(1.0))) 	error stop 31003
		if (.not. allocated(arg(doCounter)%r4) )					error stop 31010
		if (.not. precision_r4 (arg(doCounter)%r4(1),atan(1.0))) 	error stop 31014
		if (.not. precision_r4 (arg(doCounter)%r4(2),2*atan(1.0))) 	error stop 31024
		if (.not. precision_r4 (arg(doCounter)%r5(1),2*atan(1.0))) 	error stop 31015
		if (.not. precision_r4 (arg(doCounter)%r5(2),3*atan(1.0))) 	error stop 31025
		if (.not. precision_r4 (arg(doCounter)%r5(3),5*atan(1.0))) 	error stop 31035
	end do

	if (size(arg) .ne. SIZEOFA) 		error stop 311
	if ( any(lbound(arg) .ne. 1)) 		error stop 312
	if ( any(ubound(arg) .ne. SIZEOFA)) error stop 313
	if (rank(arg) .ne. 1) 				error stop 314
	if (any(shape(arg) .ne. SIZEOFA)) 	error stop 315

	do doCounter=1,SIZEOFA
		arg(doCounter)%r1=2*atan(1.0)
		arg(doCounter)%r2=>t1
		arg(doCounter)%r3=>p1
		deallocate(arg(doCounter)%r4)
		allocate(arg(doCounter)%r4(2))
		arg(doCounter)%r4=(/5*atan(1.0),3*atan(1.0)/)
		arg(doCounter)%r5=(/3*atan(1.0),2*atan(1.0),atan(1.0)/)
	end do
end subroutine

subroutine sub32(arg,arg2)
	type(t1) arg2(:)
	type(t1) , DIMENSION (size(arg2)) :: arg
	value arg
	real, target :: t1, t2
	real, pointer :: p1
	t1=11*atan(1.0)
	t2=13*atan(1.0)
	p1=>t2

	do doCounter=1,SIZEOFA
		if (.not. precision_r4 (arg(doCounter)%r1,atan(1.0))) 		error stop 32001
		if (.not. precision_r4 (arg(doCounter)%r2,5*atan(1.0))) 	error stop 32002
		if (.not. precision_r4 (arg(doCounter)%r3,7*atan(1.0))) 	error stop 32003
		if (.not. allocated(arg(doCounter)%r4) )					error stop 32010
		if (.not. precision_r4 (arg(doCounter)%r4(1),atan(1.0))) 	error stop 32014
		if (.not. precision_r4 (arg(doCounter)%r4(2),2*atan(1.0))) 	error stop 32024
		if (.not. precision_r4 (arg(doCounter)%r5(1),2*atan(1.0))) 	error stop 32015
		if (.not. precision_r4 (arg(doCounter)%r5(2),3*atan(1.0))) 	error stop 32025
		if (.not. precision_r4 (arg(doCounter)%r5(3),5*atan(1.0))) 	error stop 32035
	end do

	if (size(arg) .ne. SIZEOFA) 		error stop 321
	if ( any(lbound(arg) .ne. 1)) 		error stop 322
	if ( any(ubound(arg) .ne. SIZEOFA)) error stop 323
	if (rank(arg) .ne. 1) 				error stop 324
	if (any(shape(arg) .ne. SIZEOFA)) 	error stop 325

	do doCounter=1,SIZEOFA
		arg(doCounter)%r1=2*atan(1.0)
		arg(doCounter)%r2=>t1
		arg(doCounter)%r3=>p1
		deallocate(arg(doCounter)%r4)
		allocate(arg(doCounter)%r4(2))
		arg(doCounter)%r4=(/5*atan(1.0),3*atan(1.0)/)
		arg(doCounter)%r5=(/3*atan(1.0),2*atan(1.0),atan(1.0)/)
	end do
end subroutine

end