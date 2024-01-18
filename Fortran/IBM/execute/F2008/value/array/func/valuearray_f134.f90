!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : F2008/value/array/func/valuearray_f134.f
!*
!*  DATE                       : 2015-09-24
!*
!*  PRIMARY FUNCTIONS TESTED   : VALUE(F2008 extension) - dummy argument arrays allowed with value
!*
!*  DESCRIPTION                : testing the extensions to the VALUE attribute
!*                                for an array of complex type, checking interactions between
!*								declaring variables with * or with () eg. complex*8 vs complex(4)
!*								-passing an array to a subroutine to check that
!*								1. dummy argument is equal to the actual argument
!*								2. actual argument doesn't change
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

implicit none
integer SIZEOFA, doCounter
parameter (SIZEOFA = 10)
logical, external :: precision_x8,precision_x6,precision_x3
complex*8   r1(10),r1_r(10)
complex*16  r2(10),r2_r(10)
complex*32  r3(10),r3_r(10)
complex(4)  r4(10),r4_r(10)
complex(8)  r5(10),r5_r(10)
complex(16) r6(10),r6_r(10)

r1  =(atan(1.0_4),atan(1.0_4))
r1_r=(atan(1.0_4),atan(1.0_4))

r2  =(2*datan(1.0_8),2*datan(1.0_8))
r2_r=(2*datan(1.0_8),2*datan(1.0_8))

r3  =(3*qatan(1.0_16),3*qatan(1.0_16))
r3_r=(3*qatan(1.0_16),3*qatan(1.0_16))

r4  =(atan(1.0_4),atan(1.0_4))
r4_r=(atan(1.0_4),atan(1.0_4))

r5  =(2*datan(1.0_8),2*datan(1.0_8))
r5_r=(2*datan(1.0_8),2*datan(1.0_8))

r6  =(3*qatan(1.0_16),3*qatan(1.0_16))
r6_r=(3*qatan(1.0_16),3*qatan(1.0_16))


call sub11(r1)
	do doCounter=1,SIZEOFA
	if (.not. precision_x8(r1(doCounter),r1_r(doCounter))) error stop 11
	end do

call sub12(r2)
	do doCounter=1,SIZEOFA
	if (.not. precision_x6(r2(doCounter),r2_r(doCounter))) error stop 12
	end do

call sub13(r3)
	do doCounter=1,SIZEOFA
	if (.not. precision_x3(r3(doCounter),r3_r(doCounter))) error stop 13
	end do

call sub14(r4)
	do doCounter=1,SIZEOFA
	if (.not. precision_x8(r4(doCounter),r4_r(doCounter))) error stop 14
	end do

call sub15(r5)
	do doCounter=1,SIZEOFA
	if (.not. precision_x6(r5(doCounter),r5_r(doCounter))) error stop 15
	end do

call sub16(r6)
	do doCounter=1,SIZEOFA
	if (.not. precision_x3(r6(doCounter),r6_r(doCounter))) error stop 16
	end do

contains

subroutine sub11(arg)
    complex(4) :: arg(:)
	value arg

	do doCounter=1,SIZEOFA
		if (.not. precision_x8(arg(doCounter),r1(doCounter))) 	error stop 110
	end do
	if (size(arg) .ne. SIZEOFA) 								error stop 111
	if ( any(lbound(arg) .ne. 1)) 								error stop 112
	if ( any(ubound(arg) .ne. SIZEOFA)) 						error stop 113
	if (rank(arg) .ne. 1) 										error stop 114
	if (any(shape(arg) .ne. SIZEOFA)) 							error stop 115
	arg=(4*atan(1.0_4),4*atan(1.0_4))
end subroutine

subroutine sub12(arg)
    complex(8) :: arg(:)
	value arg

	do doCounter=1,SIZEOFA
		if (.not. precision_x6(arg(doCounter),r2(doCounter))) 	error stop 120
	end do
	if (size(arg) .ne. SIZEOFA) 								error stop 121
	if ( any(lbound(arg) .ne. 1)) 								error stop 122
	if ( any(ubound(arg) .ne. SIZEOFA)) 						error stop 123
	if (rank(arg) .ne. 1) 										error stop 124
	if (any(shape(arg) .ne. SIZEOFA)) 							error stop 125
	arg=(4*datan(1.0_8),4*datan(1.0_8))
end subroutine

subroutine sub13(arg)
    complex(16) :: arg(:)
	value arg

	do doCounter=1,SIZEOFA
		if (.not. precision_x3(arg(doCounter),r3(doCounter))) 	error stop 130
	end do
	if (size(arg) .ne. SIZEOFA) 								error stop 131
	if ( any(lbound(arg) .ne. 1)) 								error stop 132
	if ( any(ubound(arg) .ne. SIZEOFA)) 						error stop 133
	if (rank(arg) .ne. 1) 										error stop 134
	if (any(shape(arg) .ne. SIZEOFA)) 							error stop 135
	arg=(4*qatan(1.0_16),4*qatan(1.0_16))
end subroutine

subroutine sub14(arg)
    complex*8 :: arg(:)
	value arg

	do doCounter=1,SIZEOFA
		if (.not. precision_x8(arg(doCounter),r4(doCounter))) 	error stop 140
	end do
	if (size(arg) .ne. SIZEOFA) 								error stop 141
	if ( any(lbound(arg) .ne. 1)) 								error stop 142
	if ( any(ubound(arg) .ne. SIZEOFA)) 						error stop 143
	if (rank(arg) .ne. 1) 										error stop 144
	if (any(shape(arg) .ne. SIZEOFA)) 							error stop 145
	arg=(4*atan(1.0_4),4*atan(1.0_4))
end subroutine

subroutine sub15(arg)
    complex*16 :: arg(:)
	value arg

	do doCounter=1,SIZEOFA
		if (.not. precision_x6(arg(doCounter),r5(doCounter))) 	error stop 150
	end do
	if (size(arg) .ne. SIZEOFA) 								error stop 151
	if ( any(lbound(arg) .ne. 1)) 								error stop 152
	if ( any(ubound(arg) .ne. SIZEOFA)) 						error stop 153
	if (rank(arg) .ne. 1) 										error stop 154
	if (any(shape(arg) .ne. SIZEOFA)) 							error stop 155
	arg=(4*datan(1.0_8),4*datan(1.0_8))
end subroutine

subroutine sub16(arg)
    complex*32 :: arg(:)
	value arg

	do doCounter=1,SIZEOFA
		if (.not. precision_x3(arg(doCounter),r6(doCounter))) 	error stop 160
	end do
	if (size(arg) .ne. SIZEOFA) 								error stop 161
	if ( any(lbound(arg) .ne. 1)) 								error stop 162
	if ( any(ubound(arg) .ne. SIZEOFA)) 						error stop 163
	if (rank(arg) .ne. 1) 										error stop 164
	if (any(shape(arg) .ne. SIZEOFA)) 							error stop 165
	arg=(4*qatan(1.0_16),4*qatan(1.0_16))
end subroutine

end