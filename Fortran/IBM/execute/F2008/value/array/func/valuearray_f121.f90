!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2015-09-24
!*
!*  PRIMARY FUNCTIONS TESTED   : VALUE(F2008 extension) - dummy argument arrays allowed with value
!*
!*  DESCRIPTION                : testing the extensions to the VALUE attribute
!*                                for an array of logical type(*1 kind)
!*								-passing an array to a subroutine to check that
!*								1. dummy argument is equal to the actual argument
!*								2. actual argument doesn't change
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

implicit none
integer SIZEOFA
parameter (SIZEOFA = 10)
logical*1  l1(10),l1_r(10)

l1  = .true.
l1_r= .true.

call sub11(l1,10)
if (any (l1 .NEQV. l1_r)) error stop 1101
call sub12(l1)
if (any (l1 .NEQV. l1_r)) error stop 1201
call sub13(l1)
if (any (l1 .NEQV. l1_r)) error stop 1301
call sub14(l1)
if (any (l1 .NEQV. l1_r)) error stop 1401
call sub21(l1,10)
if (any (l1 .NEQV. l1_r)) error stop 2101
call sub22(l1)
if (any (l1 .NEQV. l1_r)) error stop 2201
call sub23(l1)
if (any (l1 .NEQV. l1_r)) error stop 2301
call sub24(l1)
if (any (l1 .NEQV. l1_r)) error stop 2401
call sub31(l1,l1)
if (any (l1 .NEQV. l1_r)) error stop 3101
call sub32(l1,l1)
if (any (l1 .NEQV. l1_r)) error stop 3201

contains

subroutine sub11(arg,n)
    logical*1 :: arg(n)
	integer n
	value arg

	if (any (arg .NEQV. l1_r)) error stop 1110
	if (size(arg) .ne. SIZEOFA) error stop 1111
	if ( any(lbound(arg) .ne. 1)) error stop 1112
	if ( any(ubound(arg) .ne. SIZEOFA)) error stop 1113
	if (rank(arg) .ne. 1) error stop 1114
	if (any(shape(arg) .ne. SIZEOFA)) error stop 1115
	arg = .false.
end subroutine

subroutine sub12(arg)
    logical*1 :: arg(10)
	value arg

	if (any (arg .NEQV. l1_r)) error stop 1210
	if (size(arg) .ne. SIZEOFA) error stop 1211
	if ( any(lbound(arg) .ne. 1)) error stop 1212
	if ( any(ubound(arg) .ne. SIZEOFA)) error stop 1213
	if (rank(arg) .ne. 1) error stop 1214
	if (any(shape(arg) .ne. SIZEOFA)) error stop 1215
	arg = .false.
end subroutine

subroutine sub13(arg)
    logical*1 :: arg(SIZEOFA)
	value arg

	if (any (arg .NEQV. l1_r)) error stop 1310
	if (size(arg) .ne. SIZEOFA) error stop 1311
	if ( any(lbound(arg) .ne. 1)) error stop 1312
	if ( any(ubound(arg) .ne. SIZEOFA)) error stop 1313
	if (rank(arg) .ne. 1) error stop 1314
	if (any(shape(arg) .ne. SIZEOFA)) error stop 1315
	arg = .false.
end subroutine

subroutine sub14(arg)
    logical*1 :: arg(:)
	value arg

	if (any (arg .NEQV. l1_r)) error stop 1410
	if (size(arg) .ne. SIZEOFA) error stop 1411
	if ( any(lbound(arg) .ne. 1)) error stop 1412
	if ( any(ubound(arg) .ne. SIZEOFA)) error stop 1413
	if (rank(arg) .ne. 1) error stop 1414
	if (any(shape(arg) .ne. SIZEOFA)) error stop 1415
	arg = .false.
end subroutine

subroutine sub21(arg,n)
	integer n
	logical*1, DIMENSION(n) :: arg
	value arg

	if (any (arg .NEQV. l1_r)) error stop 2110
	if (size(arg) .ne. SIZEOFA) error stop 2111
	if ( any(lbound(arg) .ne. 1)) error stop 2112
	if ( any(ubound(arg) .ne. SIZEOFA)) error stop 2113
	if (rank(arg) .ne. 1) error stop 2114
	if (any(shape(arg) .ne. SIZEOFA)) error stop 2115
	arg = .false.
end subroutine

subroutine sub22(arg)
	logical*1, DIMENSION(10) :: arg
	value arg

	if (any (arg .NEQV. l1_r)) error stop 2210
	if (size(arg) .ne. SIZEOFA) error stop 2211
	if ( any(lbound(arg) .ne. 1)) error stop 2212
	if ( any(ubound(arg) .ne. SIZEOFA)) error stop 2213
	if (rank(arg) .ne. 1) error stop 2214
	if (any(shape(arg) .ne. SIZEOFA)) error stop 2215
	arg = .false.
end subroutine

subroutine sub23(arg)
	logical*1, DIMENSION(SIZEOFA) :: arg
	value arg

	if (any (arg .NEQV. l1_r)) error stop 2310
	if (size(arg) .ne. SIZEOFA) error stop 2311
	if ( any(lbound(arg) .ne. 1)) error stop 2312
	if ( any(ubound(arg) .ne. SIZEOFA)) error stop 2313
	if (rank(arg) .ne. 1) error stop 2314
	if (any(shape(arg) .ne. SIZEOFA)) error stop 2315
	arg = .false.
end subroutine

subroutine sub24(arg)
	logical*1, DIMENSION(:) :: arg
	value arg

	if (any (arg .NEQV. l1_r)) error stop 2410
	if (size(arg) .ne. SIZEOFA) error stop 2411
	if ( any(lbound(arg) .ne. 1)) error stop 2412
	if ( any(ubound(arg) .ne. SIZEOFA)) error stop 2413
	if (rank(arg) .ne. 1) error stop 2414
	if (any(shape(arg) .ne. SIZEOFA)) error stop 2415
	arg = .false.
end subroutine

subroutine sub31(arg,arg2)
	logical*1 arg2(:)
	logical*1 arg(size(arg2))
	value arg

	if (any (arg .NEQV. l1_r)) error stop 3110
	if (size(arg) .ne. SIZEOFA) error stop 3111
	if ( any(lbound(arg) .ne. 1)) error stop 3112
	if ( any(ubound(arg) .ne. SIZEOFA)) error stop 3113
	if (rank(arg) .ne. 1) error stop 3114
	if (any(shape(arg) .ne. SIZEOFA)) error stop 3115
	arg = .false.
end subroutine

subroutine sub32(arg,arg2)
	logical*1 arg2(:)
	logical*1 , DIMENSION (size(arg2)) :: arg
	value arg

	if (any (arg .NEQV. l1_r)) error stop 3210
	if (size(arg) .ne. SIZEOFA) error stop 3211
	if ( any(lbound(arg) .ne. 1)) error stop 3212
	if ( any(ubound(arg) .ne. SIZEOFA)) error stop 3213
	if (rank(arg) .ne. 1) error stop 3214
	if (any(shape(arg) .ne. SIZEOFA)) error stop 3215
	arg = .false.
end subroutine

end