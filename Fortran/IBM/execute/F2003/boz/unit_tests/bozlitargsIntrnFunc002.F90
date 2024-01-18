!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug. 26, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : boz-literal args to REAL, INT, CMPLX and DBLE intrinsics
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Passing boz-literal constatns as arguments to
!*															 REAL, INT, CMPLX and DBLE intrinsics to make sure
!*															 the compiler works with -qxlf2003=bozlitargs off
!*
!234567890123456789012345678901234567890123456789012345678901234567890
    integer :: int_num0
    integer :: int_num1, int_num2(2), int_num3(4)
    integer :: int_num4(2), int_num5(4), int_num6(8)
    integer(8) :: int_num7
    integer(1) :: int_num8
    integer(2) :: int_num9

    real(4) :: real_num1
    real(8) :: real_num2
    real(16) :: real_num3
    complex(4) :: cmplx_num1
    complex(8) :: cmplx_num2
    complex(16) :: cmplx_num3

    equivalence(int_num1, real_num1)
    equivalence(int_num2, real_num2)
    equivalence(int_num3, real_num3)

    equivalence(int_num4, cmplx_num1)
    equivalence(int_num5, cmplx_num2)
    equivalence(int_num6, cmplx_num3)

    int_num0=int(b'1100100')
    if(int_num0.ne.0) error stop 1
    int_num0=int(o'144')
    if(int_num0.ne.0) error stop 2
    int_num0=int(z'64')
    if(int_num0.ne.0) error stop 3

    int_num7=int(b'1100100',8)
    if(int_num7.ne.0) error stop 4
    int_num7=int(o'144',8)
    if(int_num7.ne.0) error stop 5
    int_num7=int(z'64',8)
    if(int_num7.ne.0) error stop 6

    int_num8=int(b'1100100',1)
    if(int_num8.ne.0) error stop 7
    int_num8=int(o'144',1)
    if(int_num8.ne.0) error stop 8
    int_num8=int(z'64',1)
    if(int_num8.ne.0) error stop 9

    int_num8=int(b'1100100',2)
    if(int_num8.ne.0) error stop 10
    int_num8=int(o'144',2)
    if(int_num8.ne.0) error stop 11
    int_num8=int(z'64',2)
    if(int_num8.ne.0) error stop 12

    real_num1=real(b'1100100')
    if(int_num1 .ne. z'42C80000') error stop 13
    real_num1=real(o'144')
    if(int_num1 .ne. z'42C80000') error stop 14
    real_num1=real(z'64')
    if(int_num1 .ne. z'42C80000') error stop 15

    real_num2=real(b'1100100',8)
#if __LITTLE_ENDIAN__
    if(int_num2(2) .ne. z'40590000') error stop 16
    real_num2=real(o'144',8)
    if(int_num2(2) .ne. z'40590000') error stop 17
    real_num2=real(z'64',8)
    if(int_num2(2) .ne. z'40590000') error stop 18
#else
    if(int_num2(1) .ne. z'40590000') error stop 16
    real_num2=real(o'144',8)
    if(int_num2(1) .ne. z'40590000') error stop 17
    real_num2=real(z'64',8)
    if(int_num2(1) .ne. z'40590000') error stop 18
#endif

    real_num3=real(b'1100100',16)
#if __LITTLE_ENDIAN__
    if(int_num3(2) .ne.z'40590000') error stop 19
    real_num3=real(o'144',16)
    if(int_num3(2) .ne. z'40590000') error stop 20
    real_num3=real(z'64',16)
    if(int_num3(2) .ne. z'40590000') error stop 21
#else
    if(int_num3(1) .ne.z'40590000') error stop 19
    real_num3=real(o'144',16)
    if(int_num3(1) .ne. z'40590000') error stop 20
    real_num3=real(z'64',16)
    if(int_num3(1) .ne. z'40590000') error stop 21
#endif

    cmplx_num1=cmplx(b'1100100',b'1100100',4)
    if(int_num4(1) .ne. b'1100100' .or. int_num4(2) .ne. b'1100100') error stop 22
    cmplx_num1=cmplx(o'144',o'144',4)
    if(int_num4(1) .ne. o'144' .or. int_num4(2) .ne. o'144') error stop 23
    cmplx_num1=cmplx(z'64',z'64',4)
    if(int_num4(1) .ne. z'64' .or. int_num4(2) .ne. z'64') error stop 24

    cmplx_num2=cmplx(b'1100100',b'1100100',8)
#if __LITTLE_ENDIAN__
    if(int_num5(2) .ne. z'37090000' .or. int_num5(4) .ne. z'37090000') error stop 25
    cmplx_num2=cmplx(o'144',o'144',8)
    if(int_num5(2) .ne. z'37090000' .or. int_num5(4) .ne. z'37090000') error stop 26
    cmplx_num2=cmplx(z'64',z'64',8)
    if(int_num5(2) .ne. z'37090000' .or. int_num5(4) .ne. z'37090000') error stop 27
#else
    if(int_num5(1) .ne. z'37090000' .or. int_num5(3) .ne. z'37090000') error stop 25
    cmplx_num2=cmplx(o'144',o'144',8)
    if(int_num5(1) .ne. z'37090000' .or. int_num5(3) .ne. z'37090000') error stop 26
    cmplx_num2=cmplx(z'64',z'64',8)
    if(int_num5(1) .ne. z'37090000' .or. int_num5(3) .ne. z'37090000') error stop 27
#endif

    cmplx_num3=cmplx(b'1100100',b'1100100',16)
#if __LITTLE_ENDIAN__
    if(int_num6(2) .ne. z'37090000' .or. int_num6(6) .ne. z'37090000') error stop 28
    cmplx_num3=cmplx(o'144',o'144',16)
    if(int_num6(2) .ne. z'37090000' .or. int_num6(6) .ne. z'37090000') error stop 29
    cmplx_num3=cmplx(z'64',z'64',16)
    if(int_num6(2) .ne. z'37090000' .or. int_num6(6) .ne. z'37090000') error stop 30
#else
    if(int_num6(1) .ne. z'37090000' .or. int_num6(5) .ne. z'37090000') error stop 28
    cmplx_num3=cmplx(o'144',o'144',16)
    if(int_num6(1) .ne. z'37090000' .or. int_num6(5) .ne. z'37090000') error stop 29
    cmplx_num3=cmplx(z'64',z'64',16)
    if(int_num6(1) .ne. z'37090000' .or. int_num6(5) .ne. z'37090000') error stop 30
#endif

    cmplx_num1=cmplx(b'1100100',kind=4)
    if(int_num4(1) .ne. b'1100100') error stop 31
    cmplx_num1=cmplx(o'144',kind=4)
    if(int_num4(1) .ne. o'144' ) error stop 32
    cmplx_num1=cmplx(z'64',kind=4)
    if(int_num4(1) .ne. z'64') error stop 33

    cmplx_num2=cmplx(b'1100100',kind=8)
#if __LITTLE_ENDIAN__
    if(int_num5(2) .ne. z'37090000') error stop 34
    cmplx_num2=cmplx(o'144',kind=8)
    if(int_num5(2) .ne. z'37090000') error stop 35
    cmplx_num2=cmplx(z'64',kind=8)
    if(int_num5(2) .ne. z'37090000') error stop 36
#else
    if(int_num5(1) .ne. z'37090000') error stop 34
    cmplx_num2=cmplx(o'144',kind=8)
    if(int_num5(1) .ne. z'37090000') error stop 35
    cmplx_num2=cmplx(z'64',kind=8)
    if(int_num5(1) .ne. z'37090000') error stop 36
#endif

    cmplx_num3=cmplx(b'1100100',kind=16)
#if __LITTLE_ENDIAN__
    if(int_num6(2) .ne. z'37090000') error stop 37
    cmplx_num3=cmplx(o'144',kind=16)
    if(int_num6(2) .ne. z'37090000') error stop 38
    cmplx_num3=cmplx(z'64',kind=16)
    if(int_num6(2) .ne. z'37090000') error stop 39
#else
    if(int_num6(1) .ne. z'37090000') error stop 37
    cmplx_num3=cmplx(o'144',kind=16)
    if(int_num6(1) .ne. z'37090000') error stop 38
    cmplx_num3=cmplx(z'64',kind=16)
    if(int_num6(1) .ne. z'37090000') error stop 39
#endif

    real_num2=dble(b'1100100')
#if __LITTLE_ENDIAN__
    if(int_num2(2) .ne. z'37090000') error stop 40
    real_num2=dble(o'144')
    if(int_num2(2) .ne. z'37090000') error stop 41
    real_num2=dble(z'64')
    if(int_num2(2) .ne. z'37090000') error stop 42
#else
    if(int_num2(1) .ne. z'37090000') error stop 40
    real_num2=dble(o'144')
    if(int_num2(1) .ne. z'37090000') error stop 41
    real_num2=dble(z'64')
    if(int_num2(1) .ne. z'37090000') error stop 42
#endif

end program
