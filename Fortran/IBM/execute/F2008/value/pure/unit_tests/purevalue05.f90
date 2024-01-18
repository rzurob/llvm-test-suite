!******************************************************************************
!*  ===========================================================================
!*
!*  TEST CASE NAME  : F2008/value/pure/unit_tests/purevalue05.f
!*  DATE            : 2010-12-01
!*  DRIVER STANZA   : xlf2003
!*
!*  DESCRIPTION
!*  - same as purevalue04.f, except dummy args that are declared with value
!*    attribute are also declared as intent(in); same behavior is expected
!*
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program main
    use xlf_precision
    implicit none
    external precision_r4
    logical precision_r4
    integer*8 :: si8 = 1
    integer*4 :: si4 = 2
    integer*2 :: si2 = 3
    integer*1 :: si1 = 4
    integer :: si = 5
    integer*8 :: res_si

    real*16 :: sr16 = 6.1
    real*8 :: sr8 = 7.2
    real*4 :: sr4 = 8.3
    real :: sr = 9.4
    real*16 :: res_sr

    complex*32 :: sx32 = (10.5,11.6)
    complex*16 :: sx16 = (12.7,13.8)
    complex*8 :: sx8 = (14.9,15.10)
    complex :: sx = (16.11,17.12)
    complex*32 :: res_sx

    byte :: sb = 127
    logical :: sl = .true.

    res_sr = func1(si8,si4,si2,si1,si,sr16,sr8,sr4,sr,sx32,sx16,sb,sl,sx8,sx)
    if (.not.precision_range_r16(res_sr,227.21_16,0.0000005Q0)) stop 1

    res_sr = func2(si8,si4,si2,si1,si,sr16,sr8,sr4,sr,sx32,sx16,sb,sl,sx8,sx)
    if (.not.precision_range_r16(res_sr,227.21_16,0.0000005Q0)) stop 2

    res_sr = func3(si8,si4,si2,si1,si,sr16,sr8,sr4,sr,sb,.false.,sx32,sx16,sx8,&
                 sx,si8,si4,si2,si1,si,sr16,sr8,sr4,sr,sx32,sx16,sx8,sx)
    if (.not.precision_range_r16(res_sr,(327.42_16),0.0000005Q0)) stop 3

    res_sr = 0
    call sub1(res_sr,sb,si8,si4,si2,si1,si,sl,sr16,sr8,sr4,sr,sx32,sx16,sx8,sx)
    if (.not.precision_range_r16(res_sr,227.21_16,0.0000005Q0)) stop 4

    res_sr = 0
    call sub2(res_sr,si8,si4,si2,si1,sb,si,sr16,sr8,sl,sr4,sr,sx32,sx16,sx8,sx)
    if (.not.precision_range_r16(res_sr,227.21_16,0.0000005Q0)) stop 5

    res_sr = 0
    call sub3(sl,res_sr,si8,si4,si2,si1,si,sr16,sr8,sr4,sr,sx32,sx16,sx8,sx,&
              si8,si4,si2,si1,si,sr16,sr8,sr4,sr,sx32,sx16,sx8,sx,sb)
    if (.not.precision_range_r16(res_sr,200.42_16,0.0000005Q0)) stop 6

contains
    real(16) pure function func1(si8,si4,si2,si1,si,sr16,sr8,sr4,sr,sx32,sx16,sb,sl,sx8,sx)
        integer*8, value, intent(in) :: si8
        integer*4, value, intent(in) :: si4
        integer*2, value, intent(in) :: si2
        integer*1, value, intent(in) :: si1
        integer, value, intent(in) :: si
        real*16, value, intent(in) :: sr16
        real*8, value, intent(in) :: sr8
        real*4, value, intent(in) :: sr4
        real, value, intent(in) :: sr
        complex*32, value, intent(in) :: sx32
        complex*16, value, intent(in) :: sx16
        complex*8, value, intent(in) :: sx8
        complex, value, intent(in) :: sx
        byte, value, intent(in) :: sb
        logical, value, intent(in) :: sl

        if (sl) then
	    func1 = real(si8,16) + real(si4,16) + real(si2,16) + real(si1,16) &
            + real(si,16) + sr16 + real(sr8,16) + real(sr4,16) + real(sr,16) &
            + real(sx32,16) + real(sx16,16) + real(sx8,16) +  real(sx,16) &
            + real(sb,16)
        else
            func1 = 0
        endif
    end function

    real(16) elemental function func2(si8,si4,si2,si1,si,sr16,sr8,sr4,sr,sx32,sx16,sb,sl,sx8,sx)
        integer*8, value, intent(in) :: si8
        integer*4, value, intent(in) :: si4
        integer*2, value, intent(in) :: si2
        integer*1, value, intent(in) :: si1
        integer, value, intent(in) :: si
        real*16, value, intent(in) :: sr16
        real*8, value, intent(in) :: sr8
        real*4, value, intent(in) :: sr4
        real, value, intent(in) :: sr
        complex*32, value, intent(in) :: sx32
        complex*16, value, intent(in) :: sx16
        complex*8, value, intent(in) :: sx8
        complex, value, intent(in) :: sx
        byte, value, intent(in) :: sb
        logical, value, intent(in) :: sl

        if (sl) then
	    func2 = real(si8,16) + real(si4,16) + real(si2,16) + real(si1,16) &
            + real(si,16) + sr16 + real(sr8,16) + real(sr4,16) + real(sr,16) &
            + real(sx32,16) + real(sx16,16) + real(sx8,16) +  real(sx,16) &
            + real(sb,16)
        else
            func2 = 0
        endif
    end function

    real(16) pure function func3( &
           si8,si4,si2,si1,si,sr16,sr8,sr4,sr,sb,sl,sx32,sx16,sx8,sx, &
           sbi8,sbi4,sbi2,sbi1,sbi,sbr16,sbr8,sbr4,sbr,sbx32,sbx16,sbx8,sbx)
        integer*8, value, intent(in) :: si8, sbi8
        integer*4, value, intent(in) :: si4, sbi4
        integer*2, value, intent(in) :: si2, sbi2
        integer*1, value, intent(in) :: si1, sbi1
        integer, value, intent(in) :: si, sbi
        real*16, value, intent(in) :: sr16, sbr16
        real*8, value, intent(in) :: sr8, sbr8
        real*4, value, intent(in) :: sr4, sbr4
        real, value, intent(in) :: sr, sbr
        byte, value, intent(in) :: sb
        logical, value, intent(in) :: sl
        complex*32, value, intent(in) :: sx32, sbx32
        complex*16, value, intent(in) :: sx16, sbx16
        complex*8, value, intent(in) :: sx8, sbx8
        complex, value, intent(in) :: sx, sbx

        if (.not. sl) then
	    func3 = real(si8,16) + real(si4,16) + real(si2,16) + real(si1,16) &
            + real(si,16) + sr16 + real(sr8,16) + real(sr4,16) + real(sr,16) &
            + real(sx32,16) + real(sx16,16) + real(sx8,16) +  real(sx,16) &
	    + real(sbi8,16) + real(sbi4,16) + real(sbi2,16) + real(sbi1,16) &
            + real(sbi,16) + sbr16 + real(sbr8,16) + real(sbr4,16) &
            + real(sbr,16) + real(sbx32,16) + real(sb,16) + real(sbx16,16) &
            + real(sbx8,16) +  real(sbx,16)
        else
            func3 = 0
        endif
    end function

    pure subroutine sub1 (res_sr,sb,si8,si4,si2,si1,si,sl,sr16,sr8,sr4,sr,sx32,sx16,sx8,sx)
        real*16, intent(out):: res_sr
        byte, value, intent(in) :: sb
        integer*8, value, intent(in) :: si8
        integer*4, value, intent(in) :: si4
        integer*2, value, intent(in) :: si2
        integer*1, value, intent(in) :: si1
        integer, value, intent(in) :: si
        logical, value, intent(in) :: sl
        real*16, value, intent(in) :: sr16
        real*8, value, intent(in) :: sr8
        real*4, value, intent(in) :: sr4
        real, value, intent(in) :: sr
        complex*32, value, intent(in) :: sx32
        complex*16, value, intent(in) :: sx16
        complex*8, value, intent(in) :: sx8
        complex, value, intent(in) :: sx
        if (sl) then
	    res_sr = real(sb,16) + real(si8,16) + real(si4,16) + real(si2,16) &
            + real(si1,16) + real(si,16) + sr16 + real(sr8,16) + real(sr4,16) &
            + real(sr,16) + real(sx32,16) + real(sx16,16) + real(sx8,16) &
            + real(sx,16)
        else
           res_sr = 0
        endif
    end subroutine

    elemental subroutine sub2 (res_sr,si8,si4,si2,si1,sb,si,sr16,sr8,sl,sr4,sr,sx32,sx16,sx8,sx)
        real*16, intent(out):: res_sr
        integer*8, value, intent(in) :: si8
        integer*4, value, intent(in) :: si4
        integer*2, value, intent(in) :: si2
        integer*1, value, intent(in) :: si1
        byte, value, intent(in) :: sb
        integer, value, intent(in) :: si
        real*16, value, intent(in) :: sr16
        real*8, value, intent(in) :: sr8
        real*4, value, intent(in) :: sr4
        logical, value, intent(in) :: sl
        real, value, intent(in) :: sr
        complex*32, value, intent(in) :: sx32
        complex*16, value, intent(in) :: sx16
        complex*8, value, intent(in) :: sx8
        complex, value, intent(in) :: sx
        if (sl) then
	    res_sr = real(si8,16) + real(si4,16) + real(si2,16) + real(si1,16) &
            + real(sb,16) + real(si,16) + sr16 + real(sr8,16) + real(sr4,16) &
            + real(sr,16) + real(sx32,16) + real(sx16,16) + real(sx8,16) &
            + real(sx,16)
        else
            res_sr = 0
        endif
    end subroutine

    elemental subroutine sub3( sl,res_sr, &
           si8,si4,si2,si1,si,sr16,sr8,sr4,sr,sx32,sx16,sx8,sx, &
           sbi8,sbi4,sbi2,sbi1,sbi,sbr16,sbr8,sbr4,sbr,sbx32,sbx16,sbx8,sbx,sb)
        logical, value, intent(in) :: sl
        real*16, intent(inout) :: res_sr
        integer*8, value, intent(in) :: si8, sbi8
        integer*4, value, intent(in) :: si4, sbi4
        integer*2, value, intent(in) :: si2, sbi2
        integer*1, value, intent(in) :: si1, sbi1
        integer, value, intent(in) :: si, sbi
        real*16, value, intent(in) :: sr16, sbr16
        real*8, value, intent(in) :: sr8, sbr8
        real*4, value, intent(in) :: sr4, sbr4
        real, value, intent(in) :: sr, sbr
        complex*32, value, intent(in) :: sx32, sbx32
        complex*16, value, intent(in) :: sx16, sbx16
        complex*8, value, intent(in) :: sx8, sbx8
        complex, value, intent(in) :: sx, sbx
        byte, value, intent(in) :: sb
        if (sl) then
	    res_sr = real(si8,16) + real(si4,16) + real(si2,16) + real(si1,16) &
            + real(si,16) + sr16 + real(sr8,16) + real(sr4,16) + real(sr,16) &
            + real(sx32,16) + real(sx16,16) + real(sx8,16) +  real(sx,16) &
	    + real(sbi8,16) + real(sbi4,16) + real(sbi2,16) + real(sbi1,16) &
            + real(sbi,16) + sbr16 + real(sbr8,16) + real(sbr4,16) &
            + real(sbr,16) + real(sbx32,16) + real(sbx16,16) + real(sbx8,16) &
            + real(sbx,16)
        else
           res_sr = 0
        endif
    end subroutine
end
