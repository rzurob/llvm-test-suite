!******************************************************************************
!*  ===========================================================================
!*  XL Fortran Test Case                                  IBM INTERNAL USE ONLY
!*  ===========================================================================
!*
!*  TEST CASE NAME  : F2008/value/pure/unit_tests/purevalue02d.f
!*  TEST CASE TITLE : F2008: VALUE attr allowed for dummy args of PURE proc
!*  PROGRAMMER      : Gaby Baghdadi
!*  DATE            : 2010-12-01
!*  ORIGIN          : XL Fortran Compiler Development, IBM Torolab
!*  DRIVER STANZA   : xlf2003
!*
!*  DESCRIPTION
!*  - dummy data objects declared with neither intent(in) nor the value
!*     attribute
!*  - test above for pure and elemental functions and subroutines, with dummy
!*    args of all scalar data types
!*
!234567890123456789012345678901234567890123456789012345678901234567890123456789

real(16) pure function func1(si8,si4,si2,si1,si,sr16,sr8,sr4,sr,sx32,sx16,sb,sl,sx8,sx)
    integer*8 :: si8
    integer*4 :: si4
    integer*2 :: si2
    integer*1 :: si1
    integer :: si
    real*16 :: sr16
    real*8 :: sr8
    real*4 :: sr4
    real :: sr
    complex*32 :: sx32
    complex*16 :: sx16
    complex*8 :: sx8
    complex :: sx
    byte :: sb
    logical :: sl

    if (sl) then
	    func1 = real(si8,16) + real(si4,16) + real(si2,16) + real(si1,16) &
        + real(si,16) + sr16 + real(sr8,16) + real(sr4,16) + real(sr,16) &
        + real(sx32,16) + real(sx16,16) + real(sx8,16) +  real(sx,16) &
        + real(sb,16)
    else
        func1 = 0
    endif
end function

real*16 elemental function func2(si8,si4,si2,si1,si,sr16,sr8,sr4,sr,sx32,sx16,sb,sl,sx8,sx)
    integer*8 :: si8
    integer*4 :: si4
    integer*2 :: si2
    integer*1 :: si1
    integer :: si
    real*16 :: sr16
    real*8 :: sr8
    real*4 :: sr4
    real :: sr
    complex*32 :: sx32
    complex*16 :: sx16
    complex*8 :: sx8
    complex :: sx
    byte :: sb
    logical :: sl

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
    integer*8 :: si8, sbi8
    integer*4 :: si4, sbi4
    integer*2 :: si2, sbi2
    integer*1 :: si1, sbi1
    integer :: si, sbi
    real*16 :: sr16, sbr16
    real*8 :: sr8, sbr8
    real*4 :: sr4, sbr4
    real :: sr, sbr
    byte :: sb
    logical :: sl
    complex*32 :: sx32, sbx32
    complex*16 :: sx16, sbx16
    complex*8 :: sx8, sbx8
    complex :: sx, sbx

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
    byte :: sb
    integer*8 :: si8
    integer*4 :: si4
    integer*2 :: si2
    integer*1 :: si1
    integer :: si
    logical :: sl
    real*16 :: sr16
    real*8 :: sr8
    real*4 :: sr4
    real :: sr
    complex*32 :: sx32
    complex*16 :: sx16
    complex*8 :: sx8
    complex :: sx
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
    integer*8 :: si8
    integer*4 :: si4
    integer*2 :: si2
    integer*1 :: si1
    byte :: sb
    integer :: si
    real*16 :: sr16
    real*8 :: sr8
    real*4 :: sr4
    logical :: sl
    real :: sr
    complex*32 :: sx32
    complex*16 :: sx16
    complex*8 :: sx8
    complex :: sx
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
    logical :: sl
    real*16, intent(inout) :: res_sr
    integer*8 :: si8, sbi8
    integer*4 :: si4, sbi4
    integer*2 :: si2, sbi2
    integer*1 :: si1, sbi1
    integer :: si, sbi
    real*16 :: sr16, sbr16
    real*8 :: sr8, sbr8
    real*4 :: sr4, sbr4
    real :: sr, sbr
    complex*32 :: sx32, sbx32
    complex*16 :: sx16, sbx16
    complex*8 :: sx8, sbx8
    complex :: sx, sbx
    byte :: sb
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
