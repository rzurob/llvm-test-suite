!******************************************************************************
!*  ===========================================================================
!*
!*  TEST CASE NAME  : F2008/value/pure/unit_tests/purevalue04d.f
!*  DATE            : 2010-12-01
!*  DRIVER STANZA   : xlf2003
!*
!*  DESCRIPTION
!* - declare dummy data objects with value attribute and one of:
!*   POINTER, ALLOCATABLE, VOLATILE, INTENT(INOUT), or INTENT(OUT) attributes
!* - specify value attribute more than once for an object
!* - attempt to modify intent(in) dummy arg that also has value attribute
!*
!234567890123456789012345678901234567890123456789012345678901234567890123456789

    implicit none

contains
    real(16) pure function func1(si8,si4,si2,si1,si,sr16,sr8,sr4,sr,sx32,sx16,sx8,sx)
        integer*8, value, intent(inout) :: si8
        integer*4, value, pointer :: si4
        integer*2, value, allocatable :: si2
        integer*1, value, volatile :: si1
        integer, value, intent(inout) :: si
        real*16, value, intent(out) :: sr16
        real*8, intent(inout), value :: sr8
        real*4, value, intent(inout) :: sr4
        real, value, intent(inout) :: sr
        complex*32, value, allocatable :: sx32
        complex*16, intent(inout), value :: sx16
        complex*8, value, intent(out) :: sx8
        complex, value, pointer :: sx
	func1 = real(si8,16)+real(si4,16)+real(si2,16)+real(si1,16)+real(si,16)&
              + sr16 + real(sr8,16) + real(sr4,16) + real(sr,16) &
              + real(sx32,16) + real(sx16,16) + real(sx8,16) +  real(sx,16)
    end function

    real(16) elemental function func2(si8,si4,si2,si1,si,sr16,sr8,sr4,sr,sx32,sx16,sx8,sx)
        integer*8, value, pointer :: si8
        integer*4, value, intent(out) :: si4
        integer*2, value, intent(inout) :: si2
        integer*1, value, intent(inout) :: si1
        integer, value, intent(inout) :: si
        real*16, value, intent(inout) :: sr16
        real*8, value, intent(out) :: sr8
        real*4, value, volatile :: sr4
        real, value, intent(inout) :: sr
        complex*32, value, intent(out) :: sx32
        complex*16, value, intent(out) :: sx16
        complex*8, value, pointer :: sx8
        complex, value, intent(inout) :: sx
	func2 = real(si8,16)+real(si4,16)+real(si2,16)+real(si1,16)+real(si,16)&
              + sr16 + real(sr8,16) + real(sr4,16) + real(sr,16) &
              + real(sx32,16) + real(sx16,16) + real(sx8,16) +  real(sx,16)
    end function

    real(16) pure function func3( &
           si8,si4,si2,si1,si,sr16,sr8,sr4,sr,sx32,sx16,sx8,sx, &
           sbi8,sbi4,sbi2,sbi1,sbi,sbr16,sbr8,sbr4,sbr,sbx32,sbx16,sbx8,sbx)
        integer*8, intent(inout), value :: si8, sbi8
        integer*4, value, intent(out) :: si4, sbi4
        integer*2, intent(inout), value :: si2, sbi2
        integer*1, intent(inout), value :: si1, sbi1
        integer, intent(inout), value :: si, sbi
        real*16, intent(inout), value :: sr16, sbr16
        real*8, intent(inout), value :: sr8, sbr8
        real*4, value, intent(out) :: sr4, sbr4
        real, intent(inout), value :: sr, sbr
        complex*32, intent(inout), value :: sx32, sbx32
        complex*16, intent(inout), value :: sx16, sbx16
        complex*8, intent(inout), value :: sx8, sbx8
        complex, value, intent(out) :: sx, sbx
	func3 = real(si8,16)+real(si4,16)+real(si2,16)+real(si1,16)+real(si,16)&
              + sr16 + real(sr8,16) + real(sr4,16) + real(sr,16) &
              + real(sx32,16) + real(sx16,16) + real(sx8,16) +  real(sx,16) &
	      + real(sbi8,16)+real(sbi4,16)+real(sbi2,16)+real(sbi1,16)+real(sbi,16)&
              + sbr16 + real(sbr8,16) + real(sbr4,16) + real(sbr,16) &
              + real(sbx32,16) + real(sbx16,16) + real(sbx8,16) +  real(sbx,16)
    end function

    pure subroutine sub1 (res_sr,si8,si4,si2,si1,si,sr16,sr8,sr4,sr,sx32,sx16,sx8,sx)
        real*16, intent(in), value :: res_sr
        integer*8, value, intent(out) :: si8
        integer*4, intent(inout), value :: si4
        integer*2, intent(inout), value :: si2
        integer*1, pointer, value :: si1
        integer, value, allocatable :: si
        real*16, intent(inout), value :: sr16
        real*8, intent(inout), value :: sr8
        real*4, value, value :: sr4
        real, intent(inout), value :: sr
        complex*32, intent(inout), value :: sx32
        complex*16, intent(inout), value :: sx16
        complex*8, intent(inout), value :: sx8
        complex, intent(inout), value :: sx
	res_sr=real(si8,16)+real(si4,16)+real(si2,16)+real(si1,16)+real(si,16)&
              + sr16 + real(sr8,16) + real(sr4,16) + real(sr,16) &
              + real(sx32,16) + real(sx16,16) + real(sx8,16) +  real(sx,16)
    end subroutine

    elemental subroutine sub2 (res_sr,si8,si4,si2,si1,si,sr16,sr8,sr4,sr,sx32,sx16,sx8,sx)
        real*16, intent(out), value :: res_sr
        integer*8, intent(inout), value :: si8
        integer*4, intent(inout), value :: si4
        integer*2, intent(inout), value :: si2
        integer*1, intent(inout), value :: si1
        integer, intent(inout), value :: si
        real*16, intent(inout), value :: sr16
        real*8, intent(inout), value :: sr8
        real*4, intent(inout), value :: sr4
        real, intent(inout), value :: sr
        complex*32, intent(inout), value :: sx32
        complex*16, intent(inout), value :: sx16
        complex*8, intent(inout), value :: sx8
        complex, intent(inout), value :: sx
	res_sr=real(si8,16)+real(si4,16)+real(si2,16)+real(si1,16)+real(si,16)&
              + sr16 + real(sr8,16) + real(sr4,16) + real(sr,16) &
              + real(sx32,16) + real(sx16,16) + real(sx8,16) +  real(sx,16)
    end subroutine

    elemental subroutine sub3( res_sr, &
           si8,si4,si2,si1,si,sr16,sr8,sr4,sr,sx32,sx16,sx8,sx, &
           sbi8,sbi4,sbi2,sbi1,sbi,sbr16,sbr8,sbr4,sbr,sbx32,sbx16,sbx8,sbx)
        real*16, intent(inout), value :: res_sr
        integer*8, value, intent(out) :: si8, sbi8
        integer*4, intent(inout), value :: si4, sbi4
        integer*2, value, intent(out) :: si2, sbi2
        integer*1, value, intent(out) :: si1, sbi1
        integer, value, intent(out) :: si, sbi
        real*16, value, intent(out) :: sr16, sbr16
        real*8, intent(inout), value :: sr8, sbr8
        real*4, intent(inout), value :: sr4, sbr4
        real, intent(inout), value :: sr, sbr
        complex*32, intent(inout), value :: sx32, sbx32
        complex*16, value, intent(out) :: sx16, sbx16
        complex*8, intent(inout), value :: sx8, sbx8
        complex, intent(inout), value :: sx, sbx
	res_sr=real(si8,16)+real(si4,16)+real(si2,16)+real(si1,16)+real(si,16)&
              + sr16 + real(sr8,16) + real(sr4,16) + real(sr,16) &
              + real(sx32,16) + real(sx16,16) + real(sx8,16) +  real(sx,16) &
	      + real(sbi8,16)+real(sbi4,16)+real(sbi2,16)+real(sbi1,16)+real(sbi,16)&
              + sbr16 + real(sbr8,16) + real(sbr4,16) + real(sbr,16) &
              + real(sbx32,16) + real(sbx16,16) + real(sbx8,16) +  real(sbx,16)
    end subroutine
end
