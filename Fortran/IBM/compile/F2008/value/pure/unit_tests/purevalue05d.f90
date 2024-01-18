!******************************************************************************
!*  ===========================================================================
!*
!*  TEST CASE NAME  : F2008/value/pure/unit_tests/purevalue05d.f
!*  DATE            : 2010-12-01
!*  DRIVER STANZA   : xlf2003
!*
!*  DESCRIPTION
!*  - dummy data objects declared as arrays with value attribute
!*  - test above for module pure and elemental functions and subroutines, with
!*    dummy args of all scalar data types
!*  - update 2015/08/25 removed arrays from elemental procedures
!*
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module m
implicit none
contains
    real pure function func1(si8,si4,si2,si1,si,sr16,sr8,sr4,sr,sx32,sx16,sx8,sx)
        integer*8, value :: si8(0)
        integer*4 :: si4(1)
        value :: si4
        integer*2, value, dimension(2) :: si2
        integer*1, value :: si1(3)
        integer, value :: si(4)
        real*16, dimension(5) :: sr16
        value :: sr16
        real*8, value :: sr8(6)
        real*4, dimension(7), value :: sr4
        real, value :: sr(8)
        complex*32, value :: sx32(10)
        complex*16, value :: sx16(11)
        complex*8, value, dimension(12) :: sx8
        complex, dimension(13) :: sx
        value :: sx
	func1 = 0
    end function

    real elemental function func2(si8,si4,si2,si1,si,sr16,sr8,sr4,sr,sx32,sx16,sx8,sx)
        integer*8, value :: si8
        integer*4 :: si4
        value :: si4
        integer*2, value :: si2
        integer*1, value :: si1
        integer, value :: si
        real*16 :: sr16
        value :: sr16
        real*8, value :: sr8
        real*4, value :: sr4
        real, value :: sr
        complex*32, value :: sx32
        complex*16, value :: sx16
        complex*8, value :: sx8
        complex :: sx
        value :: sx
	func2 = 0
    end function

    pure subroutine sub1(si8,si4,si2,si1,si,sr16,sr8,sr4,sr,sx32,sx16,sx8,sx)
        value :: sx
        complex, dimension(13) :: sx
        complex*32, value :: sx32(10)
        real*16, dimension(5) :: sr16
        integer*4 :: si4(1)
        integer*2, value, dimension(2) :: si2
        integer*8, value :: si8(0)
        integer*1, value :: si1(3)
        integer, value :: si(4)
        value :: sr16
        value :: si4
        real*8, value :: sr8(6)
        real*4, dimension(7), value :: sr4
        real, value :: sr(8)
        complex*16, value :: sx16(11)
        complex*8, value, dimension(12) :: sx8
    end subroutine

    elemental subroutine sub2(si8,si4,si2,si1,si,sr16,sr8,sr4,sr,sx32,sx16,sx8,sx)
        value :: sx
        complex :: sx
        complex*32, value :: sx32
        real*16 :: sr16
        integer*4 :: si4
        integer*2, value :: si2
        integer*8, value :: si8
        integer*1, value :: si1
        integer, value :: si
        value :: sr16
        value :: si4
        real*8, value :: sr8
        real*4, value :: sr4
        real, value :: sr
        complex*16, value :: sx16
        complex*8, value :: sx8
    end subroutine
end module
end
