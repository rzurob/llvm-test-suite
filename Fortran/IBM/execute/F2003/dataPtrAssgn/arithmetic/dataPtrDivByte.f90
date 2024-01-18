!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrDivByte.f
!*
!*  DATE                       : Aug 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION
!*
!* - lb of data-ptr has value attr; verify lb's value after return from sub call
!* - data-ptr of type class(*), dynamic type is intrinsic byte
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m

    class(*), pointer :: p(:)

    contains
        subroutine sub(a)
            byte, target ,value :: a

            a = 12

            p(a:) => p
        end subroutine

end module

program main
    use m
    byte, target :: b
    byte, target :: tar(20)
    data tar /1,2,3,4,5,6,7,8,9,0,0,1,2,3,4,5,6,7,8,9/

    b = 18

    p(b:) => tar(::2)

    if ( .not. associated (p, tar(::2)) ) stop 10
    if ( lbound(p,1) /= 18 ) stop 11
    if ( ubound(p,1) /= 27 ) stop 13

    call sub(b)


    if ( .not. associated (p, tar(::2)) ) stop 20
    if ( lbound(p,1) /= 12 ) stop 21
    if ( ubound(p,1) /= 21 ) stop 23

    select type(p)
	type is (byte)
	    if ( any(p/p(b) .ne. (/0,1,2,3,4,0,1,2,3,4/))) stop 24
 	class default
	    stop 25
    end select

end program

