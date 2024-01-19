!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION
!*
!* - data-ptr used as arg of elemental function
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
  contains
       elemental function func(arg)
	   real(8), intent(in) :: arg
	   real(8) :: func

           func = arg * (-1)
       end function
end module

program main
    use m

    real*8, pointer :: ptr(:,:,:,:,:,:,:,:,:,:)
    real*8, target :: tar(10)

    tar = (/(real(i,8), i=1,10 )/)

    ptr(1:1,2:3,3:3,4:4,5:5,6:7,7:7,8:8,9:9,0:1) => tar(10:1:-1)

    ptr = func(ptr)

    if ( .not. associated(ptr)) error stop 11
    if ( any(lbound(ptr) .ne. (/1,2,3,4,5,6,7,8,9,0/))) error stop 12
    if ( any(ubound(ptr) .ne. (/1,3,3,4,5,7,7,8,9,1/))) error stop 13

    write(*, '(4f15.10)') ptr * ptr(1,2,3,4,5,7,7,8,9,1)
end program
