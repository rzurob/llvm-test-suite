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
!* - data-ptr is arg of pure function
!* - data-target is pure function call
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
  contains
       pure function func(arg)
	   double precision, pointer :: arg(:,:)
	   double precision, pointer :: func(:,:)
           allocate(func(size(arg,1), size(arg,2)), source =  &
             arg(ubound(arg,1):lbound(arg,1):-1,ubound(arg,2):lbound(arg,2):-1))
       end function
end module

program main
    use m

    double precision, pointer :: ptr(:,:)
    double precision, target :: tar(16)
    logical precision_r8

    tar = (/(real(i,8), i=1,16 )/)

    ptr(kind(tar):kind(tar)+3, 0:3) => tar

    if ( .not. associated(ptr)) error stop 11
    if ( any(lbound(ptr) .ne. (/8,0/))) error stop 12
    if ( any(ubound(ptr) .ne. (/11,3/))) error stop 13

    ptr(kind(ptr):, kind(tar):) => func(ptr)

    if ( .not. associated(ptr)) error stop 21
    if ( any(lbound(ptr) .ne. (/8,8/))) error stop 22
    if ( any(ubound(ptr) .ne. (/11,11/))) error stop 23

    if ( .not. precision_r8(ptr+ptr(10,10), &
                reshape((/(real(i,8),i=22,7,-1) /) ,(/4,4/)) )) error stop 25

end program
