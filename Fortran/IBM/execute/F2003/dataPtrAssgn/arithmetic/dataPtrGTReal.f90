!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrGTReal.f 
!*
!*  PROGRAMMER                 : Michelle Zhang
!*  DATE                       : Aug 31, 2006
!*  ORIGIN                     : Compiler Development, IBM Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION
!*
!* - lb/ub of data-ptr are function call and entry call
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m

    contains
        function get_lb()
            integer, pointer :: get_lb, get_ub

            allocate(get_lb, source=-12)
            return
        entry get_ub

            allocate(get_ub, source=-3)
        end function

end module

program main
    use m

    real, pointer :: p(:)
    logical precision_r4

    allocate(p(100), source =  (/(real(i,4), i=1,100)/))

    p(get_lb():get_ub()) => p(::10)

    if ( .not. associated(p)) stop 1
    if ( lbound(p,1) /= -12) stop 2
    if ( ubound(p,1) /= -3) stop 3

    if ( .not. all(p .gt. (/(i-0.1, i=1,100,10)/))) stop 4
    if (.not. precision_r4( p, (/(real(i,4), i=1,100,10)/))) stop 5

end program
