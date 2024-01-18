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
!* - lb/ub are module variable with public/private attribute
!* - data-pointer is module function name
!*
!234567890123456789012345678901234567890123456789012345678901234567890

   module m
        integer, private :: lb
        integer, allocatable :: ub
        real, pointer :: tar(:)

        contains
            function func(arg)
                real, pointer :: func(:)
                integer :: arg

                allocate(tar(arg), source=(/ ( real(i*2.2),i=1,arg ) /) )

                func(lb:ub) => tar(41:)

	        if ( .not. associated(func) ) stop 5
                if ( lbound(func, 1) /= 31 ) stop 7
                if ( ubound(func, 1) /= 60 ) stop 9

            end function

            subroutine set_lbound(arg)
                integer :: arg
                lb = arg
            end subroutine
   end module

   program main
        use m

        real, allocatable :: aaa(:)
        integer, parameter :: uBnd=60, lBnd=31, numTar = 80

        ! ub is public module variable = 60
        allocate(ub, source = uBnd)
        if ( .not. allocated(ub) ) stop 1

        ! lb is private module variable = 31
        call set_lbound(lBnd)

        allocate(aaa(uBnd-lBnd+1), source=func(numTar))
        if ( .not. allocated(aaa) ) stop 21
        if ( size(aaa) /= 30 ) stop 23

        write(*, '(5f14.8)') aaa

        End program
