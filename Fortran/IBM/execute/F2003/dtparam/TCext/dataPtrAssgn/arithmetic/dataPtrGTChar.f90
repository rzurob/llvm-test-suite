! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/F2003/dataPtrAssgn/arithmetic/dataPtrGTChar.f
! opt variations: -qnok -ql

!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrGTChar.f 
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
!* - data-target is a dummy arg of type bound function, of type character(*);
!* - data-pointer of type character(:) is type bound function name,
!*               associated with its dummy arg;
!* - the actual arg is refined with the type bound function
!* - test operator .gt.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
        type base(k1)    ! (4)
            integer, kind :: k1
            contains
                procedure, pass :: fun => func
        end type

        contains
	    function func(a, ch)
                character(:), pointer :: func(:)
                class(base(4)), intent(in) :: a
                character(*), target, allocatable :: ch(:)

                if ( .not. allocated(ch)) stop 99

                func(1:size(ch)) => ch(26:1:-1)

                if ( .not. associated(func, ch(26:1:-1))) stop 1
                if ( lbound(func,1) /= 1 ) stop 2
                if ( ubound(func,1) /= 26 ) stop 3

                if ( .not. all(func .ge. (/ ('AA', i=1,26)/) )) stop 5

            end function
end module

    program main

        use m
        type(base(4)) :: b2
        character(2), target, allocatable :: ch(:)

        ch = (/ (repeat(achar(i+64),2), i=1,26) /)

        !print *, b2%fun(ch)
        ch =  b2%fun(ch)
        print *, ch
    end program

