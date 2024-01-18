! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr033a4_1.f
!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fconstr033a4_1.f
! %VERIFY: fconstr033a4_1.out:fconstr033a4_1.vf
! %STDIN:
! %STDOUT: fconstr033a4_1.out
! %EXECARGS:
! %POSTCMD: 
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 10/20/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : structure constructor (unlimited
!                               poly-allocatable array component; use array
!                               section as the data-source; dynamic type is
!                               logical)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program fconstr033a4_1
    class (*), allocatable :: x(:,:)

    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        class (*), allocatable :: data(:)
    end type

    integer u(2), v(1)

    u = (/0, 0/)

    v = (/-1/)

    allocate (x(0:1,-1:0), source=reshape ((/((mod(i,2) == 0), i=1,4)/), &
                            (/2,2/)))

    call foo (base(4,20)(reshape(x(u, v), (/2/))))

    contains

    !! test the structure constructor in associate construct: not any more
!    associate (y => base(4,20)(reshape(x(u, v), (/2/))))

    subroutine foo (y)
        type(base(4,*)), intent(in) :: y

        print *, allocated(y%data), lbound(y%data), ubound(y%data)
    end subroutine
end
