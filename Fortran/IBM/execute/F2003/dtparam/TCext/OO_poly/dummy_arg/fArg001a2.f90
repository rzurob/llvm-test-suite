! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg001a2.f
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
! %GROUP: fArg001a2.f
! %VERIFY: 
! %STDIN:
! %STDOUT:
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
!*  DATE                       : 04/30/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : argument association (procedure as the actual
!*                               argument)
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

module m
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id = -1
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name = 'no-name'
    end type

    contains

    integer function add (b1, b2)
        type (base(4)), intent(in) :: b1, b2

        add = b1%id + b2%id
    end function

    integer function subtract (b1, b2)
        type (base(4)), intent(in) :: b1, b2

        subtract = b1%id - b2%id
    end function
end module

module m1
use m
    contains

    integer function operateB1B2 (b1, b2, op)
        class (base(4)), intent(in) :: b1, b2

        interface
            integer function op (bb1, bb2)
            use m
                type (base(4)), intent(in) :: bb1, bb2
            end function
        end interface

        operateB1B2 = op (b1, b2)
    end function
end module

program fArg001a2
use m1
    type (base(4)) :: b1
    type (child(4,1,20)) :: c1

    b1%id = 1

    c1 = child(4,1,20) (2, 'c1')

    if (operateB1B2 (child(4,1,20)(1,'test1'), child(4,1,20)(2,'test2'), op = add) /= 3) &
            error stop 1_4


    if (operateB1B2 (b1 = b1, b2 = child(4,1,20)(1,'abc'), op = subtract) /= 0) &
            error stop 2_4


    if (operateB1B2 (b1, b2=c1, op = add) /= 3) error stop 3_4

    if (operateB1B2 (b1, b2=c1%base, op = subtract) /= -1) error stop 4_4
end
