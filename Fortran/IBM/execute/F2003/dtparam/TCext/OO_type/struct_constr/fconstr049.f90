! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr049.f
! with manual adjustment (line 97: base(1,2) is a function call to makeBase,
! so it's not parameterized)
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
! %GROUP: fconstr049.f
! %VERIFY: fconstr049.out:fconstr049.vf
! %STDIN:
! %STDOUT: fconstr049.out
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
!*  DATE                       : 4/20/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : structure constructor (structure constructor
!*                               can be hidden/overridden by generics)
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
    type base(k1,k2)    ! (4,4)
        integer, kind                     :: k1,k2
        integer(k1)                       :: id
        integer(k2), allocatable, private :: data

        contains

        procedure :: print => printBase
    end type

    interface base
        function makeBase (i1, i2)
        import base
            type (base(4,4)) :: makeBase
        end function
    end interface

    contains

    subroutine printBase (b)
        class (base(4,4)), intent(in) :: b

        if (allocated (b%data)) then
            print *, b%id, b%data
        else
            print *, b%id
        end if
    end subroutine

    subroutine assgnID (b1, i1)
        class (base(4,4)), intent(inout) :: b1
        integer*4, intent(in) :: i1

        if (.not. allocated(b1%data))   allocate(b1%data)

        b1%data = i1
    end subroutine
end module

program fconstr049
use m
    type (base(4,4)) :: b1

    b1 = base(1, 2)

    call b1%print

end

function makeBase (i1, i2)
use m, only:base, assgnID
    type (base(4,4)) :: makeBase

    call assgnID (makeBase, i2)

    makeBase%id = i1
end function
