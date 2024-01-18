! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/func_return/ffuncRet500.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/21/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : function return (IMPORT statement usage)
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
        integer(k1)   :: id

        contains

        final :: finalizeBase
    end type

    interface makeData
        type (base(4)) function makeBaseObj (i)
        import base
            integer*4, intent(in) :: i
        end function
    end interface

    contains

    subroutine finalizeBase (b)
        type (base(4)) :: b

        print *, 'finalizeBase'
    end subroutine
end module

program ffuncRet500
use m
    print *, makeData (10)
    print *, 'end'
end

function makeBaseObj (i)
use m, only : base
    implicit type (base(4)) (m)
    integer*4, intent(in) :: i

    makeBaseObj%id = i
end function

