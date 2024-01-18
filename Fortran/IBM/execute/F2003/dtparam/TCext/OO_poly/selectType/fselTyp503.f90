! GB DTP extension using:
! ftcx_dtp -qck -ql -qdeferredlp /tstdev/OO_poly/selectType/fselTyp503.f
! opt variations: -qnock -qnol -qnodeferredlp

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
! %GROUP: fselTyp503.f
! %VERIFY: fselTyp503.out:fselTyp503.vf
! %STDIN:
! %STDOUT: fselTyp503.out
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
!*  DATE                       : 10/25/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : select type (poly function return as selector)
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
    type base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      id
    end type

    type, extends(base) :: child(k2,n2)    ! (20,4,1,15)
        integer, kind             :: k2
        integer, len              :: n2
        character(kind=k2,len=n2) :: name
    end type

    interface
        class (base(:,4)) function genData (id, name)
        import base
            integer(4),intent(in) :: id
            character(*), intent(in), optional :: name
            allocatable genData
        end function
    end interface
end module


program fselType503
use m
    select type (x => genData(100, 'x'))
        type is (base(*,4))
            print *, 'base', x
        type is (child(*,4,1,*))
            print *, 'child', x
    end select
end


class (base(:,4)) function genData (id, name)
use m, only : base, child
    integer(4),intent(in) :: id
    character(*), intent(in), optional :: name
    allocatable genData

    if (present(name)) then
        allocate (genData, source=child(20,4,1,15)(id,name))
    else
        allocate (genData, source=base(20,4)(id))
    end if
end function
