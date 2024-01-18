! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=none /tstdev/OO_poly/selectType/fselTyp508.f
! opt variations: -qck -qnok -qnol -qnodeferredlp -qreuse=base

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
! %GROUP: fselTyp508.f
! %VERIFY: fselTyp508.out:fselTyp508.vf
! %STDIN:
! %STDOUT: fselTyp508.out
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
!*  DATE                       : 12/13/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : select type (select type construct in function
!                               definition)
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

    type, extends(base) :: child(k2,n2)    ! (20,4,4,20)
        integer, kind :: k2
        integer, len  :: n2
        character(n2)    name
    end type

    contains

    class (base(20,4)) function rep (b)
        class(base(*,4)), intent(in) :: b
        allocatable :: rep

        select type (b)
            type is (base(*,4))
                allocate (rep)
                rep%id = b%id
            type is (child(*,4,4,*))
                allocate (child(20,4,4,20):: rep)

                select type (rep)
                    type is (child(*,4,4,*))

                    rep%id = - b%id
                    rep%name = b%name
                end select
        end select
    end function
end module

program fselTyp508
use m
    class (base(:,4)), allocatable :: b1

    associate (x => rep (base(20,4)(100)))
        select type (y => x)
            type is (base(*,4))
                print *, y
            class default
                error stop 1_4
        end select
    end associate

    associate (x => rep(child(20,4,4,20) (10, 'xlftest team')))
        select type (y => x)
            type is (child(*,4,4,*))
                print *, y
            class default
                error stop 2_4
        end select
    end associate

    allocate (b1, source=child(20,4,4,20) (-1, 'OO team'))

    associate (x => rep (b1))
        select type (y => x)
            type is (child(*,4,4,*))
                print *, y
            class default
                error stop 3_4
        end select
    end associate
end
