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
! %GROUP: fArg601.f
! %VERIFY: fArg601.out:fArg601.vf
! %STDIN:
! %STDOUT: fArg601.out
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
!*  DATE                       : 12/02/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : argument association (use of select type for
!                               unlimited poly dummy-arg)
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

program fArg601
    type base
        integer(4) :: id
    end type

    type, extends(base) :: child
        character (15) :: name
    end type

    type (base) :: b1 = base (1)
    class (base), pointer :: b2(:)

    integer(4) :: i

    i = 10

    allocate (b2(0:3), source=(/(child(ii, 'xlftest 101'), ii=0,3)/))

    call abc (i)
    call abc (b1)

    call abc (b2(2))

    contains

    subroutine abc (x)
        class(*), intent(in) :: x

        !! select type construct should be used here

        select type (x)
            class is (base)
                write (*, '(i5)', advance='no') x%id

                select type (x)
                    type is (base)

                    type is (child)
                        write (*, '(a,a)', advance='no') ', ', x%name
                    class default
                        error stop 1_4
                end select

                print *, ''
            type is (integer(4))
                print *, x
            class default
                print *, 'other type'
        end select
    end subroutine
end
