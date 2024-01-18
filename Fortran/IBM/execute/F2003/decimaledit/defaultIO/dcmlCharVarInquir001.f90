!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 06/08/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               R930: in INQUIRE statement DECIMAL=
!                               scalar-default-char-variable.
!                               Test the use of deferred char variable for
!                               decimal= in inquire stmt.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program dcmlCharVarInquir001
    abstract interface
        subroutine inquireSub (unit, c)
            integer, intent(in) :: unit
            character(:), pointer, intent(in) :: c
        end subroutine

        subroutine inquireSub2 (unit, x)
            integer, intent(in) :: unit
            class(*) :: x
        end subroutine
    end interface

    procedure(inquireSub) getDecMode
    procedure(inquireSub2) getDecMode2

    character(:), pointer :: modePtr, modePtr2
    character(:), allocatable :: modeAlloc
    character (5), target :: mode(2)

    allocate (modePtr2, source='initialize mode to be NONE')
    allocate (modeAlloc, source=modePtr2)

    modePtr => mode(1)

    open (10, file='abc')

    open (11, access='stream', form='formatted', decimal='COMMA')

    open (100, access='direct', recl=1000)

    write (10, decimal='COMMA', fmt='(DC, f10.2)') 10.2

    write (11, '(DP, ES10.2)', DECIMAL="POINT") 7.1

    write (100, rec=10) (i, i=1,10)

    open (11, decimal='COMMA')

    call getDecMode (10, modePtr)

    modePtr => mode(2)

    call getDecMode (12, modePtr)

    call getDecMode (11, modePtr2)

    call getDecMode2 (100, modeAlloc)

    !! verify
    if (mode(1)/= 'POINT') error stop 1_4

    if (mode(2) /= 'UNDEF') error stop 2_4

    if (modePtr2 /= 'COMMA') error stop 3_4

    if (modeAlloc /= 'UNDEFINED') error stop 4_4
end

subroutine getDecMode (unit, c)
    integer, intent(in) :: unit
    character(:), pointer, intent(in) :: c

    if (associated (c)) inquire(unit, decimal=c)
end subroutine


subroutine getDecMode2 (unit, x)
    integer, intent(in) :: unit
    class(*) :: x

    select type (x)
        type is (character(*))
            inquire (unit, decimal=x)

        class default
            stop 10
    end select
end subroutine
