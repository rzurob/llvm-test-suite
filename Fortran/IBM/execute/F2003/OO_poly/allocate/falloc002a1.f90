!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: falloc002a1.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/27/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (assumed-character-length in type-spec
!                               in ALLOCATE-stmt)
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

program falloc002a1
    interface
        subroutine allocate(c1, c2, c3)
            character(*), intent(out), pointer :: c1, c3(:)
            character(*), intent(out), allocatable :: c2
        end subroutine
    end interface

    character (10), pointer :: ch1
    character (8), pointer :: ch3(:)
    character (15), allocatable :: ch2

    call  allocate (ch1, ch2, ch3)

    if (ch1 /= 'xlftest   ') error stop 10_4
    if (ch2 /= 'team xlftest   ') error stop 11_4
    if (ch3(2) /= 'team    ') error stop 12_4
    if (ch3(3) /= 'xlftest ') error stop 12_4
end

subroutine allocate(c1, c2, c3)
    character(*), intent(out), pointer :: c1, c3(:)
    character(*), intent(out), allocatable :: c2

    allocate (character(*) :: c1, c2, c3(2:3))

    c1 = 'xlftest'
    c2 = 'team xlftest'

    c3(2) = 'team'
    c3(3) = c1

    if (len(c1) /= 10) error stop 1_4
    if (len(c2) /= 15) error stop 2_4
    if (len(c3) /= 8) error stop 3_4
end subroutine
