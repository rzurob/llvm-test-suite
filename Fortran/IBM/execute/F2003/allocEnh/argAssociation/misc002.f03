! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/3/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               self-assignment via pointer for deferred
!                               character type.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

    character(:), pointer :: c1
    character(:), allocatable, target :: c2

    c2 = 'xlftest 101'

    c1 => c2(1:7)

    if (c1 /= 'xlftest') error stop 1_4

    c2 = c1

    if (c2 /= 'xlftest') error stop 2_4
    end