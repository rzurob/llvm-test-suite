!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2010-09-07
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF SYNC MEMORY
!*  SECONDARY FUNCTIONS TESTED : simple test of definition preceding reference
!*  REFERENCE                  : Feature Number 351605.25
!*  REQUIRED COMPILER OPTIONS  : -qcaf -qcaf=images=<num>
!*
!*  DESCRIPTION
!*
!*  Verify that the statement in image P defining image Q's variable in fact
!*  precedes image Q's examination of it, once the handshake is done.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program syncMemDefRefInterleaved

    use, intrinsic :: iso_fortran_env
    implicit none

    integer, parameter :: P = 1, Q = 2
    integer, parameter :: EXPECTED = 3210
    integer, parameter :: IS_LOCKED = -1
    integer, parameter :: IS_UNLOCKED = 0
    logical, parameter :: DEBUG = .false.

    integer(ATOMIC_INT_KIND), save, volatile :: access_to_var[*] = IS_LOCKED

    integer, save :: iCovar[*] = 0
    integer :: curImage, localCopy, smStatus
    integer :: attemptNumber
    character(100) :: smMsg

    ! Initial housekeeping
    curImage = this_image()
    smStatus = 0
    smMsg = ''

    ! Step 1: in P: set Q's iCovar
    !         in Q: wait for P's signal

    select case (curImage)
    case (P)
      iCovar[Q] = EXPECTED
      if(DEBUG) print *, curImage, 'iCovar[', Q, '] set to', EXPECTED

    case (Q)
      if(DEBUG) print *, curImage, 'waiting for access_to_var'
      attemptNumber = 1
      do while (access_to_var == IS_LOCKED)
         if (attemptNumber > 100000000) then
            print *, curImage, 'too many tries. Aborting.'
            error stop 2
         end if
         ! ensure latest values for next time around:
         sync memory  (stat=smStatus, errmsg=smMsg)
         if (smStatus/=0) then
            print *, curImage, 'status in wait:', smStatus, '<', trim(smMsg), '>'
            error stop 3
         end if
         attemptNumber = attemptNumber + 1
      end do
    end select

    ! Step 2: synchronize memory
    sync memory (stat=smStatus, errmsg=smMsg)
    if (smStatus/=0) then
        print *, curImage, 'status at step 2:', smStatus, '<', trim(smMsg), '>'
        error stop 4
    end if

    ! Step 3: in P: signal Q that the update to iCovar is complete
    !         in Q: check the value of iCovar
    select case (curImage)
    case (P)
      access_to_var[Q] = IS_UNLOCKED
      sync memory (stat=smStatus, errmsg=smMsg) ! flush change
      if (smStatus/=0) then
         print *, curImage, 'status in unlock:', smStatus, '<', trim(smMsg), '>'
         error stop 5
      end if
      if(DEBUG) print *, curImage, 'access_to_var[',Q,'] set to', IS_UNLOCKED
    case (Q)
      localCopy = iCovar
      if ( localCopy /= EXPECTED ) then
         print *, curImage, 'variable is', localCopy, 'and not', EXPECTED
         error stop 10
      end if
    end select

end program syncMemDefRefInterleaved
