!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2010-09-22
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF SYNC MEMORY
!*  SECONDARY FUNCTIONS TESTED : sync memory in different contexts: main and internal subroutine
!*  REFERENCE                  : Feature Number 351605.25
!*
!*  REQUIRED COMPILER OPTIONS  : -qcaf -qcaf=images=<num>
!*  ADAPTED FROM               : syncMemDefRef
!*
!*  DESCRIPTION
!*
!*  Like syncMemDefRef, but the reference and the sync memory are now in an internal subroutine.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program syncMemDefRefInt

    use, intrinsic :: iso_fortran_env
    implicit none

    integer, parameter :: P = 1, Q = 2
    integer, parameter :: EXPECTED = 3210
    integer, parameter :: IS_LOCKED = -1
    integer, parameter :: IS_UNLOCKED = 0
    logical, parameter :: DEBUG = .false.

    integer(atomic_int_kind), save, volatile :: access_to_var[*] = IS_LOCKED

    integer, save :: iCovar[*] = 0
    integer :: curImage, localCopy, smStatus
    integer :: attemptNumber
    character(100) :: smMsg

    ! Initial housekeeping
    curImage = this_image()
    smStatus = 0
    smMsg = ''

    if (curImage == P) then

      ! Step 1: set Q's iCovar
      iCovar[Q] = EXPECTED
      if(DEBUG) print *, curImage, 'iCovar[', Q, '] set to', EXPECTED

      ! Step 2: synchronize memory
      sync memory (stat=smStatus, errmsg=smMsg)
      if (smStatus/=0) then
         print *, curImage, 'status at step 2(P):', smStatus, '<', trim(smMsg), '>'
         error stop 4
      end if

      ! Step 3: in P: signal Q that the update to iCovar is complete
      access_to_var[Q] = IS_UNLOCKED
      sync memory (stat=smStatus, errmsg=smMsg) ! flush change
      if (smStatus/=0) then
         print *, curImage, 'status in unlock:', smStatus, '<', trim(smMsg), '>'
         error stop 5
      end if
      if(DEBUG) print *, curImage, 'access_to_var[',Q,'] set to', IS_UNLOCKED

    else if (curImage == Q) then

      call doReference

    end if

  contains

    subroutine doReference

      ! Step 1: wait for P's signal
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

      ! Step 2: synchronize memory
      sync memory (stat=smStatus, errmsg=smMsg)
      if (smStatus/=0) then
         print *, curImage, 'status at step 2:', smStatus, '<', trim(smMsg), '>'
         error stop 6
      end if

      ! Step 3: check the value of iCovar
      localCopy = iCovar
      if ( localCopy /= EXPECTED ) then
         print *, curImage, 'variable is', localCopy, 'and not', EXPECTED
         error stop 10
      end if

    end subroutine doReference

end program syncMemDefRefInt
