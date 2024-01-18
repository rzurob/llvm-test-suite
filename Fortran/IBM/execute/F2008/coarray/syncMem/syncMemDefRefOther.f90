!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : syncMemDefRefOther
!*
!*  DATE                       : 2010-09-17
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF SYNC MEMORY
!*  SECONDARY FUNCTIONS TESTED : P defines covar in Q, R checks it
!*  REFERENCE                  : Feature Number 351605.25
!*  REQUIRED COMPILER OPTIONS  : -qcaf -qcaf=images=<num>
!*  ADAPTED FROM               : syncMemDefRef
!*
!*  DESCRIPTION
!*
!*  Image P defines a coarray variable in Q, synchronizes, and then lets R check
!*  the update.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program syncMemDefRefOther

    use, intrinsic :: iso_fortran_env
    implicit none

    integer, parameter :: P = 1, Q = 2, R = 3
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


    select case (curImage)
    case (P)

      ! Step 1: set Q's iCovar
      iCovar[Q] = EXPECTED
      if(DEBUG) print *, curImage, 'iCovar[', Q, '] set to', EXPECTED

      ! Step 2: synchronize memory
      sync memory (stat=smStatus, errmsg=smMsg)
      if (smStatus/=0) then
         print *, curImage, 'status at step 2(P):', smStatus, '<', trim(smMsg), '>'
         error stop 4
      end if

      ! Step 3: in P: signal R that the update to iCovar[Q] is complete
      access_to_var[R] = IS_UNLOCKED
      sync memory (stat=smStatus, errmsg=smMsg) ! flush change
      if (smStatus/=0) then
         print *, curImage, 'status in unlock:', smStatus, '<', trim(smMsg), '>'
         error stop 5
      end if
      if(DEBUG) print *, curImage, 'access_to_var[',R,'] set to', IS_UNLOCKED

    case (Q)
      ! Step 1: do nothing
      ! Step 2: synchronize memory
      sync memory (stat=smStatus, errmsg=smMsg) ! flush change
      if (smStatus/=0) then
         print *, curImage, 'status at step 2(Q):', smStatus, '<', trim(smMsg), '>'
         error stop 7
      end if
      ! Step 3: do nothing

    case (R)

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
      localCopy = iCovar[Q]
      if ( localCopy /= EXPECTED ) then
         print *, curImage, 'variable is', localCopy, 'and not', EXPECTED
         error stop 10
      end if

    end select

end program syncMemDefRefOther
