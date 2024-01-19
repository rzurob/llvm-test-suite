!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2010-09-17
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF SYNC MEMORY
!*  SECONDARY FUNCTIONS TESTED : P broadcasts values to all images, syncs, then each checks itself
!*  REFERENCE                  : Feature Number 351605.25
!*  REQUIRED COMPILER OPTIONS  : -qcaf -qcaf=images=<num>
!*  ADAPTED FROM               : syncMemDefRefOther
!*
!*  DESCRIPTION
!*
!*  Image P updates the coarray variable in every other image (including itself),
!*  syncs with all other images, and then each image checks the value it was given.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program syncMemDefRefBroadcast

    use, intrinsic :: iso_fortran_env
    implicit none

    integer, parameter :: P = 1
    integer, parameter :: EXPECTED = 3210
    integer, parameter :: IS_LOCKED = -1
    integer, parameter :: IS_UNLOCKED = 0
    logical, parameter :: DEBUG = .false.

    integer(atomic_int_kind), save, volatile :: access_to_var[*] = IS_LOCKED

    integer, save :: iCovar[*] = 0
    integer :: curImage, nImages, localCopy, smStatus, i
    integer :: attemptNumber
    character(100) :: smMsg

    ! Initial housekeeping
    nImages = num_images()
    curImage = this_image()
    smStatus = 0
    smMsg = ''


    if (curImage == P) then

      ! Step 1: set iCovar in every image (including our own)
      do i = 1, nImages
         iCovar[i] = i
         if(DEBUG) print *, curImage, 'iCovar[', i, '] set to', i
      end do

      ! Step 2: synchronize memory
      sync memory (stat=smStatus, errmsg=smMsg)
      if (smStatus/=0) then
         print *, curImage, 'status at step 2(P):', smStatus, '<', trim(smMsg), '>'
         error stop 4
      end if

      ! Step 3: signal each image that the update to its variable is complete
      do i = 1, nImages
         access_to_var[i] = IS_UNLOCKED
         if(DEBUG) print *, curImage, 'access_to_var[',i,'] set to', IS_UNLOCKED
      end do

      sync memory (stat=smStatus, errmsg=smMsg) ! flush change
      if (smStatus/=0) then
         print *, curImage, 'status in unlock:', smStatus, '<', trim(smMsg), '>'
         error stop 5
      end if

      ! We don't need to sync with ourselves here - just drop through to check the value

    else

      ! Step 1: wait for P's signal
      if(DEBUG) print *, curImage, 'waiting for access_to_var'
      attemptNumber = 1
      do while (access_to_var == IS_LOCKED)
         call sleep_(3)
!         if (attemptNumber > 100000000) then
         if (attemptNumber > 1000) then
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

    end if

    ! Step 2: synchronize memory
    sync memory (stat=smStatus, errmsg=smMsg)
    if (smStatus/=0) then
       print *, curImage, 'status at step 2:', smStatus, '<', trim(smMsg), '>'
       error stop 6
    end if

    ! Step 3: check the value of iCovar
    localCopy = iCovar
    if ( localCopy /= curImage ) then
       print *, curImage, 'variable is', localCopy, 'and not', curImage
       error stop 10
    end if

end program syncMemDefRefBroadcast
