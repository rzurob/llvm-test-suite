!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : syncMemDefRefPickup
!*
!*  DATE                       : 2010-09-20
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF SYNC MEMORY
!*  SECONDARY FUNCTIONS TESTED : many-to-one access
!*  REFERENCE                  : Feature Number 351605.25
!*  REQUIRED COMPILER OPTIONS  : -qcaf -qcaf=images=<num>
!*  ADAPTED FROM               : syncMemDefRefBroadcast
!*
!*  DESCRIPTION
!*
!*  Like syncMemDefRefBroadcast, this case tests communication from one image
!*  to all other imaes, but whereas "broadcast" has one image send data *to*
!*  all other images by updating the coarray for each image (i.e., varying the
!*  coindex over all images), "pickup" expects each image to get data *from*
!*  the one image.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program syncMemDefRefPickup

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

      ! Step 1: set iCovar in our image
      iCovar = EXPECTED
      if(DEBUG) print *, curImage, 'iCovar set to', EXPECTED

      ! Step 2: synchronize memory
      sync memory (stat=smStatus, errmsg=smMsg)
      if (smStatus/=0) then
         print *, curImage, 'status at step 2(P):', smStatus, '<', trim(smMsg), '>'
         error stop 4
      end if

      ! Step 3: signal that the update is complete
      access_to_var = IS_UNLOCKED
      if(DEBUG) print *, curImage, 'access_to_var set to', IS_UNLOCKED

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
      do while (access_to_var[P] == IS_LOCKED)
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

    end if

    ! Step 2: synchronize memory
    sync memory (stat=smStatus, errmsg=smMsg)
    if (smStatus/=0) then
       print *, curImage, 'status at step 2:', smStatus, '<', trim(smMsg), '>'
       error stop 6
    end if

    ! Step 3: check the value of iCovar
    localCopy = iCovar[P]
    if ( localCopy /= EXPECTED ) then
       print *, curImage, 'variable is', localCopy, 'and not', EXPECTED
       error stop 10
    end if

end program syncMemDefRefPickup
