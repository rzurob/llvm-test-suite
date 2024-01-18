!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : syncMemRefDef
!*
!*  DATE                       : 2010-09-22
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF SYNC MEMORY
!*  SECONDARY FUNCTIONS TESTED : coarray variable reference precedes (re)definition
!*  REFERENCE                  : Feature Number 351605.25
!*  REQUIRED COMPILER OPTIONS  : -qcaf -qcaf=images=<num>
!*  ADAPTED FROM               : syncMemDefRef
!*
!*  DESCRIPTION
!*
!*  Like syncMemDefRef, but we swap the reference and define operations, which
!*  gets tricky: we need to make sure the comparison is between the reference
!*  and the subsequent define, so the reference needs to be stored locally and
!*  then sent across to be checked, but only after the define (!).
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program syncMemRefDef

    use, intrinsic :: iso_fortran_env
    implicit none

    integer, parameter :: P = 1, Q = 2
    integer, parameter :: EXPECTED1 = 876543210, EXPECTED2 = 123456789, EXPECTED3 = 543212345
    integer, parameter :: IS_LOCKED = -1
    integer, parameter :: IS_UNLOCKED = 0
    logical, parameter :: DEBUG = .false.

    integer(atomic_int_kind), save, volatile :: access_to_var(2)[*] = IS_LOCKED

    integer, save :: iCovar(2)[*] = [EXPECTED1, EXPECTED2]
    integer :: curImage, localCopy(2), smStatus, firstCopy
    integer :: attemptNumber
    character(100) :: smMsg

    ! Initial housekeeping
    curImage = this_image()
    smStatus = 0
    smMsg = ''

    select case (curImage)
    case (P)

      ! get Q's iCovar - we grab the value from (1) to later stash it in (2)
      firstCopy = iCovar(1)[Q]
      if(DEBUG) print *, curImage, 'iCovar(1)[', Q, '] is', firstCopy
      if (firstCopy /= EXPECTED1) then
        print *, curImage, 'iCovar(1)[', Q, '] /=', EXPECTED1
        error stop 7
      end if

      ! synchronize memory
      sync memory (stat=smStatus, errmsg=smMsg)
      if (smStatus/=0) then
         print *, curImage, 'status at step 2(P):', smStatus, '<', trim(smMsg), '>'
         error stop 4
      end if

      ! signal Q that the reference is complete
      access_to_var(1)[Q] = IS_UNLOCKED
      sync memory (stat=smStatus, errmsg=smMsg) ! flush change
      if (smStatus/=0) then
         print *, curImage, 'status in unlock:', smStatus, '<', trim(smMsg), '>'
         error stop 5
      end if
      if(DEBUG) print *, curImage, 'access_to_var(1)[',Q,'] set to', IS_UNLOCKED

      ! now copy our stashed value to Q's iCovar(2)
      iCovar(2)[Q] = firstCopy
      sync memory (stat=smStatus, errmsg=smMsg) ! flush change
      if (smStatus/=0) then
         print *, curImage, 'status in define:', smStatus, '<', trim(smMsg), '>'
         error stop 8
      end if
      if(DEBUG) print *, curImage, 'iCovar(2)[',Q,'] set to', firstCopy

      ! signal Q that the copy can now be accessed
      access_to_var(2)[Q] = IS_UNLOCKED
      sync memory (stat=smStatus, errmsg=smMsg) ! flush change
      if (smStatus/=0) then
         print *, curImage, 'status in unlock(2):', smStatus, '<', trim(smMsg), '>'
         error stop 5
      end if
      if(DEBUG) print *, curImage, 'access_to_var(2)[',Q,'] set to', IS_UNLOCKED

    case (Q)

      ! wait for P's signal that reference has been made
      if(DEBUG) print *, curImage, 'waiting for access_to_var(1)'
      attemptNumber = 1
      do while (access_to_var(1) == IS_LOCKED)
         if (attemptNumber > 100000000) then
            print *, curImage, 'too many tries. Aborting.'
            error stop 2
         end if
         ! ensure latest values for next time around:
         sync memory  (stat=smStatus, errmsg=smMsg)
         if (smStatus/=0) then
            print *, curImage, 'status in wait(1):', smStatus, '<', trim(smMsg), '>'
            error stop 3
         end if
         attemptNumber = attemptNumber + 1
      end do

      ! synchronize memory
      sync memory (stat=smStatus, errmsg=smMsg)
      if (smStatus/=0) then
         print *, curImage, 'status at sync(Q):', smStatus, '<', trim(smMsg), '>'
         error stop 6
      end if

      ! alter value of iCovar(1) and synchronize memory
      iCovar(1) = EXPECTED3
      sync memory (stat=smStatus, errmsg=smMsg)
      if (smStatus/=0) then
         print *, curImage, 'status at sync 2(Q):', smStatus, '<', trim(smMsg), '>'
         error stop 8
      end if

      ! wait for P's signal that it has delivered its stashed copy to us
      if(DEBUG) print *, curImage, 'waiting for access_to_var(2)'
      attemptNumber = 1
      do while (access_to_var(2) == IS_LOCKED)
         if (attemptNumber > 100000000) then
            print *, curImage, 'too many tries. Aborting.'
            error stop 2
         end if
         ! ensure latest values for next time around:
         sync memory  (stat=smStatus, errmsg=smMsg)
         if (smStatus/=0) then
            print *, curImage, 'status in wait(2):', smStatus, '<', trim(smMsg), '>'
            error stop 3
         end if
         attemptNumber = attemptNumber + 1
      end do

      sync memory (stat=smStatus, errmsg=smMsg)
      if (smStatus/=0) then
         print *, curImage, 'status at sync 3(Q):', smStatus, '<', trim(smMsg), '>'
         error stop 9
      end if

      ! now the two iCovar's should be [EXPECTED3, EXPECTED1]
      localCopy = iCovar
      if ( any(localCopy /= [EXPECTED3, EXPECTED1]) ) then
         print *, curImage, 'variable is', localCopy, 'and not', [EXPECTED3, EXPECTED1]
         error stop 10
      end if

    end select

end program syncMemRefDef
