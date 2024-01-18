!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : syncMemDefQLTP
!*
!*  PROGRAMMER                 : dforster
!*  DATE                       : 2010-09-22
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF SYNC MEMORY
!*  SECONDARY FUNCTIONS TESTED : Define followed by Query of length
!*  REFERENCE                  : Feature Number 351605.25
!*  REQUIRED COMPILER OPTIONS  : -qcaf -qcaf=images=<num>
!*  ADAPTED FROM               : syncMemDefRef
!*
!*  DESCRIPTION
!*
!*  Verify that the statement in image P defining image Q's variable in fact
!*  precedes its query of Q's variable length, once the handshake is done.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program syncMemDefQLTP

    use, intrinsic :: iso_fortran_env
    implicit none

    integer, parameter :: P = 1, Q = 2
    character, parameter :: EXPECTED = 'xyz'
    integer, parameter :: IS_LOCKED = -1
    integer, parameter :: IS_UNLOCKED = 0
    logical, parameter :: DEBUG = .false.
    integer, parameter :: L1 = 3, U1 = 5, L2 = -4, U2 = -1

    integer(atomic_int_kind), save, volatile :: access_to_var[*] = IS_LOCKED

    character(3), save :: covar(L1:U1,L2:U2)[*] = ''
    integer :: curImage, localCopy, smStatus
    integer :: attemptNumber, ltp
    character(100) :: smMsg

    ! Initial housekeeping
    curImage = this_image()
    smStatus = 0
    smMsg = ''

    select case (curImage)
    case (P)

      ! Step 1: covar definition
      covar[P] = EXPECTED
      if (DEBUG) print *, curImage, 'covar[', P, '] set to', EXPECTED

      ! Step 2: synchronize memory
      sync memory (stat=smStatus, errmsg=smMsg)
      if (smStatus/=0) then
         print *, curImage, 'status at step 2(P):', smStatus, '<', trim(smMsg), '>'
         error stop 4
      end if

      ! Step 3: in P: signal Q that action 1 is complete
      access_to_var[P] = IS_UNLOCKED
      sync memory (stat=smStatus, errmsg=smMsg) ! flush change
      if (smStatus/=0) then
         print *, curImage, 'status in unlock:', smStatus, '<', trim(smMsg), '>'
         error stop 5
      end if
      if (DEBUG) print *, curImage, 'access_to_var[',P,'] set to', IS_UNLOCKED

    case (Q)

      ! Step 1: wait for P's signal
      if (DEBUG) print *, curImage, 'waiting for access_to_var'
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

	 print *, attemptNumber
      ! Step 2: synchronize memory
      sync memory (stat=smStatus, errmsg=smMsg)
      if (smStatus/=0) then
         print *, curImage, 'status at step 2:', smStatus, '<', trim(smMsg), '>'
         error stop 6
      end if

      ! Query length type parameter
      ltp = getLTP(covar[P])
      if (ltp /= len(covar[P])) then
        print *, curImage, 'ltp:   ', ltp, '; expecting: ', len(covar[P])
        error stop 10
      end if

    end select

contains

  integer function getLTP(c)
    character(*) :: c(:,:)
    getLTP = len(c)
  end function

end program syncMemDefQLTP
