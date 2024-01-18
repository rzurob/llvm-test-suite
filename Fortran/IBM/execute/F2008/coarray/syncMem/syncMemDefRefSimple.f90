!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : syncMemDefRefSimple
!*
!*  PROGRAMMER                 : dforster
!*  DATE                       : 2010-09-17
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF SYNC MEMORY
!*  SECONDARY FUNCTIONS TESTED : simple remote definition and reference
!*  REFERENCE                  : Feature Number 351605.25
!*  REQUIRED COMPILER OPTIONS  : -qcaf -qcaf=images=<num>
!*
!*  DESCRIPTION
!*
!*  Simple case of P defining a covar in Q and then checking it.
!*
!*  Warning: (hardly needed) Do not pass command line arguments to this program
!*  (at least not more than 10) -- we abusing command_argument_count to prevent
!*  the optimiser from doing away with the coarray variable definition and reference.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program syncMemDefRefSimple
    use, intrinsic :: iso_fortran_env
    implicit none

    integer, parameter :: P = 1, Q = 2
    real(8), parameter :: EXPECTED = -8.765432D-10
    logical, parameter :: DEBUG = .false.

    real(8), save :: covar[*] = 0
    logical :: okay
    real(8) :: localCopy
    integer :: curImage, smStatus
    character(100) :: smMsg
    integer :: command_argument_count

    interface
      logical(4) function precision_r8(value,exp)
        real(8) :: value,exp
      end function precision_r8
    end interface

    ! Initial housekeeping
    curImage = this_image()
    smStatus = 0
    smMsg = ''

    if (curImage == P) then

      ! Step 1: define variable (+report)
      ! silly way to keep the optimiser guessing, so it doesn't optimise out the def+ref
      if (command_argument_count() <= 10) then
         covar[Q] = EXPECTED
         if(DEBUG) print *, curImage, 'covar[', Q, '] set to', EXPECTED
      end if

      ! Step 2: synchronize memory
      sync memory (stat=smStatus, errmsg=smMsg)
      if (smStatus/=0) then
         print *, curImage, 'status at step 2(P):', smStatus, '<', trim(smMsg), '>'
         error stop 4
      end if

      ! Step 3: in P: reference variable in Q
      localCopy = covar[Q]
      okay = precision_r8(localCopy, EXPECTED)
      if(DEBUG) print *, curImage, 'variable is', localCopy, merge('==','/=',okay), EXPECTED
      if( .not.okay ) then
         error stop 10
      end if

    else

      ! no step 1

      ! Step 2: synchronize memory
      sync memory (stat=smStatus, errmsg=smMsg)
      if (smStatus/=0) then
         print *, curImage, 'status at step 2:', smStatus, '<', trim(smMsg), '>'
         error stop 6
      end if

      ! no step 3, just end (we don't actually even care if we see the change in covar here)
      if(DEBUG) print *, curImage, 'ending'

    end if

end program syncMemDefRefSimple
