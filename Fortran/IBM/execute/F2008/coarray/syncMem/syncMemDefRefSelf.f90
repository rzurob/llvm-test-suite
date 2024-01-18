!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : syncMemDefRefSelf
!*
!*  PROGRAMMER                 : dforster
!*  DATE                       : 2010-09-17
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF SYNC MEMORY
!*  SECONDARY FUNCTIONS TESTED : each image storing into its own coarray slice
!*  REFERENCE                  : Feature Number 351605.25
!*  REQUIRED COMPILER OPTIONS  : -qcaf -qcaf=images=<num>
!*  ADAPTED FROM               : syncMemDefRefSimple
!*
!*  DESCRIPTION
!*
!*  Simplest case: each image defines and then references its own covar.
!*
!*  Warning: (hardly needed) Do not pass command line arguments to this program
!*  (at least not more than 10) -- we abusing command_argument_count to prevent
!*  the optimiser from doing away with the coarray variable definition and reference.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program syncMemDefRefSelf
    use, intrinsic :: iso_fortran_env
    implicit none

    integer, parameter :: P = 1, Q = 2
    logical, parameter :: DEBUG = .false.

    complex(8), save :: covar[*] = 0.0
    logical :: okay
    complex(8) :: localCopy, expected
    integer :: curImage, smStatus
    character(100) :: smMsg

    interface
      logical(4) function precision_x16(value,exp)
        complex(8) :: value,exp
      end function precision_x16
    end interface

    ! Initial housekeeping
    curImage = this_image()
    smStatus = 0
    smMsg = ''

    ! Step 1: define
    ! silly way to keep the optimiser guessing, so it doesn't optimise out the def+ref
    if (command_argument_count() <= 10) then
      covar = (sqrt(real(curImage,8)), 1.0d0/curImage)
      if(DEBUG) print *, curImage, 'covar set to', curImage
    end if

    ! Step 2: synchronize memory
    sync memory (stat=smStatus, errmsg=smMsg)
    if (smStatus/=0) then
       print *, curImage, 'status at step 2(P):', smStatus, '<', trim(smMsg), '>'
       error stop 4
    end if

    ! Step 3: reference
    localCopy = covar
    expected = (sqrt(real(curImage,8)), 1.0d0/curImage)
    okay = precision_x16(expected, localCopy)
    if(DEBUG) print *, curImage, 'variable is', localCopy, merge('==','/=',okay), expected
    if (.not.okay) error stop 10

end program syncMemDefRefSelf
