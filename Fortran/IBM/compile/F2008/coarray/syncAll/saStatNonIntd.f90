!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : saStatNonIntd
!*
!*  DATE                       : 2010-07-26
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF SYNC ALL
!*  SECONDARY FUNCTIONS TESTED : diagnostic message on non-integer or array stat-variable
!*  REFERENCE                  : Feature Number 379697
!*  REQUIRED COMPILER OPTIONS  : -qcaf -qcaf=images=<num>
!*  ADAPTED FROM               : saBasic ()
!*
!*  DESCRIPTION
!*
!*  SYNC ALL requires a stat-variable which is a scalar integer.
!*  Here we pass it several different incorrect types of variables and expect
!*  diagnostic error messages.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program saStatNonIntd

   implicit none

   type dt
      integer :: istat
   end type dt
   real(4)      :: rstat
   character(4) :: cstat
   logical(4)   :: lstat
   complex(4)   :: zstat
   type(dt)     :: dstat
   integer(4)   :: astat(1)

   sync all (stat=rstat)
   sync all (stat=cstat)
   sync all (stat=lstat)
   sync all (stat=zstat)
   sync all (stat=dstat)
   sync all (stat=astat)
   sync all (stat=dstat%istat) ! this one is the only acceptable one

end program saStatNonIntd
