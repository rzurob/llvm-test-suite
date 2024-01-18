!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : saErrmsgNonChard
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2010-07-26
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF SYNC ALL
!*  SECONDARY FUNCTIONS TESTED : diagnostic message on non-character or array errmsg-variable
!*  REFERENCE                  : Feature Number 379697
!*  REQUIRED COMPILER OPTIONS  : -qcaf -qcaf=images=<num>
!*  ADAPTED FROM               : saBasic ()
!*
!*  DESCRIPTION
!*
!*  SYNC ALL requires an errmsg-variable which is a scalar-default-char-variable.
!*  Here we pass it several different incorrect types of variables and expect
!*  diagnostic error messages.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program saErrmsgNonChard

   implicit none

   type dt
      character(80) :: cerrmsg
   end type dt
   real(4)      :: rerrmsg
   logical(4)   :: lerrmsg
   complex(4)   :: zerrmsg
   type(dt)     :: derrmsg
   integer(4)   :: ierrmsg
   character(80):: aerrmsg(1)

   sync all (errmsg=rerrmsg)
   sync all (errmsg=lerrmsg)
   sync all (errmsg=zerrmsg)
   sync all (errmsg=derrmsg)
   sync all (errmsg=ierrmsg)
   sync all (errmsg=aerrmsg)
   sync all (errmsg=derrmsg%cerrmsg) ! this one is the only acceptable one

end program saErrmsgNonChard
