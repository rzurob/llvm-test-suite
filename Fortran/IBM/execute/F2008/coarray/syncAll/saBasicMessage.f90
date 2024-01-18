!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2010-07-26
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF SYNC ALL
!*  SECONDARY FUNCTIONS TESTED : errmsg variable is not altered on no error
!*  REFERENCE                  : Feature Number 379697
!*  REQUIRED COMPILER OPTIONS  : -qcaf -qcaf=images=<num>
!*
!*  DESCRIPTION
!*
!*  Verify that the errmsg variable remains unchanged on successful completion of SYN ALL.
!*  We set the variable to all Z's, use it in a SYNC ALL, and test it afterwards.
!*  Also, to test the coexistence of SYNC ALL statements with and without sync-stat options,
!*  we include a second SYNC ALL statement.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program saBasicMessage

   implicit none

   character(100), parameter :: fill = repeat('Z',len(fill))
   character(100) :: emsg

   emsg = fill

   sync all (errmsg=emsg)

   if (emsg /= fill) stop 2

   sync all

end program saBasicMessage
