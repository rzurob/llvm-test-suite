!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : saSyncStatd
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2010-07-26
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF SYNC ALL
!*  SECONDARY FUNCTIONS TESTED : diagnostic message on incorrect sync-stat entry
!*  REFERENCE                  : Feature Number 379697
!*  REQUIRED COMPILER OPTIONS  : -qcaf -qcaf=images=<num>
!*  ADAPTED FROM               : saBasic ()
!*
!*  DESCRIPTION
!*
!*  SYNC ALL allows only two sync-stat options: STAT and ERRMSG.
!*  Here we pass it an incorrect (though not unlikely) option instead: STATUS,
!*  and expect a diagnostic error message.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program saSyncStatd

   implicit none
   integer(4) :: stat

   sync all (status=stat)

end program saSyncStatd
