!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : acetint33d
!*
!*  DATE                       : 2006-09-27
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : intrinsic TS: ASSOCIATE selector and implied-do
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Use AC in an associate construct, verifying that interactions between
!*  associate names, intrinsics, type names, and names of variables in
!*  implied-do's are correct.  Among other things, this means that associate
!*  names cannot be used as ac-do-variables in the body of that associate
!*  construct, and that they trump intrinsics in that context, also.
!*  Associate names can be used as data objects within an AC, however.
!*  Most importantly, however, type specifiers in ACs are not confused by
!*  associate names identical to intrinsic or derived types.
!*
!*  Similarly named tests exist in the intrinsic, derived, and "none" types
!*  sub-buckets, as well as in their diagnostic counterparts.
!*
!*  Here we test the use of an ac-do-variable with the same name as a selector in an
!*  AC-implied-do in an embedded associate construct - this should be illegal.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetint33d

  implicit none
  integer :: logical, i, other(2)

  other = [integer:: 3,4]

  associate(logical => [logical:: .true., .false.])

     ! this is okay
     associate(log => [logical:: (logical(i), i=2,1,-1)])
        print *, log, logical
     end associate

     ! this is not - selectors cannot be assigned to
     associate(log => [logical:: (other(logical), logical=2,1,-1)])
        print *, log, logical
     end associate

     ! this is also bad, but it's the conflict with the logical intrinsic at fault
     associate(log => [logical:: (logical(logical), logical=2,1,-1)])
        print *, log, logical
     end associate

  end associate

end program acetint33d
