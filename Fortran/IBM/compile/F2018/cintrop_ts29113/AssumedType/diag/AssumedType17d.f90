!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : June 13, 2012
!*
!*  PRIMARY FUNCTIONS TESTED   : C-interop: Assumed Type objects
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : TYPE(*) object cannot appear as the
!*                               selector in an ASSOCIATE statement
!*
!**********************************************************************
!234567890123456789012345678901234567890123456789012345678901234567890
program AssumedType17d
implicit none

contains

   subroutine sub(a)
    type(*) :: a

    associate ( s => a )
     print*, s
    end associate
   end subroutine sub

end program AssumedType17d
