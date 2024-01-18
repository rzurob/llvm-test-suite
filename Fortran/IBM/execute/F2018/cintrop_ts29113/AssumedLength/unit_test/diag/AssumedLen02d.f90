! *********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : AssumedLen02d.f
!*
!* PROGRAMMER                   : Maryam Moghadas
!* DATE                         : June  27, 2014
!* ORIGIN                       : AIX/Linux Complier Development
!*                              : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED     : C Interop: Assumed Length object
!* SECONDARY FUNTIONS TESTED    :
!*
!* DRIVER STANZA                :
!* REQUIRED COMPILER OPTIONS    :  -qdebug=BCASSUMEDLEN (should be removed later)
!*
!* DESCRIPTION                  : verify that only non-alloc  non-pointer
!*                                assumed length object are interoperable with C
!*
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

interface
  subroutine sub_1(arg1) bind(c)
   character(*) , allocatable :: arg1(:)
  end subroutine
end interface 

end program
