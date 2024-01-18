! ******************************************************************************
!* =============================================================================
!*
!* DATE                         : June  27, 2014
!* ORIGIN                       : AIX/Linux Complier Development
!*
!* PRIMARY FUNCTIONS TESTED     : C Interop: Assumed Length object
!* SECONDARY FUNTIONS TESTED    :
!*
!* REQUIRED COMPILER OPTIONS    :  -qdebug=BCASSUMEDLEN (should be removed later)
!*
!* DESCRIPTION                  : verify that our compiler DOES NOT falg Assumed Length
!*                                object in bind(c) routine with -qlanglvl=ts
!*
!* =============================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* =============================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890123456789

interface
  subroutine sub_1(arg1) bind(c)
   character(*) :: arg1(5)
  end subroutine
end interface

character(2) :: arr(10)

call sub_1(arr)

end program
