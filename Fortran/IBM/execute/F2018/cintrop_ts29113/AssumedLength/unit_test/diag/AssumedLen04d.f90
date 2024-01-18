! *********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : AssumedLen04d.f
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
!* DESCRIPTION                  : for assumed_length dummy, actual argument can be
!*                                assumed_rank only if the the dummy is assumed_rank 
!*                                 as well.
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

 interface
   subroutine sub_int(arg)
     character(2) :: arg(..)
   end subroutine
 end interface
 character(2) :: arr(2,4)
 call sub_int(arr)
End Program

subroutine sub_int(arg)
  interface
   subroutine sub_1(arg1) bind(c)
    character(*) :: arg1(2,4)
   end subroutine
  end interface
  character(2):: arg(..)
  call sub_1(arg)
end subroutine
