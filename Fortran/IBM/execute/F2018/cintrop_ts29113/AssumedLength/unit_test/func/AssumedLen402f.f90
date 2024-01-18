! ******************************************************************************
!* =============================================================================
!*
!* DATE                         : June  25, 2014
!* ORIGIN                       : AIX Complier Development
!*
!* PRIMARY FUNCTIONS TESTED     : C Interop: Assumed length object
!* SECONDARY FUNTIONS TESTED    :
!*
!* REQUIRED COMPILER OPTIONS    : -qdebug = BCASSUMEDLEN  (temporarily)
!*
!* DESCRIPTION                  : Calling a C BIND(C) procedure from Fortran
!*
!*                                - type character(*)
!*                                - Dummy argument is an assumed_rank array
!*                                    with contigious attribute
!*                                - Call to BIND(C) procedure from different scopes:
!*                                      main program, internal/external procedure
!* =============================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* =============================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890123456789
Program main
 interface
   subroutine sub_1(arg1) bind(c)
     character(*) , contiguous :: arg1(..)
   end subroutine
 end interface

 character(2), target :: c1(4,2)
 character(2), pointer :: c2(:,:)

 k = 0
 do i=1,2
   do j=1,4
     k = k + 1
     c1(j,i) = CHAR(k+64) // CHAR(k+64)
   end do
 end do

 c2 => c1(1:4:2,1:2)

 call sub_1(c2)

end program



