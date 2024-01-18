!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: fxass022.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxass022.f
!*
!*  DATE                       : Feb 5,2004
!*
!*  PRIMARY FUNCTIONS TESTED   : ASSOCIATE on INTRINSIC Data Types
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  KEYWORD(S)                 : ASSOCIATE,integer,byte
!*  TARGET(S)                  :
!*  NUMBER OF TESTS            : 1
!*  STATUS                     : done
!*
!*  STRUCTURE                  : Main program
!*  EXECUTABLE                 : Yes
!*
!*  INPUTS                     : None

!*  OUTPUTS                    : None
!*
!*  SETUP REQUIREMENTS         : N/A
!*  DEPENDENCIES               : External routine ZZRC
!*  REQUIRED COMPILER OPTIONS  : None
!*
!*  NORMAL COMPLETION          : Return code = 0
!*  ABNORMAL COMPLETION        : Return code ^= 0
!*
!*  RUN TIME ESTIMATE          : <60 SECS
!*
!*  CONDITIONS TESTED          : Listed below.
!*
!*  DESCRIPTION                : Test: ASSOCIATE with expressions and
!*                                     with integer constant array
!*                                     and do loop. test with intrinsic
!*                                     function real.
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*                    -Initial Version
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

      program fxass22a
      implicit none

      integer i
      integer,PARAMETER :: b(10) = (/(i,i=1,10)/)
      integer,PARAMETER :: d(10) = (/ 1,3,1,4,5,2,7,8,1,10 /)
      real c(50)

      do i = 1,10
       if ( b(i) == d(i) ) then
            associate ( arg => b(i) )
            if(arg .ne. b(i) )then
            error stop 2
            endif
            end associate

      elseif ( b(i) < d(i) ) then
             c(i) = real(i)
             associate ( arg1 => real(i) )
             if(arg1 .ne. c(i))then
             error stop 3
             end if
             end associate

      elseif ( b(i) > d(i) ) then
             associate ( arg2 => 1 )
             if(arg2 .ne. 1)then
             error stop 4
             end if
             end associate
      end if
      end do
      end

