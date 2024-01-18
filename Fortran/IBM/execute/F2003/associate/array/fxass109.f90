!**********************************************************************
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!**********************************************************************
!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90
! %GROUP: fxass109.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!**********************************************************************
!*  ===================================================================
!*  AIX XL FORTRAN/6000 TEST CASE                 IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxass109.f
!*  TEST CASE TITLE            : ASSOCIATE
!*
!*  PROGRAMMER                 : Sarah Kouchaki-Ramezan
!*  DATE                       : Feb 5,2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : ASSOCIATE with array
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  KEYWORD(S)                 : ASSOCIATE,integer
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
!*  DESCRIPTION                : Test: ASSOCIATE with subroutine and 
!*                                     array sections with do loop 
!*                                     and integer data types.
!*
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*                    -Initial Version
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

      program fxass109

      implicit none
          
       integer :: a(20), b(20), c(20), i
       integer i_arr1(3)

       a(1:20:2) = (/ (i,i=1,10,1) /)
       a(2:20:2) = (/ (i,i=10,100,10) /)

       b(1:20:2) = (/ (i,i=1,10,1) /)
       b(2:20:2) = (/ (i,i=10,100,10) /)

       c(1:20:2) = (/ (i,i=1,10,1) /)
       c(2:20:2) = (/ (i,i=10,100,10) /)

       i_arr1 = (/ 1,2,3 /)
       
       call sub1(i_arr1(:),1)

       call sub1(i_arr1(1:2),5)

      associate ( arg => (a(2) + a(3) - a(1))* a(4) )
           if(arg .ne. 220)then
           error stop 2
           endif

      end associate

      associate ( arg1 => (a(3) + a(4) - a(2)) * c(20) - (b(4) + b(5) + c(5)) )
           if(arg1 .ne. 1174)then
           error stop 3
           endif
      end associate

      contains

      subroutine sub1(arr,l)
      integer arr(:),l
      integer i,j,k
      j = LBOUND(arr,1)
      k = UBOUND(arr,1)
      do i = j,k
         associate ( arg2 => arr(i) )
           if(arg2 .ne. i)then
           call zzrc(l)
           endif
         end associate
      enddo
      end subroutine 

      end
