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
! %GROUP: fxass099.f
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
!*  TEST CASE NAME             : fxass099.f
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

      program fxass099

      implicit none
          
       integer :: a(20), c(20), i, j
       integer :: arr(1:3) 
       integer, dimension(3,2) :: arr32

       a(1:20:2) = (/ (i,i=1,10,1) /)
       a(2:20:2) = (/ (i,i=10,100,10) /)
       c = a

       arr = (/ 4.0,5.0,6.0 /)       
       arr32 =  reshape( (/ 1,2,3,4,5,6 /), (/ 3,2 /) )
 
      associate ( arg1 => arr(2) )
           if(arg1 .ne. arr(2))then
           error stop 4
           endif
      end associate

      associate ( arg3 => arr )
           arg3(2) = 100
      end associate
           if(arr(2) .ne. 100)then
           error stop 5
           endif

      associate ( arg4 => (/ 2,3,4 /) )
           if(arg4(1) .ne. 2) error stop 6
           if(arg4(2) .ne. 3) error stop 7
           if(arg4(3) .ne. 4) error stop 8
      end associate
      
      associate ( arg5 => a(1:20) )
           do i = 1,20
           if(arg5(i) .ne. c(i)) error stop 9
           end do
           arg5 = 0
      end associate
           do i = 1,20
           if(a(i) .ne. 0) error stop 10
           end do

      associate ( arg6 => reshape((/ 1,2,3,4,5,6 /), (/ 3,2 /)) )
        do i = 1,3
           do j = 1,2
            if(arg6(i,j) .ne. arr32(i,j))then
            error stop 11
            endif
           enddo
        enddo
      end associate

      end
