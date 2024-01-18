!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: fxass032.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxass032.f
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
!*  DESCRIPTION                : Test: ASSOCIATE with expressions
!*                                     with integer, integer*1, integer*2
!*                                     integer*4, integer*8 and byte
!*                                     data types. calling subrotine
!*                                     and using associate in inside
!*                                     the subroutine.
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

      program fxass32a
      implicit none

      integer a / 1 /
      integer b / 4 /
      integer c

      integer*1 a1 / 2 /
      integer*1 b1 / 8 /
      integer*1 c1

      integer*2 a2 / 5 /
      integer*2 b2 / 4 /
      integer*2 c2

      integer*4 a4 / 9 /
      integer*4 b4 / 2 /
      integer*4 c4

      integer*8 a8 / 9 /
      integer*8 b8 / 2 /
      integer*8 c8

      byte ab1 / 1 /
      byte ab2 / 4 /
      byte cb


      c = (a + b)*10 + 10
      call int_sub(a,b,c)
         if(c .eq. 0)then
           error stop 10
         endif

      c1 = (a1 + b1)*10 + 10
      call int1_sub(a1,b1,c1)
         if(c1 .eq. 0)then
           error stop 11
         endif

      c2 = (a2 + b2)*10 + 10
      call int2_sub(a2,b2,c2)
         if(c2 .eq. 0)then
           error stop 12
         endif

      c4 = (a4 + b4)*10 + 10
      call int4_sub(a4,b4,c4)
         if(c4 .eq. 0)then
           error stop 13
         endif

      c8 = (a8 + b8)*10 + 10
      call int8_sub(a8,b8,c8)
         if(c8 .eq. 0)then
           error stop 14
         endif

      cb = (ab1 + ab2)*10 + 10
      call byte_sub(ab1,ab2,cb)
         if(cb .eq. 0)then
           error stop 15
         endif

      contains

!-----------   ASSOCIATE with INTEGER expressions ----------------

      subroutine int_sub(a,b,c)
            integer a,b
            integer, intent(out) :: c
            integer count
            c = (a + b)*10 + 10
            do count = 1, 10

            associate ( arg => a )
              arg = arg + (a + b)*10
              if(arg .ne. a)then
              c = 0
              return
              endif
            end associate

            end do

            return
      end subroutine int_sub

!-----------   ASSOCIATE with INTEGER*1 expressions ----------------

      subroutine int1_sub(a1,b1,c1)
            integer*1 a1,b1
            integer*1, intent(out) :: c1
            integer count
            do count = 1, 10

            associate ( arg1 => a1 )
              arg1 = arg1 + (a1 + b1)*10_1
              if(arg1 .ne. a1)then
              c1 = 0_1
              return
              endif
            end associate

            end do
            return
      end subroutine int1_sub


!-----------   ASSOCIATE with INTEGER*2 expressions ----------------

      subroutine int2_sub(a2,b2,c2)
            integer*2 a2,b2
            integer*2, intent(out) :: c2
            integer count
            do count = 1, 10

            associate ( arg2 => a2 )
              arg2 = arg2 + (a2 + b2)*10_2
              if(arg2 .ne. a2)then
              c2 = 0_2
              return
              endif
            end associate

            end do
            return
      end subroutine int2_sub

!-----------   ASSOCIATE with INTEGER*4 expressions ----------------

      subroutine int4_sub(a4,b4,c4)
            integer*4 a4,b4
            integer*4, intent(out) :: c4
            integer count
            do count = 1, 10

            associate ( arg4 => a4 )
              arg4 = arg4 + (a4 + b4)*10
              if(arg4 .ne. a4)then
              c4 = 0
              return
              endif
            end associate

            end do
            return
      end subroutine int4_sub

!-----------   ASSOCIATE with INTEGER*8 expressions ----------------

      subroutine int8_sub(a8,b8,c8)
            integer*8 a8,b8
            integer*8, intent(out) :: c8
            integer count
            do count = 1, 10

            associate ( arg8 => a8 )
              arg8 = arg8 + (a8 + b8)*10_8
              if(arg8 .ne. a8)then
              c8 = 0_8
              return
              endif
            end associate

            end do
            return
      end subroutine int8_sub

!-----------   ASSOCIATE with BYTE expressions ----------------

      subroutine byte_sub(ab1,ab2,cb)
            byte ab1,ab2
            byte, intent(out) :: cb
            integer count
            do count = 1, 10

            associate ( arg_1 => ab1 )
              arg_1 = arg_1 + (ab1 + ab2)*10_1
              if(arg_1 .ne. ab1)then
              cb = 0_1
              return
              endif
            end associate

            end do
            return
      end subroutine byte_sub

      end

