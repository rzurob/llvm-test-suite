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
! %GROUP: fxass034.f
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
!*  TEST CASE NAME             : fxass034.f
!*  TEST CASE TITLE            : ASSOCIATE
!*
!*  PROGRAMMER                 : Sarah Kouchaki-Ramezan
!*  DATE                       : Feb 5,2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
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
!*                                     data types. using function for
!*                                     using associate in inside
!*                                     the function.
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

      program fxass34a
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
      c = int_fun(a,b)
         if(c .eq. 0)then 
           error stop 10
         endif

      c1 = (a1 + b1)*10 + 10   
      c1 = int1_fun(a1,b1)
         if(c1 .eq. 0)then 
           error stop 11
         endif

      c2 = (a2 + b2)*10 + 10   
      c2 = int2_fun(a2,b2)
         if(c2 .eq. 0)then 
           error stop 12
         endif

      c4 = (a4 + b4)*10 + 10   
      c4 = int4_fun(a4,b4)
         if(c4 .eq. 0)then 
           error stop 13
         endif

      c8 = (a8 + b8)*10 + 10   
      c8 = int8_fun(a8,b8)
         if(c8 .eq. 0)then 
           error stop 14
         endif

      cb = (ab1 + ab2)*10 + 10   
      cb = byte_fun(ab1,ab2)
         if(cb .eq. 0)then 
           error stop 15
         endif

      contains

!-----------   ASSOCIATE with INTEGER expressions ----------------

      integer function int_fun(a,b)
            integer a,b
            integer c
            integer count
            c = (a + b)*10 + 10   
            do count = 1, 10

            associate ( arg => (a + b)*10 + 10 )
              if(arg .ne. c)then 
              int_fun = 0
              endif
            end associate
            end do

              int_fun = c
      end function int_fun

!-----------   ASSOCIATE with INTEGER*1 expressions ----------------

      integer*1 function int1_fun(a1,b1)
            integer*1 a1,b1
            integer*1 c1
            integer count
            c1 = (a1 + b1)*10 + 10   
            do count = 1, 10

            associate ( arg1 => (a1 + b1)*10 + 10 )
              if(arg1 .ne. c1)then 
              int1_fun = 0
              endif
            end associate

            end do
              int1_fun = c1
      end function int1_fun


!-----------   ASSOCIATE with INTEGER*2 expressions ----------------

      integer*2 function int2_fun(a2,b2)
            integer*2 a2,b2
            integer*2 c2
            integer count
            c2 = (a2 + b2)*10 + 10   
            do count = 1, 10

            associate ( arg2 => (a2 + b2)*10 + 10 )
              if(arg2 .ne. c2)then 
              int2_fun = 0
              
              endif
            end associate

            end do
              int2_fun = c2
            
      end function int2_fun

!-----------   ASSOCIATE with INTEGER*4 expressions ----------------

      integer*4 function int4_fun(a4,b4)
            integer*4 a4,b4
            integer*4 c4
            integer count
            c4 = (a4 + b4)*10 + 10   
            do count = 1, 10

            associate ( arg4 => (a4 + b4)*10 + 10 )
              if(arg4 .ne. c4)then 
              int4_fun = 0
              
              endif
            end associate

            end do
              int4_fun = c4
            
      end function int4_fun

!-----------   ASSOCIATE with INTEGER*8 expressions ----------------

      integer*8 function int8_fun(a8,b8)
            integer*8 a8,b8
            integer*8 c8
            integer count
            c8 = (a8 + b8)*10 + 10
            do count = 1, 10
  
            associate ( arg8 => (a8 + b8)*10 + 10 )
              if(arg8 .ne. c8)then 
              int8_fun = 0
              endif
            end associate

            end do
            
              int8_fun = c8
      end function int8_fun

!-----------   ASSOCIATE with BYTE expressions ----------------

      byte function byte_fun(ab1,ab2)
            byte ab1,ab2
            byte cb
            integer count
            cb = (ab1 + ab2)*10 + 10
            do count = 1, 10

            associate ( arg_1 => (ab1 + ab2)*10 + 10 )
              if(arg_1 .ne. cb)then 
              byte_fun = 0
              endif
            end associate

            end do
            byte_fun = cb
            
      end function byte_fun

      end

