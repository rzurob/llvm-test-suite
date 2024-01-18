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
! %GROUP: fxass040.f
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
!*  TEST CASE NAME             : fxass040.f
!*  TEST CASE TITLE            : ASSOCIATE
!*
!*  PROGRAMMER                 : Sarah Kouchaki-Ramezan
!*  DATE                       : Feb 5,2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : ASSOCIATE on INTRINSIC Data Types
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  KEYWORD(S)                 : ASSOCIATE,integer,byte, RECURSIVE
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
!*                                     data types. using recursive function
!*                                     for using associate in inside
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

      program fxass40a
      implicit none
 
      interface 
      recursive function int_fun() result(c)
            integer :: c
      end function

      recursive function int1_fun() result(c1)
            integer*1 :: c1
      end function

      recursive function int2_fun() result(c2)
            integer*2 :: c2
      end function

      recursive function int4_fun() result(c4)
            integer*4 :: c4
      end function
      recursive function int8_fun() result(c8)
            integer*8 :: c8
      end function

      recursive function byte_fun() result(cb)
            byte :: cb
      end function

      end interface

      integer c / 10 /

      integer*1 c1 /10/

      integer*2 c2 / 10 /

      integer*4 c4 / 10 /

      integer*8 c8 / 10 /
      
      byte cb / 10 /

      
      c = int_fun()

      c1 = int1_fun()

      c2 = int2_fun()

      c4 = int4_fun()

      c8 = int8_fun()

      cb = byte_fun()

   end
!-----------   ASSOCIATE with INTEGER expressions ----------------

      recursive function int_fun() result (c)
            integer :: c
            integer ,save :: cont
            data cont /0/

              cont = cont + 1
            if ( cont .lt. 3 ) then
              associate ( arg => cont )
              if(arg .ne. cont ) then
              error stop 10
              endif
              end associate
              c = int_fun()
            end if

            return
      end function int_fun

!-----------   ASSOCIATE with INTEGER*1 expressions ----------------

      recursive function int1_fun() result(c1)
            integer*1 :: c1
            integer*1 ,save :: cont
            data cont /0_1/

            cont = cont + 1_1

            if ( cont .lt. 3_1 ) then
              associate ( arg => cont )
              if(arg .ne. cont ) then
              error stop 11
              endif
              end associate
              c1 = int1_fun()
            end if

            return

      end function int1_fun


!-----------   ASSOCIATE with INTEGER*2 expressions ----------------

      recursive function int2_fun() result(c2)
            integer*2 :: c2
            integer*2 ,save :: cont
            data cont /0_2/

              cont = cont + 1_2
            if ( cont .lt. 3_2 ) then
              associate ( arg => cont )
              if(arg .ne. cont ) then
              error stop 12
              endif
              end associate
              c2 = int2_fun()
            end if

            return

      end function int2_fun

!-----------   ASSOCIATE with INTEGER*4 expressions ----------------

      recursive function int4_fun() result(c4)
            integer*4 :: c4
            integer ,save :: cont
            data cont /0/

              cont = cont + 1
            if ( cont .lt. 3 ) then
              associate ( arg => cont )
              if(arg .ne. cont ) then
              error stop 13
              endif
              end associate
              c4 = int4_fun()
            end if

            return

      end function int4_fun

!-----------   ASSOCIATE with INTEGER*8 expressions ----------------

      recursive function int8_fun() result(c8)
            integer*8 :: c8
            integer*8 ,save :: cont
            data cont /0_8/

              cont = cont + 1_8
            if ( cont .lt. 3_8 ) then
              associate ( arg => cont )
              if(arg .ne. cont ) then
              error stop 14
              endif
              end associate
              c8 = int8_fun()
            end if

            return

      end function int8_fun

!-----------   ASSOCIATE with BYTE expressions ----------------

      recursive function byte_fun() result(cb)
            byte :: cb
            byte ,save :: cont
            data cont /0_1/

              cont = cont + 1_1
            if ( cont .lt. 3_1 ) then
              associate ( arg => cont )
              if(arg .ne. cont ) error stop 15
              end associate
              cb = byte_fun()
            end if

            return

      end function byte_fun
