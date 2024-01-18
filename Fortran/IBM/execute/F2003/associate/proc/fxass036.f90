!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb 5,2004
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
!*                                     data types. calling recusive subroutine
!*                                     and using associate in inside
!*                                     the subrotine.
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

      program fxass36a
      implicit none

      interface
      recursive subroutine int_sub()
      end subroutine

      recursive subroutine int1_sub()
      end subroutine

      recursive subroutine int2_sub()
      end subroutine

      recursive subroutine int4_sub()
      end subroutine
      recursive subroutine int8_sub()
      end subroutine

      recursive subroutine byte_sub()
      end subroutine

      end interface

      call int_sub()

      call int1_sub()

      call int2_sub()

      call int4_sub()

      call int8_sub()

      call byte_sub()

   end
!-----------   ASSOCIATE with INTEGER expressions ----------------

      recursive subroutine int_sub()
            integer ,save :: cont
            data cont /0/

              cont = cont + 1
            if ( cont .lt. 3 ) then
              associate ( arg => cont )
              if(arg .ne. cont ) error stop 10
              end associate
              call int_sub()
            end if

            return

      end subroutine int_sub

!-----------   ASSOCIATE with INTEGER*1 expressions ----------------

      recursive subroutine int1_sub()
            integer*1 ,save :: cont
            data cont /0/

            cont = cont + 1

            if ( cont .lt. 3 ) then
              associate ( arg => cont )
              if( arg .ne. cont ) error stop 11
              end associate
              call int1_sub()
            end if

            return
      end subroutine int1_sub


!-----------   ASSOCIATE with INTEGER*2 expressions ----------------

      recursive subroutine int2_sub()
            integer*2 ,save :: cont
            data cont /0/

              cont = cont + 1
            if ( cont .lt. 3 ) then
              associate ( arg => cont )
              if(arg .ne. cont ) error stop 12
              end associate
              call int2_sub()
            end if

            return
      end subroutine int2_sub

!-----------   ASSOCIATE with INTEGER*4 expressions ----------------

      recursive subroutine int4_sub()
            integer ,save :: cont
            data cont /0/

              cont = cont + 1
            if ( cont .lt. 3 ) then
              associate ( arg => cont )
              if(arg .ne. cont ) error stop 13
              end associate
              call int4_sub()
            end if

            return
      end subroutine int4_sub

!-----------   ASSOCIATE with INTEGER*8 expressions ----------------

      recursive subroutine int8_sub()
            integer*8 ,save :: cont
            data cont /0/

              cont = cont + 1
            if ( cont .lt. 3 ) then
              associate ( arg => cont )
              if(arg .ne. cont ) error stop 14
              end associate
              call int8_sub()
            end if

            return
      end subroutine int8_sub

!-----------   ASSOCIATE with BYTE expressions ----------------

      recursive subroutine byte_sub()
            byte ,save :: cont
            data cont /0/

              cont = cont + 1
            if ( cont .lt. 3 ) then
              associate ( arg => cont )
              if(arg .ne. cont ) error stop 15
              end associate
              call byte_sub()
            end if

            return
      end subroutine byte_sub
