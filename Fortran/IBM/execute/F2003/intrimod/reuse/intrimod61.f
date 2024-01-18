!***********************************************************************
!**********************************************************************
@process intsize(8)
      PROGRAM intrimod61
C**********************************************************************
C*  ===================================================================
C*
C*                             : Utility Procedure Interfaces
C*
C*  DATE                       : January 13, 2004
C*
C*  PRIMARY FUNCTIONS TESTED   : IRTC
C*  SECONDARY FUNCTIONS TESTED : None
C*
C*  DESCRIPTION                : Procedure IRTC,correct calls
C*  KEYWORD(S)                 :
C*  TARGET(S)                  :
C*  NUMBER OF TESTS            : 5
C*  STATUS                     :
C*
C*  STRUCTURE                  : Main program
C*  EXECUTABLE                 : Yes
C*
C*  INPUTS                     : None
C*  OUTPUTS                    : None
C*
C*  SETUP REQUIREMENTS         : N/A
C*  DEPENDENCIES               :
C*  REQUIRED COMPILER OPTIONS  : None
C*
C*  NORMAL COMPLETION          : Return code = 0
C*  ABNORMAL COMPLETION        : Return code ^= 0
C*
C*  RUN TIME ESTIMATE          : <60 SECS
C*
C*  CONDITIONS TESTED          : Listed below.
C*
C/  COND.   DESCRIPTION
C/  -----   ------------------------------------------------------------
C/      1   Type of the function - INTEGER(4)
C/      2   Type of the function - INTEGER(2)
C/      3   @PROCESS INTSIZE(8)
C/      4   Type of the function - REAL(8)
C/      5   Type of the function - COMPLEX(8)
C/      6   USE XLFUTILITY, ONLY: IRTC
C/          and user's function DATE(m), which is in conflict with
C/          the utility DATE().
C/
C* ===================================================================
C*
C*  REVISION HISTORY
C*
C*  MM/DD/YY:  Init:  Comments:
C*  07/22/94   EB     -Initial Version (utility_proc/fxut031a.f)
C*  01/13/04   BC     -Modifyed and reused for INTRINSIC/NON_INTRINSIC module
C*                     nature.
C*
C* ===================================================================
C234567890123456789012345678901234567890123456789012345678901234567890

        use, intrinsic ::  xlfutility, only: irtc

C    DEFINES

       integer(4) A,m,d,date
       integer(2) A1
       integer A2
       real(8) A3
       complex(8) A4

C    CALLS OF IRTC

       A = irtc()
       A1 = irtc()
       A2 = irtc()
       A3 = irtc()
       A4 = irtc()

       m = 2
       d = date(m)

       if ( d .ne. 7 ) error stop 17

        end

       integer function date(m)
          integer m
          date = m+5
       end function date



