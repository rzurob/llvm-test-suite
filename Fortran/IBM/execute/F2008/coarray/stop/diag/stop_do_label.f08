!*
!*  ===================================================================
!*
!*  TYPE                       : Diagnostic test
!*  FEATURE                    : #351605.31 CAF - STOP statement
!*
!*  DATE                       : 19 Oct 2010
!*
!*  REQUIRED COMPILER OPTIONS  :
!*  DEPENDENCIES               :
!*
!*  DESCRIPTION                : Test that if a statement label is specified
!*                               in a DO statement, STOP must not be
!*                               used to terminate that DO construct.
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

      program stop_do_label
      volatile res

      do 6789 i=1,1000
         res=1.0 + i*2
         print *, "i=", i
6789  stop
      print *, res
      end
