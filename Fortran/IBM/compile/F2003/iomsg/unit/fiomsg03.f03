! *********************************************************************
!*
!*  ===================================================================
!*
!*  DIAGNOSES TESTED           : IOMSG variable must be a character
!*                               variable.
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

        Program fiomsg03
          integer i
          open(11, status='scratch', iomsg=i)
        end program fiomsg03