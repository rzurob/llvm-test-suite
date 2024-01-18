! *********************************************************************
!*
!*  ===================================================================
!*
!*  DIAGNOSES TESTED           : IOMSG variable cannot be an array.
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

        Program fiomsg01
          character(10) string(10)
          open(11, status='scratch', iomsg=string)
        end program fiomsg01
