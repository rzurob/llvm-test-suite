! *********************************************************************
!*
!*  ===================================================================
!*
!*  DIAGNOSES TESTED           : IOMSG variable cannot be protected
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

        Module mod1
          character(191), protected :: iomsg_var
        end module

        Program fiomsg11
          use mod1
          open(11, status='scratch', iomsg=iomsg_var)
        end program fiomsg11

