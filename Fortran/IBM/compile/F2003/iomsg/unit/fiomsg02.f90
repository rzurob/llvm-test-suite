! *********************************************************************
!*
!*  ===================================================================
!*
!*  DIAGNOSES TESTED           : IOMSG variable cannot be intent(in)
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

        Program fiomsg02
          character(191) :: iomsg_var
          iomsg_var = 'intent in variable'
          call sub(iomsg_var)
        end program fiomsg02

        Subroutine sub(iomsg_var)
           intent(in) iomsg_var
           character(*) iomsg_var
           open(11, status='scratch', iomsg=iomsg_var)
        end subroutine sub
