! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/14/2005
!*
!*  DESCRIPTION                : miscellaneous items (defect 300593)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program fmisc039
    read *, i1      !<-- illegal, should diagnose
    print *, i1     !<-- illegal, should diagnose

    contains

    subroutine i1(i)
        print *, i
    end subroutine

end