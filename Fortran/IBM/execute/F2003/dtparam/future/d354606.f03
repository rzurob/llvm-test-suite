! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/19/2009
!*
!*  DESCRIPTION                : test case that tracks the problems reported in
!                               defect 354606.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

    implicit none

    character(len=7) :: t="xlftest"
    character(:),pointer :: p1
    allocate(p1,source="xlftest")
    print *,p1(1:3)%len,len(p1(1:3))
    print *,"|",t(1:3),"|"
    print *,len(t(1:3)),t(1:3)%len
end