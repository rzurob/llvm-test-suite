!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrD327298.f
!*
!*  DATE                       : Aug 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION
!*
!*  - This is diagnostic testcase
!*  Use the testcase to record defect 327298
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main
    integer, target  :: val(10)
    integer, pointer :: p(:)
    data   (val(i), i=1,max(1,10)) /10,9,8,7,6,5,4,3,2,1/

    p(1:10) => val
end

