! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : October 06, 2010
!* .or.GIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Diagnostic Test for array: mismatching TKR on
!*                               coindex assginment.
!*
!*  KEY.or.(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HIS.or.
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================

program coindex_assignment_002d
    implicit none
    integer, save    :: caf1(10)[*]
    integer, save    :: caf2[*]
    integer :: me, num, left, i
    logical :: l1

    me = THIS_IMAGE()
    num = NUM_IMAGES()

    if (me == 1) then
        left = num
    else
        left = me - 1
    end if

    me = THIS_IMAGE()
    caf1(:)[me] = [(i*me, i = 1,10)]
    caf2[me] = me
    SYNC ALL
    l1 = caf2[left]
    i = caf1(:)[me]

    print *, l1

end program
