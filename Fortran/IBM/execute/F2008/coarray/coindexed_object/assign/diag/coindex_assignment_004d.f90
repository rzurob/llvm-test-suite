! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Andy Chen
!*  DATE                       : October 06, 2010
!* .or.GIN                     :
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              :
!*
!*  DESCRIPTION                : Diagnostic Test for array: conindexing a
!*                               non-coarry
!*
!*  KEY.or.(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HIS.or.
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================

program coindex_assignment_004d
    implicit none
    integer, save    :: caf1(10)[*]
    integer :: me, num, left, i
    integer :: array1(10)
    
    me = THIS_IMAGE()
    num = NUM_IMAGES()

    if (me == 1) then
        left = num
    else
        left = me - 1
    end if
    array1 = [(i,i=1,10)]

    SYNC ALL
    caf1(:)[me] = array1(:)[left]
    SYNC ALL

    print *, caf1(1)[me]

end program
