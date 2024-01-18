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
!*  DESCRIPTION                : Diagnostic Test for array: Intrinsic coarray as
!*                               array coarray
!*
!*  KEY.or.(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HIS.or.
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================

program coindex_assignment_003d
    implicit none
    integer, save    :: caf1[*]
    integer :: me, num, left, i
    integer :: array1(10)
    
    me = THIS_IMAGE()
    num = NUM_IMAGES()

    if (me == 1) then
        left = num
    else
        left = me - 1
    end if

    me = THIS_IMAGE()
    caf1[me] = me
    SYNC ALL
    array1(:) = caf1(:)[left]

    print *, caf1[me]

end program
