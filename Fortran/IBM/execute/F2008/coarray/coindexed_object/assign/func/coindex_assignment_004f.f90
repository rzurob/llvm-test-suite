! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Andy Chen
!*  DATE                       : September 09, 2010
!* .or.GIN                     :
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              :
!*
!*  DESCRIPTION                : Test FORALL assignment
!*
!*  KEY.or.(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HIS.or.
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================

program coindex_assignment_004f
    integer    :: caf1(10)[*]
    SAVE caf1
    integer    :: caf2(10)[*]
    SAVE caf2
    integer me, num, left
    integer    :: array1(10)
    integer    :: array2(10)

    me = THIS_IMAGE()
    num = NUM_IMAGES()

    if (me == 1) then
        left = num
    else
        left = me - 1
    end if

    array1 = [(i*me, i = 1,10)]
    forall(i=1:10)
        caf1(i) = array1(i)
    end forall
    SYNC ALL
    forall(i=1:10)
        caf2(i) = caf1(i)[left]
    end forall
    SYNC ALL
    forall(i=1:10)
        array2(i) = caf2(i)[me]
    end forall


    do i=1, 10
        if (caf1(i)[me] .ne. array1(i)) then
            error stop 1
        end if
        if (i*left .ne. array2(i)) then
            error stop 2
        end if
    end do

end program
