! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : September 09, 2010
!* .or.GIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Test masked array assignment - where
!*
!*  KEY.or.(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HIS.or.
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================

program coindex_assignment_003f
    integer    :: caf3(10)[*]
    SAVE caf3
    integer    :: caf4(10)[*]
    SAVE caf4
    integer me, num, left
    integer    :: array1(10)
    integer    :: array2(10)
    logical    :: log_true(10)

    me = THIS_IMAGE()
    num = NUM_IMAGES()

    if (me == 1) then
        left = num
    else
        left = me - 1
    end if

    array1 = [(i*me, i=1,10)]
    log_true = [(.true., i=1,10)]
    where (log_true) caf3(:)[me] = array1(:)
    SYNC ALL
    where (log_true) caf4(:)[me] = caf3(:)[left]
    where (log_true) array2(:) = caf4(:)[me]

    do i=1, 10
        if (i*left .ne. array2(i)) then
            error stop 1
        end if
    end do

end program
