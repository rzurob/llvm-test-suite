! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : September 08, 2010
!* .or.GIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Test intrinsic type for scalar: INTEGER, REAL,
!                                DOUBLE, COMPLEX, CHARACTER, LOGICAL
!*
!*  KEY.or.(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HIS.or.
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================

program coindex_assignment_002f
    integer    :: caf1[*]
    SAVE caf1
    real(4)    :: caf2[*]
    SAVE caf2
    real(8)    :: caf3[*]
    SAVE caf3
    complex    :: caf4[*]
    SAVE caf4
    character  :: caf51[*]
    SAVE caf51
    character  :: caf52[*]
    SAVE caf52
    logical    :: caf6[*]
    SAVE caf6
    integer me, num, left
    integer    :: int1
    integer    :: int2
    real(4)    :: real1
    real(4)    :: real2
    real(8) d1, d2
    complex    :: cpx2
    character c1
    logical ::logical1
    logical ::logical2
    interface
        logical function precision_r4 (r1, r2)
            real(4), intent(in) :: r1, r2
        end function

        logical function precision_r8 (r1, r2)
            real(8), intent(in) :: r1, r2
        end function

        logical function precision_x8 (r1, r2)
            complex, intent(in) :: r1, r2
        end function
    end interface

    me = THIS_IMAGE()
    num = NUM_IMAGES()

    if (me == 1) then
        left = num
    else
        left = me - 1
    end if
    int1 = me
    caf1 = int1
    real1 = me * 1.2
    caf2 = real1
    d1 = me*100.d0
    caf3 = d1
    caf4 = (1.0,1.0*me)

    caf6 = (mod(me, 2) .ne. 0)
    SYNC ALL

    logical1 = caf6
    logical2 = caf6[left]
    d2 = caf3[left]
    int2 = caf1[left]
    real2 = caf2[left]
    cpx2 = caf4[left]

    if (caf1[me] .ne. int1) then
        error stop 1
    end if
    if (left .ne. int2) then
        error stop 2
    end if

    if ( .not. precision_r4(caf2[me], real1)) then
        error stop 3
    end if
    if ( .not. precision_r4(left*1.2, real2)) then
        error stop 4
    end if

    if (.not. precision_r8(caf3[me], d1)) then
        error stop 5
    end if
    if (.not. precision_r8(100.d0*left,d2)) then
        error stop 6
    end if

    if ( .not. precision_x8( caf4, (1.0,1.0*me) ) )then
        error stop 7
    end if

    if ( .not. precision_x8( cpx2, (1.0,1.0*left) ) ) then
        error stop 8
    end if

    if ( logical1 .neqv. (mod(me, 2) .ne. 0) ) then
        error stop 11
    end if
    if (logical2 .neqv. (mod(left, 2) .ne. 0)) then
        error stop 12
    end if

end program
