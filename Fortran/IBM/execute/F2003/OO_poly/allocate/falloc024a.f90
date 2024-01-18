! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/23/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (stat-var set to 3)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program falloc024a
    class (*), pointer :: x1(:)

    class (*), allocatable :: x2(:)

    integer size1, error(1000)

    size1 = 2**25

    allocate (complex(8) :: x2(100))

    error  = 0

    do i = 1, 1000
        allocate (complex(4) :: x1(size1), x2(2), stat= error(i))
    end do

    if (maxval(error) /= 3) error stop 1_4

    if (minval(error) < 2) error stop 2_4
end
