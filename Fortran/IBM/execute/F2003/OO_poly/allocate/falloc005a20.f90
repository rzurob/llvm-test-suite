! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/16/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (level-4 expression used as the
!                               source-expr; use intrinsic types)
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

program falloc005a20
    real(8) :: r1 (10)
    integer(2) :: i1(5), i2 (10), j

    logical, allocatable :: l1(:), l2(:)

    r1 = (/(i*1.08, i=1,10)/)

    i1 = (/1_2, 2_2, 10_2, 4_2, 5_2/)

    i2 = (/(j*2_2, j=1_2,10_2)/)

    allocate(l1(size(r1)), source=r1>6.0d0)

    allocate (l2 (size(i1)), source=i2(::2) .eq. i1)

    if ((any (l1(:5))) .or. (.not. all (l1(6:)))) error stop 1_4

    if (.not. l2(3)) error stop 2_4
    if (any (l2(4:)) .or. l2(1) .or. l2(2)) error stop 3_4

    deallocate (l1, l2)
end
