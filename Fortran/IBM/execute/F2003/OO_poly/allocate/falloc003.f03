! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/20/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (array constructor as source-expr)
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

program falloc003
    class (*), pointer :: x(:,:)
    integer*4, pointer :: x1(:,:)

    allocate (x1(2:3,-1:0), source=reshape ((/1,2,3,4/), (/2,2/)))

    !! verify the results
    if (any(x1(2,:) /= (/1,3/)) .or. any (x1(3,:) /= (/2,4/))) error stop 1_4
    if (any(x1(:,-1) /= (/1,2/)) .or. any(x1(:,0) /= (/3,4/))) error stop 2_4

    !! try the unlimited poly-pointer array
    allocate (x(2:3,-1:0), source=reshape ((/1,2,3,4/), (/2,2/)))

    if (any(shape (x) /= (/2,2/))) error stop 3_4

    if (any (lbound(x) /= (/2, -1/))) error stop 4_4

    if (any(ubound(x) /= (/3,0/))) error stop 5_4
    deallocate (x, x1)
end
