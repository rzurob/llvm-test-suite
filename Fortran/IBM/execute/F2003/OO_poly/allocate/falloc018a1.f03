! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/10/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (module allocatable accessed via
!                               multiple files)
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

program falloc018a1
    call allocateB1_mOnly (-100, 'test1')

    call testAllocation

    call allocateB1_mB2_m

    call testAllocation

    call allocateB2_mOnly (-200, 'test2', -1, 1)

    call testAllocation

    call deallocateB1_mB2_m

    call testAllocation
end

!! allocate b1_m to be of child type with id = i and name = c
subroutine allocateB1_mOnly (i, c)
    use m
    integer(4), intent(in) :: i
    character(*), intent(in) :: c

    if (allocated(b1_m)) deallocate (b1_m)

    allocate (b1_m, source=child(i,c))
end subroutine


!! allocate b1_m to be of child type with id = i and name = c; bounds from lb to
!ub
subroutine allocateB2_mOnly (i, c, lb, ub)
    use m
    integer(4), intent(in) :: i, lb, ub
    character(*), intent(in) :: c

    if (allocated(b2_m))  deallocate (b2_m)

    allocate (b2_m(lb:ub), source=child(i,c))
end subroutine
