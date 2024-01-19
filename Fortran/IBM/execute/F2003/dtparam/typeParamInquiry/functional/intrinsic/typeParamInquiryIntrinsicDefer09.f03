!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : August 5 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : TYPE PARAMETER INQUIRY
!*
!*  DESCRIPTION
!*
!* 1. TEST SECTION 6.1.3
!* 2. TYPE PARAMETER INQUIRY FOR INTRINSIC TYPE
!* 3. DEFERRED TYPE PARAMETER INQUIRY
!* 4. ALLOCATABLE OBJECT
!* 5. USE MOVE_ALLOC
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m

    character(:),allocatable :: from2,to2
    contains
       subroutine move(from,to)
           character(:),allocatable,intent(inout) :: from
           character(:),allocatable,intent(out) :: to

           allocate(from,source="xlftest")
           call move_alloc(from,to)
       end subroutine

end module

program typeParamInquiryIntrinsicDefer09
    use m
    implicit none

    character(:),allocatable :: from1(:),to1(:)

    allocate(from1(2:4),source="xlf")
    call move_alloc(from1,to1)

    if(allocated(from1))                              error stop 10_4
    if(to1%len /= from1%len .or. to1%len /= 3  &
                     .or. len(to1) /= 3)              error stop 11_4
    if(to1%kind /= from1%kind .or. to1%kind /= 1  &
                     .or. kind(to1) /= 1)             error stop 12_4
    if(lbound(to1,1) /=2 .or. ubound(to1,1) /= 4)     error stop 13_4

    deallocate(to1)
    allocate(to1(len("abc")+len("def")),source="abc"//"def")
    call move_alloc(to1,from1)

    if(any(from1 /= "abcdef"))                        error stop 14_4
    if(from1%len /= len(from1) .or. from1%len /= 6)   error stop 15_4
    if(from1%kind /= kind(from1) .or. from1%kind /=1) error stop 16_4
    if(ubound(from1,1) /=6 .or. lbound(from1,1) /= 1) error stop 17_4

    call move(from2,to2)

    if(allocated(from2))                              error stop 18_4
    if(to2%len /= from2%len .or. to2%len /= 7  &
                     .or. len(to2) /= 7)              error stop 19_4
    if(to2%kind /= from2%kind .or. to2%kind /= 1  &
                     .or. kind(to2) /= 1)             error stop 20_4

end
