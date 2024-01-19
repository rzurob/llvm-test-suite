! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/29/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Use the deferred character allocatable variable
!                               in the intrinsic assignment and the expression
!                               is of zero-sized and zero-lenght strings.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program zeroSizeArray008
    character(:), allocatable :: c1(:,:), c2(:), string(:)

    c1 = reshape((/(repeat(' ', 20), i=1,100)/), (/10,10/))

    c2 = (/(trim(c1(3,i)), i=1,8)/)

    string = c2(5:1)

    !! verify c1, c2 and string
    if ((.not. allocated(c1)) .or. (.not. allocated(c2)) .or. &
        (.not. allocated(string))) error stop 1_4

    if (any(shape(c1) /= (/10,10/))) error stop 2_4

    if (size(c2) /= 8) error stop 3_4

    if (size(string) /= 0) error stop 4_4

    if (any(c1 /= '')) error stop 5_4

    if (len(c2) /= 0) error stop 6_4

    if (len(string) /= 0) error stop 7_4
end
